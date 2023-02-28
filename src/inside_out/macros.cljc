(ns inside-out.macros
  (:refer-clojure :exclude [meta])
  (:require [clojure.string :as str]
            [clojure.zip :as z]
            [inside-out.util :as util]
            [re-db.hooks :as hooks]
            [re-db.reactive :as r])
  #?(:cljs (:require-macros inside-out.macros)))

(defn replace-toplevel [pmap forms]
  (map (fn [x]
         (if (list? x)
           (list* (pmap (first x) (first x)) (rest x))
           (pmap x x))) forms))

(defmacro support-clj-protocols
  "Given a cljs deftype/defrecord, replace cljs-specific protocols/methods with clj variants
   when expansion target is clj.

  NOTE: only works for the subset of protocols/methods listed below."
  [form]
  (if (:ns &env) ;; target is cljs
    form
    (replace-toplevel '{ILookup clojure.lang.ILookup
                        -lookup valAt

                        ISeqable clojure.lang.Seqable
                        -seq seq

                        IDeref clojure.lang.IDeref
                        -deref deref

                        IReset clojure.lang.IAtom
                        -reset! reset
                        ISwap clojure.lang.IAtom
                        -swap! swap

                        IMeta clojure.lang.IMeta
                        -meta meta

                        IWithMeta clojure.lang.IObj
                        -with-meta withMeta

                        IFn clojure.lang.IFn
                        -invoke invoke} form)))

;; from https://stackoverflow.com/questions/39768093/how-to-implement-walk-postwalk-traversal-using-clojure-zip
(defn prewalk [f loc]
  (let [loc (z/replace loc (f (z/node loc) loc))]
    (if-some [loc (z/down loc)]
      (loop [loc loc]
        (let [loc (prewalk f loc)]
          (if-some [loc (z/right loc)]
            (recur loc)
            (z/up loc))))
      loc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility macros

(defmacro if-found [[sym lookup] then else]
  `(let [v# ~(concat lookup [::not-found])]
     (if (= ::not-found v#)
       ~else
       (let [~sym v#]
         ~then))))

(defmacro some-or [& forms]
  (loop [forms (reverse forms)
         out nil]
    (if (empty? forms)
      out
      (recur (rest forms)
             `(if-some [v# ~(first forms)]
                v#
                ~out)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functions

(defn remove-empty [m] (into {} (remove (comp nil? second)) m))

(defn code-zipper
  "Zipper for representing arbitrary code supporting traversal into maps, sets, and any sequential collection"
  [form]
  (z/zipper
   (fn branch? [x]
     (or (sequential? x)
         (map? x)
         (set? x)))
   seq
   (fn make-node [node children]
     (cond (map? node) (with-meta (into (empty node) children) (clojure.core/meta node))
           (map-entry? node) #?(:clj (clojure.lang.MapEntry. (first children) (second children))
                                :cljs (cljs.core/MapEntry. (first children) (second children) nil))
           (vector? node) (with-meta (vec children) (clojure.core/meta node))
           (set? node) (with-meta (set children) (clojure.core/meta node))
           :else children))
   form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; working with field variables

(defn field? [x]
  (util/field-sym? (cond-> x (list? x) first)))

;; unwraps fields wrapped in lists

(defn quoted? [x] (and (list? x) (= 'quote (first x))))
(defn quote-it [x] `(quote ~x))
(defn unquote-it [x] (cond-> x (quoted? x) second))

(defn field-sym [x]
  (cond-> x
          (and (field? x)
               (list? x)) first))

(defn quote-field-sym [form]
  (cond-> form
          (field? form)
          (-> field-sym quote-it)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handling ?field metadata

(defn inline-meta [field]
  (when (list? field)
    (apply hash-map (rest field))))

;; metadata inference: you can pass your own `:infer-meta` function as an option to `with-form*`
;; if these are not sufficient for your use-case

(defn infer-from-map-value [m loc]
  (when (some-> loc z/left z/up z/node map-entry?)
    {:attribute (z/node (z/left loc))
     :id (:db/id (-> loc z/up z/up z/node))}))

(defn infer-from-tx-value [m loc]
  (let [parent (some-> loc z/up z/node)]
    (when (and (vector? parent)
               (= :db/add (first parent))
               (= 3 (count (z/lefts loc))))
      {:attribute (nth parent 2)
       :id (nth parent 1)})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; core analysis/macro

(def default-infer [infer-from-map-value
                    infer-from-tx-value])

(defn ns-ify
  "For a keyword :a.b.c, turns first segment into namespace."
  [kw]
  (let [kw-name (name kw)
        parts (str/split kw-name #"\.")]
    (if (> (count parts) 1)
      (keyword (first parts) (str/join "." (rest parts)))
      (keyword kw-name))))

(defn lift-ns [m ns]
  (let [ns-name (name ns)]
    (reduce-kv (fn [out k v]
                 (cond-> out
                         (= ns-name (namespace k))
                         (assoc (ns-ify (name k)) v))) {} m)))

(comment
 (lift-ns {:form/a.b 1 :form/b.c 2 :other 3} :form))

(defn dissoc-ns [m ns]
  (let [ns-name (name ns)]
    (reduce (fn [out k]
              (cond-> out
                      (= ns-name (namespace k))
                      (dissoc k))) m (keys m))))

(defn analyze-form
  ([form] (analyze-form form {}))
  ([form options]
   (let [analyze-many (fn [{:as field :keys [many]}]
                        (cond-> field
                                many
                                (assoc :many
                                       (let [{:form/keys [fields compute]} (analyze-form many options)]
                                         {:many/fields (update-keys fields quote-it)
                                          :many/compute compute}))))
         [return fields] (let [!fields (atom {})]
                           [(->> (code-zipper form)
                                 (prewalk (fn [x loc]
                                            (when (field? x)
                                              (swap! !fields update (field-sym x) merge
                                                     ;; fields know their own symbol
                                                     {:sym (quote-field-sym x)}

                                                     ;; infer metadata from position within form
                                                     (->> (:infer-meta options default-infer)
                                                          (reduce (fn [meta f] (merge meta (remove-empty (f meta loc))))
                                                                  {})
                                                          (#(update-vals % quote-field-sym)))

                                                     ;; merge inline-meta last, overrides inferred metadata
                                                     (-> (inline-meta x) ;; (?field :attr v)
                                                         analyze-many))) ;; handle many-children
                                            (field-sym x)))
                                 z/node)
                            @!fields])
         ;; the core function where we bring bindings into scope and evaluate the form
         compute (let [bindings (gensym "bindings")]
                   `(fn [~bindings]
                      (let [~@(mapcat (fn [sym] [(unquote-it sym) `(get ~bindings '~sym)]) (keys fields))]
                        ~return)))
         ;; all other fields are assumed to be <key> {?field <value>}
         fields (reduce-kv
                 (fn [fields meta-k values]
                   (cond (map? values)
                         (reduce-kv (fn [fields sym value]
                                      (cond-> fields
                                              (fields sym)
                                              (assoc-in [sym meta-k] value))) fields values)
                         (vector? values)
                         (reduce (fn [fields sym]
                                   (cond-> fields
                                           (fields sym)
                                           (assoc-in [sym meta-k] true))) fields values)))
                 fields
                 (-> options
                     (dissoc :meta)
                     (dissoc-ns :form)))
         ;; handle form metadata
         form-meta (merge {:meta (cond-> (:meta options)
                                         (map? (:meta options))
                                         (update-keys quote-field-sym))}
                          (lift-ns options :form))]
     {:form/fields fields
      :form/return return
      :form/compute compute
      :form/meta form-meta})))

(comment

 ;; check that all the ?vars are found
 (->> (analyze-form '[[:db/add 1 :name/first ?first]
                      {:name/last ?last}
                      [:db/add 2 :pet/name ?pet-name]])
      :form/fields
      keys
      set
      (= '#{?first ?last ?pet-name}))

 ;; verify that all fields are found and replaced
 (->> '[[:db/add ?id :name/first ?owner-name]
        [:db/add 2 :pet/owner (?id :default 99)]
        [:db/add 2 :pet/name ?pet-name]
        #{?set-member}]
      analyze-form
      ((juxt :form/return (comp set keys :form/fields)))
      (= '[[[:db/add ?id :name/first ?owner-name]
            [:db/add 2 :pet/owner ?id]
            [:db/add 2 :pet/name ?pet-name]
            #{?set-member}]
           #{?id ?owner-name ?pet-name ?set-member}]))

 ;; metadata overrides
 (-> '[[:db/add 1 :name/first (?first :attribute :first-name)]]
     analyze-form
     (get-in [:form/fields '?first :attribute])
     (= :first-name)))

(defn with-form*
  "Implements with-form. Can pass :infer-meta function for additional metadata inference."
  [_form _env analyzer-options bindings body]
  (let [[root-sym expr] bindings
        options (apply hash-map (drop 2 bindings))
        _ (assert (every? keyword? (keys options)) (str "Invalid options (not a keyword: " (remove keyword? (keys options)) ")"))
        {:form/keys [fields compute meta]} (analyze-form expr (merge analyzer-options options))]
    `(hooks/with-let [~root-sym (~'inside-out.forms/root ~compute ~meta ~(vec (vals fields)))
                      ~@(->> fields
                             (mapcat (fn [[sym {:keys [many]}]]
                                       [(cond-> sym
                                                many
                                                (with-meta {:many/bindings (->> many :many/fields keys vec)}))
                                        `(get ~root-sym '~sym)])))]
                     ~@body)))

(defn form* [_form _env expr options]
  (let [{:form/keys [fields compute meta]} (analyze-form expr options)]
    `(~'inside-out.forms/root ~compute ~(or meta {}) ~(vec (vals fields)))))

;; just for dev/notebook
(defn timeout* [_ _ ms body]
  `(~'js/setTimeout (fn [] ~@body) ~ms))

(defmacro timeout [ms & body]
  (timeout* nil nil ms body))

(defmacro swap-> [ref & forms]
  `(swap! ~ref (fn [val#] (-> val# ~@forms))))


