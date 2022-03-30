(ns inside-out.forms
  "API namespace"
  (:refer-clojure :exclude [assoc-in atom descendants update-vals])
  (:require #?@(:cljs [[reagent.core :as reagent]
                       [reagent.ratom :as ratom]])
            [applied-science.js-interop :as j]
            [clojure.string :as str]
            [inside-out.macros :as macros]
            [inside-out.util :as util :refer [assoc-in update-vals]])
  #?(:cljs (:require-macros [inside-out.forms])))

;; intended to be overridden via set! in cljs
;; intended use case is defining attribute-metadata globally
;; eg {:person/name {:label "Name"}}
(def global-meta {})

(declare closest)

(macros/support-clj-protocols
  (deftype Field [parent compute !state metadata !meta !children]
    ISeqable
    (-seq [o]
      (if (:many metadata)
        (seq (map @!children @!state))
        (seq (vals @!children))))
    ILookup
    (-lookup [o k]
      (if (symbol? k)
        (get @!children k)
        (macros/some-or (@!meta k)
                        (metadata k)
                        (when (= k :touched?) (closest parent :touched?)))))
    (-lookup [o k nf] (macros/some-or (get o k) nf))

    IDeref
    (-deref [o]
      (cond compute (compute (update-vals @!children deref))
            (:many metadata) (let [state @!state]
                               (into (empty state)
                                     (map (comp deref @!children))
                                     state))
            :else @!state))

    IReset
    (-reset! [o new-value]
      (reset! !state new-value))

    IMeta
    (-meta [o] (merge metadata @!meta))

    IWithMeta
    (-with-meta [o meta] (Field. parent compute !state meta !meta !children))
    IFn
    (-invoke [this bindings]
      (if-let [many (:many metadata)]
        (mapv (:many/compute many) bindings)
        (do
          (assert compute (str (:sym metadata) " is a simple field not associated with an expression."))
          (compute bindings))))))

(defn compute [field bindings] (field bindings))

(defn !meta [^Field field] (.-!meta field))
(defn !children [^Field ?field] (.-!children ?field))
(def children (comp deref !children))
(defn !state [^Field ?field] (.-!state ?field))
(defn parent ^Field [^Field ?field] (.-parent ?field))

(defn closest [field pred]
  (when field
    (if-some [v (pred field)]
      v
      (closest (parent field) pred))))

(def atom* #?(:cljs reagent/atom :clj clojure.core/atom))

(defn- make-field
  [parent compute {:as meta :keys [sym attribute]}]
  {:pre [sym]}
  ;; "inherit" metadata from ancestors that contain a :meta option with
  ;; a matching symbol or attribute.
  (let [inherited-meta (->> (iterate inside-out.forms/parent parent)
                            (into () (comp (take-while identity)
                                           (keep #(:meta (.-metadata ^Field %)))))
                            (cons global-meta)
                            (into {}
                                  (map #(cond->> (% sym)
                                                 attribute (concat (% attribute))))))
        meta (merge inherited-meta meta)
        field (->Field parent
                       compute
                       (atom* (:init meta))
                       (merge inherited-meta meta)
                       (atom* {})
                       (clojure.core/atom {}))]
    field))

(declare make-binding!)

(defn add-to-parent! [child parent]
  {:pre [child]}
  (swap! (!children parent)
         assoc
         (:sym child)
         child)
  child)

(defn remove-binding! [child]
  (swap! (!children (parent child)) dissoc (:sym child)))

(defn- make-many-child [field bindings]
  (let [{:many/keys [compute fields]} (:many field)
        child (make-field field compute (merge {:sym (gensym 'list-child-)}
                                                    (:child-meta field)))]
    (doseq [[sym meta] fields
            :let [init (get bindings sym)]]
      (make-binding! child (cond-> meta (some? init) (assoc :init init))))
    child))

(defn add-many! [plural-field & children]
  (->> children
       (mapv (fn [bindings]
               (let [child (make-many-child plural-field bindings)]
                 (swap! (!children plural-field) assoc (:sym child) child)
                 (swap! (!state plural-field) conj (:sym child))
                 child)))))

(defn remove-many! [& children]
  (doseq [{:as ?child :keys [sym]} children]
    (swap! (!state (parent ?child)) #(vec (remove #{sym} %)))
    (remove-binding! ?child))
  nil)

(defn init-many! [field]
  (when (:many field)
    (let [child-bindings (:init field)]
      (reset! (!state field) (if child-bindings (empty child-bindings) []))
      (reset! (!children field) {})
      (apply add-many! field child-bindings)))
  field)

(defn make-binding!
  [parent child-meta]
  (-> (make-field parent nil child-meta)
      init-many!
      (add-to-parent! parent)))

(defn root [compute meta fields]
  (let [root-field (make-field nil compute (assoc (or meta {}) :sym `!root))]
    (doseq [field-meta fields] (make-binding! root-field field-meta))
    root-field))

;; ## Validation

(defn max-length
  "Returns a validator which restricts `count` of input to max `i`"
  [i]
  (fn [value & [_context]]
    (when (and value (> (count value) i))
      {:type :invalid
       :content (str "Too long (max " i " chars)")})))

(defn min-length
  "Returns a validator which restricts `count` of input to min `i`"
  [i]
  (fn [value & [_context]]
    (when (and value (< (count value) i))
      {:type :invalid
       :content (str "Too short (min " i " chars)")})))

(def required
  (fn [value & _]
    (when (nil? value)
      {:type :invalid :content "Required"})))

(defn debug-name [?field]
  (->> (iterate parent ?field)
       (take-while identity)
       (drop-last)
       (map :sym)
       reverse
       (str/join " > ")))

(defn field-context
  [field]
  (assoc (closest field (comp not-empty deref !children))
    :sym (:sym field)
    :path (debug-name field)))

(defn wrap-message [m]
  (cond (string? m) [{:content m}]
        (map? m) [m]
        :else m))

(defn compute-messages
  ([field]
   (compute-messages @field (cond->> (:validators field)
                                     (:required field)
                                     (concat [:required])) (field-context field)))
  ([value validators context]
   (->> validators
        (replace {:required required})
        (into [] (comp
                  (mapcat #(wrap-message (% value context)))
                  (keep identity)
                  (map (fn [x]
                         (assert (and (map? x) (:type x) (:content x))
                                 (str "Validator message must be a map containing :type and :context. Checking "
                                      (:path context) ", found " x))
                         x))
                  (let [sym (:sym context)]
                    (map #(assoc % :sym sym))))))))

(defn descendants [?field]
  (let [ch (vals @(!children ?field))]
    (concat ch (mapcat descendants ch))))

(defn messages
  "Returns validator messages for a field/form"
  [field & {:keys [deep]}]
  (concat (:remote-messages field)
          (if deep
            (mapcat compute-messages (cons field (descendants field)))
            (compute-messages field))))

(defn show-message?
  "Returns true if message should be shown, based on :visibility of message
   and :touched/:focused state of field."
  [{:keys [focused? touched?]} message]
  (let [visibility (or (:visibility message)
                       ;; visibility defaults
                       (case (:type message)
                         ;; show invalid when touched
                         :invalid :touched
                         ;; show hint when focused
                         :hint :focused
                         ;; any other type (or no type), always show
                         :always))]
    (or (= :always visibility)
        (and touched? (= :touched visibility))
        (and focused? (= :focused visibility)))))

(def message-order
  ;; show errors above hint
  {:invalid 1
   :hint 2})

(def nonbreaking-space "the &nbsp; character (for taking up space when no messages are present)"
  \u00A0)
(def empty-message {:content nonbreaking-space})

(defn visible-messages
  "Returns messages visible for field (not recursive), or the `not-found` message (default: a nonbreaking-space)"
  ([?field] (visible-messages ?field nonbreaking-space))
  ([?field not-found]
   (or (->> (messages ?field :deep false)
            (sort-by (comp message-order :type))
            (filter (partial show-message? ?field))
            seq)
       (wrap-message not-found))))

(defn types
  "Returns set of :type values present in coll"
  [coll]
  (into #{} (map :type) coll))

(defn valid?
  "Returns true if field & descendants are valid"
  [field & {:keys [deep] :or {deep true}}]
  (not (:invalid (types (messages field :deep deep)))))

(def invalid? (complement valid?))

;; how to handle remote submission of forms

(defn touch! [form] (swap! (!meta form) assoc :touched? true))

(defn submittable?
  "Returns true if submission should be enabled (form is valid & not loading)"
  [form]
  (and (not (:loading? form))
       (valid? form)))

#?(:cljs
   (defn watch-promise
     "Wraps a promise to store :loading? and :remote-messages as reactive metadata on `form`.

      If the promise resolves to a map containing :messages, these will be set as
      the form's :remote-messages, which are included in `(messages form)`.

      Added sugar: if an :error key is present, it will be considered the only message
      and wrapped as {:type :error, :content error}"
     [form promise]
     (let [meta-atom (!meta form)
           complete! (fn [{:as result :keys [message]}]
                       (macros/swap-> meta-atom
                                      (dissoc :loading?)
                                      (assoc :remote-messages
                                        (wrap-message message)))
                       result)]
       (macros/swap-> meta-atom
                      (assoc :loading? true)
                      (dissoc :remote-messages))
       (-> promise
           (j/call :then complete!)
           (j/call :catch (fn [e] (complete! {:error (ex-message e)})))))))

(defn clear!
  "Resets the form to initial values"
  [field & {:keys [deep] :or {deep true}}]
  (doseq [field (if deep (cons field (descendants field)) [field])]
    (if (:many field)
      (init-many! field)
      (reset! field (:init field)))
    (reset! (!meta field) {}))
  field)

(comment
 (compute-messages "abc" [:required (max-length 2)] nil)
 (compute-messages nil [:required (max-length 2)] nil))

(defn change-handler
  "Returns a callback which resets a ref to target.value on change"
  ([?field] (change-handler ?field {}))
  ([?field {:as options :keys [parse-value]
            :or {parse-value #(util/guard % (partial not= ""))}}]
   (fn [e]
     (reset! ?field (parse-value #?(:cljs (j/get-in e [:target :value])))))))

(defn focus-handler
  [?field]
  (fn [e] (swap! (!meta ?field) assoc :focused? true)))

(defn blur-handler
  [?field]
  (util/memo-on ?field ::on-blur
                (fn [e] (macros/swap-> (!meta ?field)
                                       (dissoc :focused?)
                                       (assoc :touched? true)))))

(comment
 ;; change-handler can be generated from cursor
 {:on-change (change-handler ?last)})


(defmacro form [expr & opts] `(inside-out.macros/form ~expr ~@opts))
(defmacro try-submit!
  "If form is submittable, evaluate and watch `promise`, otherwise touch form."
  [form promise]
  `(if (submittable? ~form)
     (watch-promise ~form ~promise)
     (touch! ~form)))

(defmacro for-many [[as ?field] expr]
  (let [bindings (-> &env (find ?field) key meta :many/bindings)]
    `(doall
      (for [field# ~?field]
        (let [{:syms ~(mapv second bindings)} field#
              ~as field#]
          ~expr)))))