(ns inside-out.forms
  "API namespace"
  (:refer-clojure :exclude [assoc-in atom descendants update-vals])
  (:require #?@(:cljs [[reagent.core :as reagent]
                       [reagent.ratom :as ratom]])
            [clojure.core :as core]
            [clojure.string :as str]
            [inside-out.macros :as macros :refer [swap->]]
            [inside-out.util :as util :refer [assoc-in update-vals]]
            [inside-out.promise :as p])
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
                        (when (= k :touched) (closest parent :touched)))))
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

(defn message
  ([type] {:type type})
  ([type content] {:type type :content content})
  ([type content & {:as options}] (merge options (message type content))))

(defn debounce [ms f]
  #?(:clj f
     :cljs
     (if (nil? ms)
       f
       (let [last-time (volatile! (- (js/Date.now) ms 1))
             last-result (atom* nil)
             next-args (volatile! nil)
             next-timeout (volatile! nil)
             eval! (fn [args]
                     (vreset! next-args nil)
                     (vreset! last-time (js/Date.now))
                     (reset! last-result (apply f args)))
             schedule! (fn [args]
                         (do (vreset! next-args args)
                             (some-> @next-timeout js/clearTimeout)
                             (vreset! next-timeout (js/setTimeout #(eval! @next-args) ms))))]
         (fn debounced [& args]
           (let [diff (- (js/Date.now) @last-time)]
             (if (or @next-args (< diff ms))
               (schedule! args)
               (eval! args)))
           @last-result)))))

(defn validator [f & {:as options :keys [async debounce-ms when-touched]}]
  (vary-meta f assoc ::validator options))

(defn- wrap-async-validator! [f {:keys [debounce-ms]} _field]
  #?(:clj
     f
     :cljs
     (let [state (atom* nil)
           progress-msg (message :in-progress "Loading...")
           request! (debounce debounce-ms
                      (fn [value context error]
                        (let [this-req (inc (:req @state 0))]
                          (swap! state assoc :in-progress? true :req this-req)
                          (p/catch
                           (p/let [result (f value context state)]
                             (swap-> state
                                     (assoc-in [:cache value] result)
                                     (cond-> (= this-req (:req @state))
                                             (dissoc :in-progress? :error))))
                           (fn [e]
                             (when (= this-req (:req @state))
                               (swap-> state
                                       (dissoc :in-progress?)
                                       (assoc :error (message :error (str "Validation error: " e)))))))
                          (keep identity [error progress-msg]))))]
       (fn async-validate [value context]
         #?(:cljs
            (let [{:keys [in-progress? cache error] :as st} @state]
              (or (when in-progress? progress-msg)
                  (get cache value)
                  (request! value context error))))))))

(defn wrap-touched-validator [f options field]
  (fn [value context]
    (when (:touched field)
      (f value context))))

(defn get-any [ks m]
  (reduce (fn [ret k] (if-some [v (k m)] (reduced v) ret)) nil ks))

(defn wrap-compute-when [f {:keys [compute-when]} field]
  (fn [value context]
    (when (get-any compute-when field)
      (f value context))))

(defn init-validator [f field]
  (if-let [options (some-> (meta f) ::validator)]
    (let [{:keys [async compute-when]} options]
      (cond-> f
              async (wrap-async-validator! options field)
              compute-when (wrap-compute-when options field)))
    f))

(declare compute-messages field-context)


;; ## Validation

(def required
  (fn [value _]
    (when (nil? value)
      {:type :invalid :content "Required"})))

(defn max-length
  "Returns a validator which restricts `count` of input to max `i`"
  [i]
  (fn [value _]
    (when (and value (> (count value) i))
      {:type :invalid
       :content (str "Too long (max " i " chars)")})))

(defn min-length
  "Returns a validator which restricts `count` of input to min `i`"
  [i]
  (fn [value _]
    (when (and value (< (count value) i))
      {:type :invalid
       :content (str "Too short (min " i " chars)")})))

(defonce !validators (atom* {:required required
                             :max-length max-length
                             :min-length min-length}))

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
                       meta
                       (atom* {})
                       (clojure.core/atom {}))
        validators (->> (:validators meta)
                        (concat (when (:required meta) [:required]))
                        (replace {:required (:required @!validators)})
                        (mapv #(init-validator % field)))
        messages-fn #(compute-messages @field validators (field-context field))]
    (swap! (!meta field) assoc :!messages
           #?(:cljs (ratom/make-reaction messages-fn)
              :clj  (reify clojure.lang.IDeref (deref [_] (messages-fn)))))
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
    #_#_:path (debug-name field)))

(defn wrap-messages
  ([m] (wrap-messages :invalid m))
  ([default-type m]
   (cond (string? m) [{:content m :type default-type}]
         (map? m) [m]
         (sequential? m) (into (empty m) (map (partial wrap-messages default-type)) m)
         :else m)))

(defn compute-messages
  ([field]
   (compute-messages @field (:validators field) (field-context field)))
  ([value validators context]
   (->> validators
        (into [] (comp
                  (mapcat #(wrap-messages :invalid (% value context)))
                  (keep identity)
                  (map (fn [x]
                         (assert (and (map? x) (:type x))
                                 (str "Validator message must be a map containing :type. Checking "
                                      (:path context) ", found " x))
                         x))
                  (let [sym (:sym context)]
                    (map #(assoc % :sym sym))))))))

(defn descendants [?field]
  (let [ch (vals @(!children ?field))]
    (concat ch (mapcat descendants ch))))

(defn computed-messages [^Field field] @(:!messages field))

(defn messages
  "Returns validator messages for a field/form"
  [field & {:keys [deep]}]
  (concat (:remote-messages field)
          (if deep
            (mapcat computed-messages (cons field (descendants field)))
            (computed-messages field))))

(defn show-message?
  "Returns true if message should be shown, based on :visibility of message
   and :touched/:focused state of field."
  [{:keys [focused touched]} message]
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
        (and touched (= :touched visibility))
        (and focused (= :focused visibility)))))

(def message-order
  ;; show errors above hint
  {:error 0
   :invalid 1
   :hint 2})

(defn visible-messages
  "Returns messages visible for field (not recursive), or the `not-found` message"
  ([?field] (visible-messages ?field nil))
  ([?field not-found]
   (or (->> (messages ?field :deep false)
            (sort-by (comp message-order :type))
            (filter (partial show-message? ?field))
            seq)
       not-found)))

(defn types
  "Returns set of :type values present in coll"
  [coll]
  (into #{} (map :type) coll))

(defn valid?
  "Returns true if field & descendants are valid"
  [field & {:keys [deep] :or {deep true}}]
  (not (some #{:in-progress :invalid} (types (messages field :deep deep)))))

(def invalid? (complement valid?))

;; how to handle remote submission of forms

(defn touch! [form] (swap! (!meta form) assoc :touched true))

(defn in-progress? [form]
  (:in-progress (types (messages form :deep true))))

(defn submittable?
  "Returns true if submission should be enabled (form is valid & not loading)"
  [form]
  (and (not (:loading? form))
       (not (in-progress? form))
       (valid? form)))

#?(:cljs
   (defn wait-for-async-validators+
     "Returns promise which resolves when form has no :in-progress fields"
     ^js [^Field form]
     (let [watch-key (gensym "wait-for-async-validators")]
       (js/Promise.
        (fn [resolve reject]
          (let [waiting? (reagent/track! #(in-progress? form))
                stop! (fn []
                        (remove-watch waiting? watch-key)
                        (reagent/dispose! waiting?)
                        (resolve))]
            (if @waiting?
              (add-watch waiting? watch-key
                         (fn [_ _ _ is-waiting?]
                           (when-not is-waiting?
                             (stop!))))
              (stop!))))))))

(defn clear-remote-messages! [!form]
  (swap! (!meta !form) dissoc :remote-messages))

(defn watch-promise
  "Wraps a promise to store :loading? and :remote-messages as reactive metadata on `form`.


   If the promise resolves to a map containing :error, :invalid, :info, or :messages,
   it will be set to the form's :remote-messages, which are included in `(messages form)`."
  [form promise]
  #?(:cljs
     (let [meta-atom (!meta form)
           complete! (fn [{:as result :keys [messages error invalid info]}]
                       (swap-> meta-atom
                               (dissoc :loading?)
                               (assoc :remote-messages
                                      (cond messages (wrap-messages messages)
                                            error (wrap-messages :error error)
                                            invalid (wrap-messages :invalid invalid)
                                            info (wrap-messages :info info))))
                       result)]
       (swap-> meta-atom
               (assoc :loading? true)
               (dissoc :remote-messages))
       (p/catch
        (p/-> promise complete!)
        (fn [e] (complete! {:error (ex-message e)}))))
     :clj promise))

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
 (compute-messages "abc" [required (max-length 2)] nil)
 (compute-messages nil [required (max-length 2)] nil))

(defn change-handler
  "Returns a callback which resets a ref to target.value on change"
  ([?field] (change-handler ?field {}))
  ([?field {:as options :keys [parse-value]
            :or {parse-value #(util/guard % (partial not= ""))}}]
   (fn [e]
     (reset! ?field (parse-value #?(:cljs (.. ^js e -target -value)))))))

(defn focus-handler
  [?field]
  (fn [e] (swap! (!meta ?field) assoc :focused true)))

(defn blur-handler
  [?field]
  (fn [_e]
    (swap-> (!meta ?field)
            (dissoc :focused)
            (assoc :touched true))))

(comment
 ;; change-handler can be generated from cursor
 {:on-change (change-handler ?last)})


(defmacro form [expr & opts] `(inside-out.macros/form ~expr ~@opts))

(defn valid?+
  "[async] Touches form, waits for async validators to complete, returns true if form is valid."
  [form]
  (touch! form)
  #?(:cljs
     (p/do (wait-for-async-validators+ form)
           (valid? form))
     :clj (valid? form)))

(defmacro try-submit+
  "[async] Evaluates `submit-expr` if valid?+ returns true"
  [form submit-expr]
  (if (:ns &env)
    `(watch-promise ~form
       (p/when (valid?+ ~form)
         ~submit-expr))
    `(do (touch! ~form)
         (when (submittable? ~form) ~submit-expr))))

(defmacro for-many [[as ?field] expr]
  (let [bindings (-> &env (find ?field) key meta :many/bindings)]
    `(doall
      (for [field# ~?field]
        (let [{:syms ~(mapv second bindings)} field#
              ~as field#]
          ~expr)))))