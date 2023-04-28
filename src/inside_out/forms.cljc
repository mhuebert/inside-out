(ns inside-out.forms
  "API namespace"
  (:refer-clojure :exclude [assoc-in descendants update-vals])
  (:require [clojure.core :as core]
            [clojure.string :as str]
            [inside-out.macros :as macros :refer [swap->]]
            [inside-out.util :as util :refer [assoc-in update-vals]]
            [promesa.core :as p]
            [re-db.reactive :as r])
  #?(:cljs (:require-macros [inside-out.forms])))

;; TODO
;; consider how to handle async validators "on blur" only,
;; consider how to handle server-side validation which comes back as a map of {<:attribute> [... "error"]}
;;   (maybe: iterate through child fields looking for a matching attribute?
;;    need to have server-messages or custom-messages on each field?)



;; fn or map that returns metadata per symbol/attribute.
;; intended to be overridden via set! in cljs
;; eg {:person/name {:label "Name"}}
(defonce global-meta nil)

#?(:cljs
   (defn set-global-meta! [m] (set! global-meta m)))

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
    #?@(:cljs [ISwap])
    (-swap! [o f]
      (swap! !state f))
    (-swap! [o f a]
      (swap! !state f a))
    (-swap! [o f a b]
      (swap! !state f a b))
    (-swap! [o f a b xs]
      (apply swap! !state f a b xs))

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

(defn message
  ([type] {:type type})
  ([type content] {:type type :content content})
  ([type content & {:as options}] (merge options (message type content))))

(defn wrap-messages
  ([m] (wrap-messages :invalid m))
  ([default-type m]
   (cond (string? m) [(message default-type m)]
         (map? m) [m]
         (sequential? m) (into []
                               (mapcat (partial wrap-messages default-type))
                               m)
         (nil? m) []
         :else m)))

(defn validator [f & {:as options :keys [debounce-ms compute-when]}]
  (vary-meta f assoc ::validator options))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stateful validators
;;
;; A validator can call `get-vstate` and `set-vstate` to manage reactive state.

(defn get-vstate
  "Gets the state of a validator."
  [f context]
  (@(:validator/!state context) [(:sym context) f]))

(defn set-vstate
  "Sets the state of a validator."
  [f context new-state]
  (swap! (:validator/!state context) assoc [(:sym context) f] new-state))

(defn debounce
  "Wraps a validator function to debounce its execution."
  [ms f]
  #?(:clj f
     :cljs
     (if (nil? ms)
       f
       (let [last-time (volatile! (- (js/Date.now) ms 1))
             next-args (volatile! nil)
             next-timeout (volatile! nil)
             eval! (fn [value context]
                     (vreset! next-args nil)
                     (vreset! last-time (js/Date.now))
                     (set-vstate f context #:debounce{:result (f value context)
                                                      :for-value value}))
             schedule! (fn [value ctx]
                         (vreset! next-args [value ctx])
                         (some-> @next-timeout js/clearTimeout)
                         (vreset! next-timeout (js/setTimeout
                                                #(apply eval! @next-args)
                                                ms)))]
         (with-meta (fn [value context]
                      (let [vstate (get-vstate f context)
                            diff (- (js/Date.now) @last-time)
                            _ (cond (and (contains? vstate :debounce/for-value)
                                         (= value (:debounce/for-value vstate))) nil ;; value hasn't changed, no-op
                                    (or
                                     @next-args ;; already scheduled, update timer
                                     (< diff ms)) ;; still within debounce window, update timer
                                    (schedule! value context)
                                    :else (r/silently (eval! value context)))]
                        (cond-> (:debounce/result (get-vstate f context))
                                @next-args
                                (-> wrap-messages (conj (message :in-progress))))))
                    (meta f))))))

(defn handle-async-promise
  ;; handles a promise returned by an async validator
  [promise f value context]
  (let [current? #(= value (:async/result-value (get-vstate f context)))] ;; ignore promise results for old values
    (r/silently
     (set-vstate f context #:async{:result-value value
                                   :in-progress (message :in-progress)}))
    (-> (p/let [result promise]
          (when (current?)
            (set-vstate f context #:async{:result-value value
                                          :result result})))
        (p/catch
         (fn [error]
           (when (current?)
             (set-vstate f context #:async{:result-value value
                                           :error (message :error (ex-message error))})))))
    (or (:async/error (get-vstate f context))
        (message :in-progress))))

(defn async-result
  ;; handles
  [vstate f value context]
  (or (:async/error vstate)
      (:async/in-progress vstate)
      (if (= value (:async/result-value vstate))
        (:async/result vstate)
        (handle-async-promise (f value context) f value context))))

(defn compute-validator [f value context]
  #?(:clj (f value context)
     :cljs
     (let [vstate (get-vstate f context)]
       (if (contains? vstate :async/result-value)
         (async-result vstate f value context)
         (let [result (f value context)]
           (if (p/promise? result)
             (handle-async-promise result f value context)
             result))))))

(defn wrap-debounced-validator! [f {:keys [debounce-ms]} _field]
  (if debounce-ms
    (debounce debounce-ms f)
    f))

(defn get-any [ks m]
  (reduce (fn [ret k] (if-some [v (k m)] (reduced v) ret)) nil ks))

(defn is [pred message]
  (fn [v ctx]
    (when (and v (not (pred v)))
      (if (fn? message)
        (message v ctx)
        message))))

(defn in-set [set]
  (is #(contains? set %)
      #(str "Value must be one of " (str/join ", " set))))

(defn init-validator [f field]
  (let [f (cond (set? f) (in-set f)
                (identical? f string?) (is f "Must be a string")
                (identical? f number?) (is f "Must be a number")
                (identical? f boolean?) (is f "Must be a boolean")
                :else f)]
    (let [options (some-> (meta f) ::validator)]
      (-> f
          (wrap-debounced-validator! options field)))))

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

(defonce !validators (r/atom {:required required
                              :max-length max-length
                              :min-length min-length}))

(defn ensure-vector [x]
  (when x
    (if (sequential? x)
      (vec x)
      [x])))

(defn messages-reaction [^Field field]
  ;; a form's messages are computed in a reaction
  ;; (to only do the work when dependent values change)
  (let [metadata (.-metadata field)
        astate (r/atom {})
        validators (->> (:validators metadata)
                        ensure-vector
                        (concat (when (:required metadata) [:required]))
                        (replace {:required (:required @!validators)})
                        (mapv #(init-validator % field)))
        messages-fn #(do @astate
                         (compute-messages field @field validators (assoc (field-context field)
                                                                     :validator/!state astate)))]
    (r/make-reaction messages-fn)))

(defn- make-field
  [parent compute {:as meta :keys [sym attribute]}]
  {:pre [sym]}
  ;; "inherit" metadata from ancestors that contain a :meta option with
  ;; a matching symbol or attribute.
  (let [inherited-meta (->> (iterate inside-out.forms/parent parent)
                            (into () (comp (take-while identity)
                                           (keep #(:meta (.-metadata ^Field %)))))
                            (into {}
                                  (map #(cond->> (% sym)
                                                 attribute (concat (% attribute))))))
        metadata (merge (when global-meta
                          (merge (global-meta sym)
                                 (global-meta attribute)))
                        inherited-meta meta)
        field (->Field parent
                       compute
                       (r/atom (:init metadata))
                       metadata
                       (r/atom {})
                       (atom {}))]
    (swap! (!meta field) assoc :!messages (messages-reaction field))
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

(defn compute-messages
  ([field]
   (compute-messages field @field (:validators field) (assoc (field-context field)
                                                        :validator/!state (atom {}))))
  ([field value validators context]
   (->> validators
        (into []
              (comp
               (filter #(if-let [compute-when (:compute-when (::validator (meta %)))]
                          (get-any compute-when field)
                          true))
               (map #(compute-validator % value context))
               (mapcat (partial wrap-messages :invalid))
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

(defn computed-messages [^Field field]
  @(:!messages field))

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
          (let [waiting? (r/reaction! (in-progress? form))
                stop! (fn []
                        (remove-watch waiting? watch-key)
                        (r/dispose! waiting?)
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


   If the promise resolves to a message (a map containing :content), it will be set to the
   form's :remote-messages, which are included in `(messages form)`."
  [form promise]
  #?(:cljs
     (let [meta-atom (!meta form)
           complete! (fn [result]
                       (swap-> meta-atom
                               (dissoc :loading?)
                               (assoc :remote-messages
                                      (cond
                                        ;; preferred case: a single message, a map, is returned
                                        (:content result) (wrap-messages result)

                                        ;; legacy api
                                        (:messages result) (wrap-messages (:messages result))
                                        (:error result) (wrap-messages :error (:error result))
                                        (:invalid result) (wrap-messages :invalid (:invalid result))
                                        (:info result) (wrap-messages :info (:info result)))))
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
    (swap! (!meta field) select-keys [:!messages]))
  field)

(comment
 (compute-messages nil "abc" [required (max-length 2)] nil)
 (compute-messages nil nil [required (max-length 2)] nil))

(defn change-handler
  "Returns a callback which resets a ref to target.value on change"
  ([?field] (change-handler ?field {}))
  ([?field {:as options :keys [parse-value]
            :or {parse-value #(util/guard % (partial not= ""))}}]
   (fn [e]
     (reset! ?field (parse-value #?(:cljs (.. ^js e -target -value)))))))

(defn focus-handler
  [?field]
  (fn [e]
    (swap! (!meta ?field) assoc :focused true :blurred false)))

(defn blur-handler
  [?field]
  (fn [_e]
    (swap-> (!meta ?field)
            (assoc :focused false
                   :blurred true
                   :touched true))))

(defmacro form [expr & {:as opts}]
  (macros/form* &form &env expr opts))

(defmacro with-form [bindings & body]
  (macros/with-form* &form &env {} bindings body))

(comment
 ;; change-handler can be generated from cursor
 {:on-change (change-handler ?last)})

(defn valid?+
  "[async] Touches form, waits for async validators to complete, returns true if form is valid."
  [form]
  (touch! form)
  #?(:cljs
     (p/do (wait-for-async-validators+ form)
           (valid? form))
     :clj (valid? form)))

(defmacro try-submit+
  "[async] Evaluates `submit-expr` if valid?+ returns true. The form's :remove-messages are set to
   the return value of submit-expr (if it is a message) or nil."
  [form submit-expr]
  (if (:ns &env)
    `(watch-promise ~form
       (-> (valid?+ ~form)
           (.then (fn [res#] (when res# ~submit-expr)))))
    `(do (touch! ~form)
         (when (submittable? ~form) ~submit-expr))))

(defmacro for-many [[as ?field] expr]
  (let [bindings (-> &env (find ?field) key meta :many/bindings)]
    `(doall
      (for [field# ~?field]
        (let [{:syms ~(mapv second bindings)} field#
              ~as field#]
          ~expr)))))