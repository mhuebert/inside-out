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
         (map? m) (do
                    (when-not (:type m) (throw (ex-info "Message must have a :type key" {:message m})))
                    [m])
         (sequential? m) (into []
                               (comp (mapcat (partial wrap-messages default-type))
                                     (keep identity))
                               m)
         :else m)))

(defn validator [f & {:as options :keys [debounce-ms
                                         compute-when
                                         on-blur]}]
  (vary-meta f merge options))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stateful validators
;;
;; A validator can call `get-vstate` and `set-vstate` to manage reactive state.

(defn get-vstate
  "Gets the state of a validator."
  [k f context]
  (@(:validator/!state context) [k (:sym context) f]))

(defn set-vstate
  "Sets the state of a validator."
  [k f context new-state]
  (swap! (:validator/!state context) assoc [k (:sym context) f] new-state))

(defn valid-vstate?
  "The vstate is valid for the current value"
  [value vstate]
  (and (contains? vstate :for-value)
       (= value (:for-value vstate))))

(defn debounce
  "Wraps a validator function to debounce its execution."
  [f ms]
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
                     (set-vstate :debounce f context {:result (f value context)
                                                      :for-value value}))
             schedule! (fn [value ctx]
                         (vreset! next-args [value ctx])
                         (some-> @next-timeout js/clearTimeout)
                         (vreset! next-timeout (js/setTimeout
                                                #(apply eval! @next-args)
                                                ms)))]
         (with-meta (fn [value context]
                      (let [vstate (get-vstate :debounce f context)
                            diff (- (js/Date.now) @last-time)
                            _ (cond (valid-vstate? value vstate) nil ;; value hasn't changed, no-op
                                    (or
                                     @next-args ;; already scheduled, update timer
                                     (< diff ms)) ;; still within debounce window, update timer
                                    (schedule! value context)
                                    :else (r/silently (eval! value context)))
                            result (:result (get-vstate :debounce f context))]
                        (if @next-args
                          (message :in-progress)
                          result)))
                    (meta f))))))

(defn on-blur
  "Validator will only run when the field is blurred. Messages remain displayed until value changes."
  [f]
  (fn [value {:as context :keys [field]}]
    (if (:blurred field)
      (let [result (f value context)]
        (->> {:result result :for-value value}
             (set-vstate :on-blur f context)
             (r/silently))
        result)
      (let [vstate (get-vstate :on-blur f context)]
        (when (valid-vstate? value vstate)
          (:result vstate))))))

(defn get-any [ks m]
  (reduce (fn [ret k] (if-some [v (k m)] (reduced v) ret)) nil ks))

(defn validate-when [f conditions]
  (fn [value context]
    (when (get-any conditions (:field context))
      (f value context))))

(defn handle-promise-result
  ;; handles a newly-created promise, returned by an async validator.
  [result f value context]
  (if-not (p/promise? result)
    result
    (do (when (not= result (:promise (get-vstate :async f context))) ;; in case we've already handled this promise
          (r/silently
           (set-vstate :async f context {:for-value value
                                         :promise result
                                         :in-progress (message :in-progress)}))
          (-> (p/let [result result]
                (when (valid-vstate? value (get-vstate :async f context))
                  (set-vstate :async f context {:for-value value
                                                :promise result
                                                :result result})))
              (p/catch
               (fn [error]
                 (when (valid-vstate? value (get-vstate :async f context))
                   (set-vstate :async f context {:for-value value
                                                 :promise result
                                                 :error (message :error (ex-message error))}))))))
        (message :in-progress))))

(defn async-result
  [f context]
  (let [{:keys [error in-progress result]} (get-vstate :async f context)]
    (or error in-progress result)))

(defn compute-validator [f value context]
  (wrap-messages
   #?(:clj (f value context)
      :cljs
      ;; if we have a valid async result, show it, otherwise compute the result
      ;; (we need to do this check so that side-effecting async validators don't
      ;;  re-run while in progress)
      (if (valid-vstate? value (get-vstate :async f context))
        (async-result f context)
        (-> (f value context)
            (handle-promise-result f value context))))))

(defn is [pred message]
  (fn [v ctx]
    (when (and v (not (pred v)))
      (if (fn? message)
        (message v ctx)
        message))))

(defn in-set [set]
  (is #(contains? set %)
      #(str "Value must be one of " (str/join ", " set))))

(defn init-validator [f]
  (let [f (cond (set? f) (in-set f)
                (identical? f string?) (is f "Must be a string")
                (identical? f number?) (is f "Must be a number")
                (identical? f boolean?) (is f "Must be a boolean")
                :else f)]
    (let [{:keys [debounce-ms
                  compute-when
                  on-blur]} (meta f)]
      (cond-> f
              debounce-ms (debounce debounce-ms)
              compute-when (validate-when compute-when)
              on-blur (on-blur)))))

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
        vstate (r/atom {})
        validators (->> (:validators metadata)
                        ensure-vector
                        (concat (when (:required metadata) [:required]))
                        (replace {:required (:required @!validators)})
                        (mapv init-validator))
        messages-fn #(do @vstate
                         (-> (compute-messages @field validators (assoc (field-context field)
                                                                   :validator/!state vstate))
                             (into (when-let [{:keys [for-value messages]} (:remote-messages @(!meta field))]
                                     (when (= for-value @field) messages)) )))]
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
    :field field
    :sym (:sym field)
    #_#_:path (debug-name field)))

(defn compute-messages
  ([field]
   (compute-messages @field (:validators field) (assoc (field-context field)
                                                  :validator/!state (atom {}))))
  ([value validators {:as context :keys [sym]}]
   (->> validators
        (into []
              (comp
               (mapcat #(compute-validator % value context))
               (map #(assoc % :sym sym)))))))

(defn descendants [?field]
  (let [ch (vals @(!children ?field))]
    (concat ch (mapcat descendants ch))))

(defn computed-messages [^Field field]
  @(:!messages field))

(defn messages
  "Returns validator messages for a field/form"
  [field & {:keys [deep]}]
  (if deep
    (mapcat computed-messages (cons field (descendants field)))
    (computed-messages field)))

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
  (doseq [field (cons !form (descendants !form))]
    (swap! (!meta field) dissoc :remote-messages)))

(defn set-path-messages!
  "Given a map of {<path> [...messages]}, sets :remote-messages on fields
   at <path> in form. Messages that cannot be unambiguously assigned to a field
    based on its :attribute are assigned to the form itself."
  [!form path-messages]
  (let [fields-by-attr (->> (group-by :attribute (descendants !form))
                            (reduce-kv (fn [m attr fields]
                                         (if (= 1 (count fields))
                                           (assoc m attr (first fields))
                                           m)) {}))]
    (doseq [[path messages] path-messages
            :let [!field (fields-by-attr (last path) !form)]]
      (if !field
        (swap! (!meta !field) assoc :remote-messages {:for-value @!field :messages messages})
        (swap! (!meta !form) assoc :remote-messages {:for-value @!form :messages (map #(str % " at " path) messages)}))))
  !form)

(comment
 (-> (inside-out.forms/form {:X ?x :Y {:z ?z :Q ?q}})
     (set-path-messages! {[:X] ["X"]
                          [:Y] ["in root"]
                          [:X :Q] ["Q"]})
     (messages :deep false)))

(defn parse-remote-messages
  ;; return a map of {<path> [...messages]}
  [result]
  (cond
    (::messages-by-path result) (update-vals (::messages-by-path result)
                                             wrap-messages)
    ;; preferred case: a single message, a map, is returned
    (:content result) {[] (wrap-messages result)}

    ;; legacy api
    (:messages result) {[] (wrap-messages (:messages result))}
    (:error result) {[] (wrap-messages :error (:error result))}
    (:invalid result) {[] (wrap-messages :invalid (:invalid result))}
    (:info result) {[] (wrap-messages :info (:info result))}))

(defn watch-promise
  "Wraps a promise to store :loading? and :remote-messages as reactive metadata on `form`.

   If the promise resolves to a message (a map containing :content), it will be set to the
   form's :remote-messages, which are included in `(messages form)`."
  [!form promise]
  #?(:cljs
     (let [meta-atom (!meta !form)
           complete! (fn [result]
                       (swap! meta-atom dissoc :loading?)
                       (set-path-messages! !form (parse-remote-messages result))
                       result)]
       (clear-remote-messages! !form)
       (swap! meta-atom assoc :loading? true)
       (p/catch
        (p/-> promise complete!)
        (fn [e] (complete! (or (util/guard (ex-data e) ::messages-by-path)
                               {:error (ex-message e)})))))
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
  "[async] Evaluates `submit-expr` if valid?+ returns true. The form's :remote-messages are set to
   the return value of submit-expr (if it is a message) or nil."
  [form submit-expr]
  (if (:ns &env)
    `(watch-promise ~form
       (-> (valid?+ ~form)
           (.then (fn [valid#] (when valid# ~submit-expr)))))
    `(do (touch! ~form)
         (when (submittable? ~form) ~submit-expr))))

(defmacro for-many [[as ?field] expr]
  (let [bindings (-> &env (find ?field) key meta :many/bindings)]
    `(doall
      (for [field# ~?field]
        (let [{:syms ~(mapv second bindings)} field#
              ~as field#]
          ~expr)))))