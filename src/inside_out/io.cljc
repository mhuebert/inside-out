(ns inside-out.io
  "IO DSL: Human-AI Collaborative Forms

   Core namespace for the io DSL that extends inside-out forms
   for AI coordination workflows.

   Syntax:
     ?var           - Human input slot
     $var           - AI generation slot
     (?var :ask ..) - Human input with prompt
     ($var :ai ..)  - AI slot with generation hints
     {interpolation} - String interpolation in prompts

   Example:
     {:topic   ?topic
      :outline ($outline :ai \"Create outline for {?topic}\")
      :draft   ($draft :ai \"Write draft following {$outline}\")}
  "
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [inside-out.forms :as forms]
            [re-db.reactive :as r]
            [promesa.core :as p])
  #?(:cljs (:require-macros [inside-out.io])))

;; =============================================================================
;; SLOT TYPES
;; =============================================================================

(defn human-slot? [x]
  (and (symbol? x)
       (str/starts-with? (name x) "?")))

(defn ai-slot? [x]
  (and (symbol? x)
       (str/starts-with? (name x) "$")))

(defn slot? [x]
  (or (human-slot? x) (ai-slot? x)))

(defn anonymous-slot? [x]
  (and (symbol? x)
       (contains? #{"?" "$"} (name x))))

;; =============================================================================
;; FORM ANALYSIS
;; =============================================================================

(defn extract-slots
  "Extract all slots from an io form, returning a map of {slot-sym -> slot-info}"
  [form]
  (let [slots (atom {})
        counter (atom 0)]
    (walk/postwalk
      (fn [x]
        (cond
          ;; Anonymous slot - assign unique name
          (anonymous-slot? x)
          (let [prefix (if (= "?" (name x)) "?_anon" "$_anon")
                sym (symbol (str prefix (swap! counter inc)))]
            (swap! slots assoc sym {:sym sym
                                    :type (if (human-slot? x) :human :ai)
                                    :anonymous? true})
            sym)

          ;; Named slot without metadata
          (and (slot? x) (not (list? x)))
          (do (swap! slots update x merge {:sym x
                                           :type (if (human-slot? x) :human :ai)})
              x)

          ;; Slot with metadata: (?var :key val ...)
          (and (list? x)
               (slot? (first x)))
          (let [[sym & kvs] x
                meta (apply hash-map kvs)]
            (swap! slots update sym merge {:sym sym
                                           :type (if (human-slot? sym) :human :ai)
                                           :meta meta})
            sym)

          :else x))
      form)
    @slots))

(defn extract-interpolations
  "Extract {var} references from a string"
  [s]
  (when (string? s)
    (->> (re-seq #"\{([^}]+)\}" s)
         (map (comp symbol second))
         (filter slot?)
         set)))

(defn find-dependencies
  "Find all slot dependencies in a form or string"
  [form slots]
  (let [deps (atom #{})]
    (walk/postwalk
      (fn [x]
        (cond
          (and (slot? x) (contains? slots x))
          (do (swap! deps conj x) x)

          (string? x)
          (do (swap! deps into (extract-interpolations x)) x)

          :else x))
      form)
    @deps))

(defn build-dependency-graph
  "Build a dependency graph for all slots in a form"
  [form]
  (let [slots (extract-slots form)]
    (reduce-kv
      (fn [graph sym {:keys [meta] :as slot-info}]
        (let [;; Dependencies from :ai/:ask prompts
              prompt-deps (when-let [prompt (or (:ai meta) (:ask meta))]
                            (extract-interpolations prompt))
              ;; Dependencies from :from references
              from-deps (when-let [from (:from meta)]
                          (if (slot? from) #{from} #{}))
              all-deps (into (or prompt-deps #{}) from-deps)]
          (assoc graph sym (assoc slot-info :deps all-deps))))
      {}
      slots)))

(defn topological-sort
  "Sort slots by dependency order"
  [dep-graph]
  (let [in-degree (reduce-kv
                    (fn [m sym {:keys [deps]}]
                      (reduce (fn [m d]
                                (update m d (fnil inc 0)))
                              (update m sym (fnil identity 0))
                              deps))
                    {}
                    dep-graph)
        queue (into [] (filter #(zero? (in-degree %)) (keys dep-graph)))]
    (loop [queue queue
           result []
           in-deg in-degree]
      (if (empty? queue)
        (if (= (count result) (count dep-graph))
          result
          (throw (ex-info "Circular dependency detected" {:graph dep-graph})))
        (let [sym (first queue)
              dependents (->> dep-graph
                              (filter (fn [[_ {:keys [deps]}]] (contains? deps sym)))
                              (map first))
              new-in-deg (reduce (fn [m d] (update m d dec)) in-deg dependents)
              new-ready (filter #(zero? (new-in-deg %)) dependents)]
          (recur (into (vec (rest queue)) new-ready)
                 (conj result sym)
                 new-in-deg))))))

;; =============================================================================
;; INTERPOLATION
;; =============================================================================

(defn interpolate
  "Replace {var} references in a string with their values"
  [s values]
  (if-not (string? s)
    s
    (str/replace s #"\{([^}]+)\}"
                 (fn [[_ ref]]
                   (let [sym (symbol ref)]
                     (str (get values sym ref)))))))

;; =============================================================================
;; SLOT STATE
;; =============================================================================

(defrecord SlotState
  [sym           ; Symbol name
   type          ; :human or :ai
   status        ; :pending :ready :in-progress :complete :error :awaiting-approval
   value         ; Current value (nil if pending)
   error         ; Error if status is :error
   meta          ; Slot metadata (:ask, :ai, :approve, :ui, etc.)
   deps          ; Set of dependency symbols
   history])     ; Vector of {:timestamp :action :value}

(defn make-slot-state [sym {:keys [type meta deps]}]
  (map->SlotState
    {:sym sym
     :type type
     :status :pending
     :value nil
     :error nil
     :meta (or meta {})
     :deps (or deps #{})
     :history []}))

(defn slot-ready?
  "A slot is ready to execute when all its dependencies are complete"
  [slot-state all-states]
  (and (= :pending (:status slot-state))
       (every? (fn [dep]
                 (= :complete (:status (get all-states dep))))
               (:deps slot-state))))

(defn slot-blocked?
  "A slot is blocked if any dependency is not complete"
  [slot-state all-states]
  (and (= :pending (:status slot-state))
       (some (fn [dep]
               (not= :complete (:status (get all-states dep))))
             (:deps slot-state))))

;; =============================================================================
;; WORKFLOW STATE
;; =============================================================================

(defrecord WorkflowState
  [id            ; Unique workflow ID
   form          ; Original io form
   slots         ; Map of {sym -> SlotState}
   order         ; Topological order of slots
   values        ; Map of {sym -> value} for completed slots
   created-at    ; Timestamp
   updated-at    ; Timestamp
   status])      ; :running :paused :complete :error

(defn make-workflow [id form]
  (let [dep-graph (build-dependency-graph form)
        order (topological-sort dep-graph)
        slots (reduce-kv
                (fn [m sym info]
                  (assoc m sym (make-slot-state sym info)))
                {}
                dep-graph)]
    (map->WorkflowState
      {:id id
       :form form
       :slots slots
       :order order
       :values {}
       :created-at #?(:clj (java.util.Date.) :cljs (js/Date.))
       :updated-at #?(:clj (java.util.Date.) :cljs (js/Date.))
       :status :running})))

;; =============================================================================
;; STATE QUERIES
;; =============================================================================

(defn pending-slots
  "Get slots that are pending (not yet started)"
  [workflow]
  (->> (:slots workflow)
       (filter (fn [[_ s]] (= :pending (:status s))))
       (into {})))

(defn ready-slots
  "Get slots that are ready to execute"
  [workflow]
  (->> (:slots workflow)
       (filter (fn [[_ s]] (slot-ready? s (:slots workflow))))
       (into {})))

(defn blocked-slots
  "Get slots that are blocked on dependencies"
  [workflow]
  (->> (:slots workflow)
       (filter (fn [[_ s]] (slot-blocked? s (:slots workflow))))
       (into {})))

(defn in-progress-slots
  "Get slots currently being processed"
  [workflow]
  (->> (:slots workflow)
       (filter (fn [[_ s]] (= :in-progress (:status s))))
       (into {})))

(defn awaiting-approval-slots
  "Get AI slots awaiting human approval"
  [workflow]
  (->> (:slots workflow)
       (filter (fn [[_ s]] (= :awaiting-approval (:status s))))
       (into {})))

(defn complete-slots
  "Get completed slots"
  [workflow]
  (->> (:slots workflow)
       (filter (fn [[_ s]] (= :complete (:status s))))
       (into {})))

(defn next-human-slot
  "Get the next human slot that needs input"
  [workflow]
  (first (filter (fn [[sym s]]
                   (and (= :human (:type s))
                        (slot-ready? s (:slots workflow))))
                 (map (fn [sym] [sym (get-in workflow [:slots sym])])
                      (:order workflow)))))

(defn next-ai-slot
  "Get the next AI slot that can be generated"
  [workflow]
  (first (filter (fn [[sym s]]
                   (and (= :ai (:type s))
                        (slot-ready? s (:slots workflow))))
                 (map (fn [sym] [sym (get-in workflow [:slots sym])])
                      (:order workflow)))))

(defn workflow-progress
  "Calculate workflow progress as percentage"
  [workflow]
  (let [total (count (:slots workflow))
        complete (count (complete-slots workflow))]
    (if (zero? total)
      100
      (int (* 100 (/ complete total))))))

(defn workflow-complete?
  "Check if workflow is complete"
  [workflow]
  (every? (fn [[_ s]] (= :complete (:status s)))
          (:slots workflow)))

;; =============================================================================
;; STATE TRANSITIONS
;; =============================================================================

(defn update-slot
  "Update a slot's state"
  [workflow sym updates]
  (let [now #?(:clj (java.util.Date.) :cljs (js/Date.))
        history-entry (merge {:timestamp now} updates)]
    (-> workflow
        (update-in [:slots sym] merge updates)
        (update-in [:slots sym :history] conj history-entry)
        (assoc :updated-at now))))

(defn set-slot-value
  "Set a slot's value and mark complete"
  [workflow sym value]
  (-> workflow
      (update-slot sym {:status :complete :value value})
      (assoc-in [:values sym] value)))

(defn set-slot-error
  "Mark a slot as errored"
  [workflow sym error]
  (update-slot workflow sym {:status :error :error error}))

(defn set-slot-in-progress
  "Mark a slot as in progress"
  [workflow sym]
  (update-slot workflow sym {:status :in-progress}))

(defn set-slot-awaiting-approval
  "Mark an AI slot as awaiting human approval"
  [workflow sym value]
  (update-slot workflow sym {:status :awaiting-approval :value value}))

(defn approve-slot
  "Approve an AI-generated value"
  [workflow sym]
  (let [value (get-in workflow [:slots sym :value])]
    (-> workflow
        (update-slot sym {:status :complete})
        (assoc-in [:values sym] value))))

(defn reject-slot
  "Reject an AI-generated value, reset to pending for regeneration"
  [workflow sym reason]
  (update-slot workflow sym {:status :pending
                             :value nil
                             :rejection-reason reason}))

;; =============================================================================
;; SERIALIZATION
;; =============================================================================

(defn serialize-workflow
  "Serialize workflow state for persistence"
  [workflow]
  {:id (:id workflow)
   :form (pr-str (:form workflow))
   :slots (->> (:slots workflow)
               (map (fn [[sym s]]
                      [(str sym)
                       {:status (:status s)
                        :value (:value s)
                        :error (:error s)
                        :history (:history s)}]))
               (into {}))
   :values (->> (:values workflow)
                (map (fn [[sym v]] [(str sym) v]))
                (into {}))
   :created-at (str (:created-at workflow))
   :updated-at (str (:updated-at workflow))
   :status (:status workflow)})

(defn deserialize-workflow
  "Deserialize workflow state from persistence"
  [data]
  (let [form (read-string (:form data))
        base-workflow (make-workflow (:id data) form)]
    (-> base-workflow
        (assoc :slots
               (reduce-kv
                 (fn [m sym-str saved]
                   (let [sym (symbol sym-str)]
                     (update m sym merge
                             {:status (keyword (:status saved))
                              :value (:value saved)
                              :error (:error saved)
                              :history (or (:history saved) [])})))
                 (:slots base-workflow)
                 (:slots data)))
        (assoc :values
               (reduce-kv
                 (fn [m sym-str v]
                   (assoc m (symbol sym-str) v))
                 {}
                 (:values data)))
        (assoc :status (keyword (:status data))))))

;; =============================================================================
;; REACTIVE WORKFLOW (for UI integration)
;; =============================================================================

(defn make-reactive-workflow
  "Create a reactive workflow state atom"
  [id form]
  (r/atom (make-workflow id form)))

(defn reactive-slot-status
  "Get reactive status of a slot"
  [!workflow sym]
  (r/reaction (get-in @!workflow [:slots sym :status])))

(defn reactive-slot-value
  "Get reactive value of a slot"
  [!workflow sym]
  (r/reaction (get-in @!workflow [:slots sym :value])))

(defn reactive-next-action
  "Get the next action needed (human input or AI generation)"
  [!workflow]
  (r/reaction
    (let [wf @!workflow]
      (or (when-let [[sym _] (first (awaiting-approval-slots wf))]
            {:action :approve :slot sym})
          (when-let [[sym _] (next-human-slot wf)]
            {:action :human-input :slot sym})
          (when-let [[sym _] (next-ai-slot wf)]
            {:action :ai-generate :slot sym})
          (when (workflow-complete? wf)
            {:action :complete})))))

;; =============================================================================
;; FORM MACRO
;; =============================================================================

(defmacro io
  "Create an io workflow from a form expression"
  [form & {:keys [id] :or {id (gensym "workflow-")}}]
  `(make-reactive-workflow '~id '~form))

(comment
  ;; Example usage
  (def !wf (io {:topic ?topic
                :outline ($outline :ai "Create outline for {?topic}")
                :draft ($draft :ai "Write draft following {$outline}")}))

  (build-dependency-graph '{:topic ?topic
                            :outline ($outline :ai "Create outline for {?topic}")})

  (topological-sort (build-dependency-graph '{:topic ?topic
                                              :outline ($outline :ai "Create outline for {?topic}")}))
  )
