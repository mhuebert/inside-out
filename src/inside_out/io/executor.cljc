(ns inside-out.io.executor
  "Executor runtime for io workflows

   The executor orchestrates the flow of a workflow:
   - Identifies ready slots
   - Invokes AI for $slots
   - Presents UIs for ?slots
   - Handles approval gates
   - Advances the workflow automatically"
  (:require [clojure.string :as str]
            [inside-out.io :as io]
            [promesa.core :as p]
            [re-db.reactive :as r]))

;; =============================================================================
;; AI CLIENT PROTOCOL
;; =============================================================================

(defprotocol IAIClient
  "Protocol for AI generation"
  (generate [this prompt opts]
    "Generate a response for the given prompt.
     Returns a promise that resolves to the generated text.
     opts may include :model, :temperature, :max-tokens, etc."))

;; Mock AI client for development
(defrecord MockAIClient []
  IAIClient
  (generate [_ prompt opts]
    (p/do
      (p/delay (+ 500 (rand-int 1000)))  ; Simulate latency
      (str "AI response to: " (subs prompt 0 (min 50 (count prompt))) "..."))))

(defn make-mock-client []
  (->MockAIClient))

;; =============================================================================
;; EXECUTOR STATE
;; =============================================================================

(defrecord Executor
  [!workflow      ; Reactive atom holding WorkflowState
   ai-client      ; IAIClient implementation
   !running?      ; Atom<boolean> - whether executor is actively processing
   on-human-input ; Callback when human input is needed: (fn [slot-sym slot-meta])
   on-approval    ; Callback when approval is needed: (fn [slot-sym value slot-meta])
   on-complete    ; Callback when workflow completes: (fn [workflow])
   on-error])     ; Callback on error: (fn [slot-sym error])

(defn make-executor
  "Create an executor for a workflow"
  [{:keys [!workflow ai-client on-human-input on-approval on-complete on-error]}]
  (map->Executor
    {:!workflow !workflow
     :ai-client (or ai-client (make-mock-client))
     :!running? (atom false)
     :on-human-input (or on-human-input (fn [sym meta] (println "Need input for:" sym)))
     :on-approval (or on-approval (fn [sym val meta] (println "Need approval for:" sym)))
     :on-complete (or on-complete (fn [wf] (println "Workflow complete!")))
     :on-error (or on-error (fn [sym err] (println "Error in" sym ":" err)))}))

;; =============================================================================
;; PROMPT BUILDING
;; =============================================================================

(defn build-prompt
  "Build the AI prompt by interpolating values"
  [slot-state workflow]
  (let [prompt-template (or (get-in slot-state [:meta :ai])
                            (str "Generate content for " (:sym slot-state)))
        values (:values workflow)]
    (io/interpolate prompt-template values)))

;; =============================================================================
;; SLOT EXECUTION
;; =============================================================================

(defn execute-ai-slot!
  "Execute an AI slot - generate content and update state"
  [{:keys [!workflow ai-client on-approval on-error]} sym]
  (let [workflow @!workflow
        slot-state (get-in workflow [:slots sym])
        prompt (build-prompt slot-state workflow)
        requires-approval? (get-in slot-state [:meta :approve])]

    ;; Mark as in-progress
    (swap! !workflow io/set-slot-in-progress sym)

    ;; Generate with AI
    (-> (generate ai-client prompt (:meta slot-state))
        (p/then (fn [result]
                  (if requires-approval?
                    ;; Needs approval - set awaiting and callback
                    (do (swap! !workflow io/set-slot-awaiting-approval sym result)
                        (on-approval sym result (:meta slot-state)))
                    ;; Auto-complete
                    (swap! !workflow io/set-slot-value sym result))))
        (p/catch (fn [err]
                   (swap! !workflow io/set-slot-error sym (ex-message err))
                   (on-error sym err))))))

(defn request-human-input!
  "Request human input for a slot"
  [{:keys [!workflow on-human-input]} sym]
  (let [slot-state (get-in @!workflow [:slots sym])]
    (swap! !workflow io/set-slot-in-progress sym)
    (on-human-input sym (:meta slot-state))))

(defn provide-human-input!
  "Provide a human input value for a slot"
  [{:keys [!workflow]} sym value]
  (swap! !workflow io/set-slot-value sym value))

(defn approve!
  "Approve an AI-generated value"
  [{:keys [!workflow]} sym]
  (swap! !workflow io/approve-slot sym))

(defn reject!
  "Reject an AI-generated value"
  [{:keys [!workflow]} sym reason]
  (swap! !workflow io/reject-slot sym reason))

;; =============================================================================
;; WORKFLOW ADVANCEMENT
;; =============================================================================

(defn advance!
  "Advance the workflow by processing the next available slot.
   Returns a promise that resolves when the step completes (or nil if nothing to do)"
  [{:as executor :keys [!workflow on-complete]}]
  (let [workflow @!workflow]
    (cond
      ;; Workflow complete
      (io/workflow-complete? workflow)
      (do (on-complete workflow)
          nil)

      ;; Has slot awaiting approval - wait for human
      (seq (io/awaiting-approval-slots workflow))
      nil

      ;; Has human slot ready - request input
      (when-let [[sym _] (io/next-human-slot workflow)]
        (request-human-input! executor sym)
        :waiting-human)

      ;; Has AI slot ready - execute
      (when-let [[sym _] (io/next-ai-slot workflow)]
        (execute-ai-slot! executor sym))

      ;; Nothing to do (might be blocked or all in-progress)
      :else nil)))

(defn run!
  "Run the workflow until it needs human input or completes.
   Returns a promise that resolves when workflow pauses or completes."
  [{:as executor :keys [!workflow !running?]}]
  (reset! !running? true)
  (p/loop []
    (let [workflow @!workflow]
      (cond
        ;; Complete
        (io/workflow-complete? workflow)
        (do (reset! !running? false)
            :complete)

        ;; Needs human (input or approval)
        (or (seq (io/awaiting-approval-slots workflow))
            (io/next-human-slot workflow))
        (do (advance! executor)  ; Request the input
            (reset! !running? false)
            :waiting-human)

        ;; Can advance with AI
        (io/next-ai-slot workflow)
        (p/do
          (advance! executor)
          ;; Small delay to prevent tight loop
          (p/delay 10)
          (p/recur))

        ;; Blocked (shouldn't happen in well-formed workflow)
        :else
        (do (reset! !running? false)
            :blocked)))))

;; =============================================================================
;; CONVENIENCE API
;; =============================================================================

(defn submit-input!
  "Submit human input and continue running"
  [executor sym value]
  (provide-human-input! executor sym value)
  (run! executor))

(defn submit-approval!
  "Submit approval decision and continue running"
  [executor sym approved? & [rejection-reason]]
  (if approved?
    (approve! executor sym)
    (reject! executor sym rejection-reason))
  (run! executor))

(defn get-current-slot
  "Get the current slot that needs attention"
  [executor]
  (let [workflow @(:!workflow executor)]
    (or (first (io/awaiting-approval-slots workflow))
        (io/next-human-slot workflow))))

(defn get-workflow-summary
  "Get a summary of workflow state"
  [executor]
  (let [workflow @(:!workflow executor)]
    {:id (:id workflow)
     :status (:status workflow)
     :progress (io/workflow-progress workflow)
     :complete? (io/workflow-complete? workflow)
     :current-slot (first (get-current-slot executor))
     :pending (count (io/pending-slots workflow))
     :in-progress (count (io/in-progress-slots workflow))
     :awaiting-approval (count (io/awaiting-approval-slots workflow))
     :complete (count (io/complete-slots workflow))
     :values (:values workflow)}))

;; =============================================================================
;; REACTIVE BINDINGS
;; =============================================================================

(defn reactive-summary
  "Create a reactive summary of executor state"
  [{:keys [!workflow]}]
  (r/reaction
    (let [wf @!workflow]
      {:progress (io/workflow-progress wf)
       :complete? (io/workflow-complete? wf)
       :current (or (first (first (io/awaiting-approval-slots wf)))
                    (first (io/next-human-slot wf))
                    (first (io/next-ai-slot wf)))
       :status (cond
                 (io/workflow-complete? wf) :complete
                 (seq (io/awaiting-approval-slots wf)) :awaiting-approval
                 (io/next-human-slot wf) :awaiting-input
                 (io/next-ai-slot wf) :generating
                 :else :blocked)})))

(defn reactive-slot
  "Create a reactive view of a specific slot"
  [{:keys [!workflow]} sym]
  (r/reaction
    (get-in @!workflow [:slots sym])))

(defn reactive-values
  "Create a reactive map of all completed values"
  [{:keys [!workflow]}]
  (r/reaction (:values @!workflow)))

(comment
  ;; Example usage
  (def !wf (io/make-reactive-workflow
             :demo
             '{:topic ?topic
               :outline ($outline :ai "Create outline for {?topic}" :approve)
               :draft ($draft :ai "Write draft following {$outline}")}))

  (def exec (make-executor {:!workflow !wf}))

  ;; Start running - will pause at ?topic
  (run! exec)

  ;; Provide topic
  (submit-input! exec '?topic "The Future of AI")

  ;; Will generate outline, pause for approval
  ;; ...

  ;; Approve outline
  (submit-approval! exec '$outline true)

  ;; Will generate draft and complete
  )
