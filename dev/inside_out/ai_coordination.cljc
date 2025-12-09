(ns inside-out.ai-coordination
  "Exploration: Inside-Out as a DSL for AI Coordination

   This namespace explores how inside-out's reactive form primitives
   could be extended/reimagined for coordinating human-AI workflows
   with transient UIs."
  (:require [inside-out.forms :as forms]
            [re-db.reactive :as r]
            [promesa.core :as p]))

;; =============================================================================
;; CORE INSIGHT: Fields as Workflow Slots
;; =============================================================================
;;
;; In forms: ?name is a slot for user input
;; In AI coordination: ?step is a slot that can be:
;;   - Filled by human input
;;   - Computed by an AI agent
;;   - Derived from other slots
;;   - Awaiting approval before downstream use
;;
;; The reactive graph handles dependencies automatically.

;; =============================================================================
;; PRIMITIVE 1: Input Sources
;; =============================================================================
;;
;; A slot can declare where its value comes from:
;;
;; (:source ?field) => :human | :ai | :computed | :external
;;
;; Examples:
;;   (?requirements :source :human)           ; User provides this
;;   (?analysis :source :ai :model :claude)   ; AI generates this
;;   (?summary :source :computed)             ; Derived from other slots
;;   (?data :source :external :fetch #(...))  ; From external API

(defn human-input
  "Creates a field that expects human input, with optional transient UI spec"
  [meta]
  (assoc meta :source :human))

(defn ai-step
  "Creates a field that will be filled by an AI agent"
  [{:keys [model prompt input-fields output-schema] :as meta}]
  (assoc meta
         :source :ai
         :compute-fn (fn [bindings]
                       ;; Placeholder for actual AI invocation
                       ;; Would call the model with prompt template + bindings
                       {:status :pending
                        :model model
                        :prompt prompt
                        :inputs (select-keys bindings input-fields)})))

;; =============================================================================
;; PRIMITIVE 2: Approval Gates
;; =============================================================================
;;
;; A critical pattern: AI output requires human approval before use.
;; This creates a natural pause point with a transient UI.
;;
;; (?analysis :requires-approval true
;;            :approval-ui [...])
;;
;; The downstream slots see the value only after approval.
;; Rejection can trigger re-generation or alternative paths.

(defn requires-approval? [field]
  (:requires-approval field))

(defn approval-status [field]
  ;; :pending | :approved | :rejected
  (:approval-status @(forms/!meta field)))

(defn approve! [field]
  (swap! (forms/!meta field) assoc :approval-status :approved))

(defn reject! [field reason]
  (swap! (forms/!meta field) assoc
         :approval-status :rejected
         :rejection-reason reason))

;; For downstream computation, we need to know if a value is "ready"
(defn ready? [field]
  (if (requires-approval? field)
    (= :approved (approval-status field))
    (some? @field)))

;; =============================================================================
;; PRIMITIVE 3: Transient UIs
;; =============================================================================
;;
;; Each slot can specify a UI that appears when interaction is needed.
;; These UIs are "transient" - they appear, serve their purpose, then disappear.
;;
;; (?feedback :source :human
;;            :transient-ui {:type :inline-edit
;;                           :context ?ai-output
;;                           :prompt "Provide feedback on this draft"})
;;
;; UI types might include:
;;   :text-input, :textarea, :select, :multi-select
;;   :approval-panel (approve/reject with optional feedback)
;;   :diff-view (compare versions)
;;   :inline-edit (edit in context)
;;   :rating (thumbs up/down, stars, etc.)

(def transient-ui-types
  {:text-input    {:render 'ui/text-input}
   :textarea      {:render 'ui/textarea}
   :approval      {:render 'ui/approval-panel
                   :actions [:approve :reject :request-revision]}
   :select        {:render 'ui/select}
   :diff          {:render 'ui/diff-view}
   :inline-edit   {:render 'ui/inline-edit}
   :rating        {:render 'ui/rating}
   :checklist     {:render 'ui/checklist}})

(defn transient-ui-spec [field]
  (:transient-ui field))

(defn needs-ui? [field]
  "Returns true if field currently needs user interaction"
  (and (transient-ui-spec field)
       (case (:source field)
         :human (nil? @field)
         :ai (and (requires-approval? field)
                  (= :pending (approval-status field)))
         false)))

;; =============================================================================
;; PRIMITIVE 4: Workflow as Computed Form
;; =============================================================================
;;
;; A workflow IS a form - its output is a computed expression.
;; The difference is in the metadata and how slots get filled.
;;
;; (workflow ?product-launch
;;   {:requirements ?requirements
;;    :market-analysis ?analysis
;;    :go-to-market ?gtm-plan
;;    :final-decision ?decision}
;;
;;   :slots {?requirements {:source :human
;;                          :transient-ui {:type :textarea}}
;;           ?analysis {:source :ai
;;                      :model :claude
;;                      :prompt "Analyze market for: {?requirements}"
;;                      :requires-approval true}
;;           ?gtm-plan {:source :ai
;;                      :depends-on [?requirements ?analysis]
;;                      :prompt "Create GTM plan given: ..."}
;;           ?decision {:source :human
;;                      :transient-ui {:type :approval
;;                                     :context [?analysis ?gtm-plan]}}})

;; Workflow state is just form state with additional coordination metadata
(defn workflow-status [workflow]
  (let [slots (forms/descendants workflow)]
    {:total (count slots)
     :completed (count (filter ready? slots))
     :pending-human (count (filter #(and (= :human (:source %))
                                         (not (ready? %))) slots))
     :pending-ai (count (filter #(and (= :ai (:source %))
                                      (not (ready? %))) slots))
     :pending-approval (count (filter #(and (requires-approval? %)
                                            (= :pending (approval-status %))) slots))}))

;; =============================================================================
;; PRIMITIVE 5: Parallel and Iterative Patterns
;; =============================================================================
;;
;; :many fields naturally express parallel work and iteration.
;;
;; Parallel agents:
;;   (?drafts :many {:draft ?draft :model ?model}
;;            :init (for [m [:claude :gpt :gemini]]
;;                    {'?model m}))
;;
;; Iterative refinement:
;;   (?revisions :many {:content ?content :feedback ?feedback}
;;               :add-on :rejection)
;;
;; Map-reduce:
;;   (?analyses :many {:chunk ?chunk :analysis ?analysis})
;;   (?synthesis :compute (synthesize ?analyses))

(defn parallel-agents
  "Creates slots for multiple agents working on the same task"
  [models task-meta]
  {:many true
   :child-meta (assoc task-meta :source :ai)
   :init (mapv (fn [m] {'?model m}) models)})

(defn iterative-refinement
  "Creates a structure for iterative human-AI refinement"
  [initial-meta]
  {:many true
   :child-meta {:content {:source :ai}
                :feedback {:source :human
                           :transient-ui {:type :inline-edit}}}
   :init [{'?content initial-meta}]
   :add-on-rejection true})

;; =============================================================================
;; PRIMITIVE 6: Conditional Flows
;; =============================================================================
;;
;; Workflows often branch based on intermediate results.
;; Inside-out already supports this via computed expressions:
;;
;; (workflow
;;   (case (:risk-level @?analysis)
;;     :high {:needs-review true
;;            :reviewer ?senior-reviewer
;;            :final ?reviewed-decision}
;;     :low {:needs-review false
;;           :final ?auto-decision}))

(defn when-ready
  "Wraps a computation to only proceed when dependencies are ready"
  [deps compute-fn]
  (fn [bindings]
    (when (every? #(ready? (get bindings %)) deps)
      (compute-fn bindings))))

;; =============================================================================
;; EXAMPLE: Document Review Workflow
;; =============================================================================

(comment
  ;; A workflow for AI-assisted document review

  (def review-workflow-schema
    '{:document ?document             ; Human uploads document
      :ai-summary ?summary            ; AI summarizes
      :ai-issues ?issues              ; AI identifies issues
      :human-review ?review           ; Human reviews AI findings
      :final-decision ?decision})     ; Human makes final call

  (def review-workflow-meta
    '{?document {:source :human
                 :transient-ui {:type :file-upload
                                :accept [".pdf" ".docx"]}}

      ?summary {:source :ai
                :model :claude
                :prompt "Summarize this document: {?document}"
                :requires-approval false}  ; Auto-proceed

      ?issues {:source :ai
               :model :claude
               :prompt "Identify potential issues in: {?document}"
               :output-schema [{:issue ?issue-text
                                :severity ?severity
                                :location ?location}]
               :requires-approval true
               :transient-ui {:type :approval
                              :show-context ?summary}}

      ?review {:source :human
               :depends-on [?issues]
               :transient-ui {:type :checklist
                              :items ?issues
                              :actions [:acknowledge :dismiss :escalate]}}

      ?decision {:source :human
                 :depends-on [?review]
                 :transient-ui {:type :select
                                :options [:approve :reject :revise]}}})

  ;; The workflow "runs" by:
  ;; 1. Presenting ?document UI (file upload)
  ;; 2. On upload, triggering AI for ?summary and ?issues
  ;; 3. Presenting approval UI for ?issues
  ;; 4. On approval, presenting ?review checklist
  ;; 5. On completion, presenting ?decision select
  ;; 6. Returning final computed form value
  )

;; =============================================================================
;; EXAMPLE: Multi-Agent Collaboration
;; =============================================================================

(comment
  ;; Multiple AI agents with different perspectives

  '{:topic ?topic
    :perspectives (?perspectives :many {:agent ?agent
                                        :analysis ?analysis
                                        :confidence ?confidence}
                                 :init [{'?agent :optimist}
                                        {'?agent :pessimist}
                                        {'?agent :pragmatist}])
    :synthesis ?synthesis
    :human-choice ?choice}

  ;; Each perspective runs in parallel with agent-specific prompting
  ;; Synthesis combines them
  ;; Human makes final choice
  )

;; =============================================================================
;; RUNTIME: Workflow Executor
;; =============================================================================
;;
;; The executor manages the lifecycle:
;; 1. Identify slots that are ready to compute
;; 2. For :human slots, render transient UI
;; 3. For :ai slots, invoke the model
;; 4. For :computed slots, run the computation
;; 5. Handle approval gates
;; 6. Continue until workflow is complete or blocked

(defn next-actions [workflow]
  "Returns the set of slots that can progress"
  (let [all-slots (forms/descendants workflow)]
    (->> all-slots
         (remove ready?)
         (filter (fn [slot]
                   (let [deps (or (:depends-on slot) [])]
                     (every? #(ready? (get workflow %)) deps))))
         (group-by :source))))

(defn pending-uis [workflow]
  "Returns slots that need human interaction"
  (->> (next-actions workflow)
       :human
       (filter needs-ui?)))

(defn pending-ai-calls [workflow]
  "Returns slots that need AI invocation"
  (:ai (next-actions workflow)))

;; =============================================================================
;; DESIGN QUESTIONS TO EXPLORE
;; =============================================================================
;;
;; 1. How to handle streaming AI responses?
;;    - Field value updates incrementally
;;    - UI shows partial results
;;    - Approval can happen mid-stream?
;;
;; 2. How to handle errors and retries?
;;    - AI call fails -> retry policy in metadata
;;    - Validation fails -> show in transient UI
;;    - Human abandons -> cleanup hooks
;;
;; 3. How to persist workflow state?
;;    - Serialize field values and metadata
;;    - Resume from checkpoint
;;    - Handle long-running workflows (days/weeks)
;;
;; 4. How to compose workflows?
;;    - Sub-workflows as :many fields?
;;    - Workflow as a reusable "component"
;;    - Nested approval chains
;;
;; 5. How to handle concurrent human editors?
;;    - Optimistic updates
;;    - Conflict resolution
;;    - Real-time collaboration on transient UIs
;;
;; 6. How to express complex routing?
;;    - Switch/case based on field values (already supported)
;;    - Loops and iteration
;;    - Exception handling paths
;;
;; 7. How to debug and inspect workflows?
;;    - Visualize the dependency graph
;;    - Step through execution
;;    - Replay from any state

)
