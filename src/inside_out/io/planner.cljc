(ns inside-out.io.planner
  "Meta-planning: AI generates io workflow plans

   Flow:
   1. User describes goal/scenario
   2. AI generates an io workflow plan
   3. User reviews and optionally edits
   4. Plan is parsed and executed
   5. Results returned

   This creates a recursive pattern where AI coordinates
   its own collaboration with humans."
  (:require [clojure.string :as str]
            [inside-out.io :as io]
            [inside-out.io.executor :as exec]
            [promesa.core :as p]
            [re-db.reactive :as r]))

;; =============================================================================
;; IO DSL REFERENCE (for AI prompt)
;; =============================================================================

(def io-dsl-reference
  "# io DSL Reference

The io DSL defines human-AI collaborative workflows. A workflow is a Clojure map
where values can be **slots** that get filled during execution.

## Slot Types

- `?name` - Human input (user provides this value)
- `$name` - AI generation (AI generates this value)

## Slot Metadata

Slots can have metadata in a list form:

```clojure
(?topic :ask \"What topic interests you?\")     ; Prompt shown to user
($draft :ai \"Write about {?topic}\")           ; Instructions for AI
($plan :ai \"Create plan\" :approve)            ; Requires human approval
```

## String Interpolation

Reference other slots in prompts using `{slot-name}`:

```clojure
($summary :ai \"Summarize {?document}\")
($refined :ai \"Improve {$draft} based on {?feedback}\")
```

## Dependencies

Dependencies are **implicit** - if a prompt references `{?topic}`, that slot
depends on `?topic`. Execution order is determined automatically.

## Key Metadata Options

- `:ask \"prompt\"` - Question/prompt for human input
- `:ai \"instructions\"` - Instructions for AI generation
- `:approve` - AI output requires human approval before continuing
- `:ui :textarea` - Use textarea instead of text input (for humans)
- `:ui :select` - Use dropdown with `:options [...]`

## Patterns

### Simple linear flow:
```clojure
{:topic ?topic
 :outline ($outline :ai \"Create outline for {?topic}\")
 :draft ($draft :ai \"Write based on {$outline}\")}
```

### With approval gate:
```clojure
{:idea ?idea
 :plan ($plan :ai \"Plan for {?idea}\" :approve)
 :execution ($result :ai \"Execute {$plan}\")}
```

### With human feedback loop:
```clojure
{:topic ?topic
 :draft ($draft :ai \"First draft on {?topic}\")
 :feedback (?feedback :ask \"How should we improve {$draft}?\")
 :final ($final :ai \"Revise {$draft} based on {?feedback}\")}
```

### Multiple human inputs:
```clojure
{:goal (?goal :ask \"What's your goal?\")
 :constraints (?constraints :ask \"What constraints exist?\" :ui :textarea)
 :timeline (?timeline :ask \"What's your timeline?\" :ui :select
            :options [\"1 week\" \"1 month\" \"3 months\"])
 :plan ($plan :ai \"Create plan for {?goal} given {?constraints} in {?timeline}\")}
```

## Guidelines

1. Start with human input to understand the task
2. Use approval gates (`:approve`) for important AI decisions
3. Include feedback loops for iterative refinement
4. Keep slot names descriptive (kebab-case with ? or $ prefix)
5. Write clear prompts that reference needed context
6. The workflow output is the entire map with all slots filled
")

;; =============================================================================
;; PLAN GENERATION PROMPT
;; =============================================================================

(defn make-planner-prompt [goal context]
  (str io-dsl-reference
       "\n\n---\n\n"
       "# Your Task\n\n"
       "Generate an io workflow plan for the following goal:\n\n"
       "**Goal:** " goal "\n\n"
       (when context
         (str "**Additional Context:** " context "\n\n"))
       "## Requirements\n\n"
       "1. Output ONLY a valid Clojure map (the io form)\n"
       "2. Start with human input slots to gather necessary information\n"
       "3. Use AI slots for analysis, generation, and synthesis\n"
       "4. Add `:approve` to AI slots that make important decisions\n"
       "5. Include at least one feedback loop if the task involves creative work\n"
       "6. Use descriptive slot names that explain their purpose\n"
       "7. Write clear, specific prompts for each AI slot\n\n"
       "Output the io form now (just the Clojure map, no explanation):"))

;; =============================================================================
;; PLAN PARSING AND VALIDATION
;; =============================================================================

(defn parse-plan
  "Parse an io form from a string (AI response)"
  [s]
  (try
    (let [;; Extract just the map if there's surrounding text
          form-match (re-find #"\{[^{}]*(?:\{[^{}]*\}[^{}]*)*\}" s)
          form-str (or form-match s)
          form (cljs.reader/read-string form-str)]
      (if (map? form)
        {:ok true :form form}
        {:ok false :error "Response is not a map" :raw s}))
    (catch js/Error e
      {:ok false :error (.-message e) :raw s})))

(defn validate-plan
  "Validate an io form for common issues"
  [{:keys [ok form] :as result}]
  (if-not ok
    result
    (let [slots (io/extract-slots form)
          human-slots (filter #(io/human-slot? (key %)) slots)
          ai-slots (filter #(io/ai-slot? (key %)) slots)
          issues (cond-> []
                   (empty? human-slots)
                   (conj "Plan should include at least one human input slot (?)")

                   (empty? ai-slots)
                   (conj "Plan should include at least one AI slot ($)")

                   ;; Check for orphan slots (referenced but not defined)
                   ;; This is a simple check - could be more thorough
                   )]
      (if (empty? issues)
        (assoc result :valid true :slots slots)
        (assoc result :valid false :issues issues)))))

;; =============================================================================
;; PLAN GENERATOR
;; =============================================================================

(defprotocol IPlanGenerator
  "Protocol for generating io plans from goals"
  (generate-plan [this goal opts]
    "Generate an io workflow plan. Returns promise of {:ok, :form} or {:ok false, :error}"))

(defrecord MockPlanGenerator []
  IPlanGenerator
  (generate-plan [_ goal opts]
    (p/do
      (p/delay 1500)
      ;; Generate a reasonable mock plan based on goal keywords
      (let [form (cond
                   (str/includes? (str/lower-case goal) "research")
                   '{:topic (?topic :ask "What topic would you like to research?")
                     :scope (?scope :ask "How deep should the research go?"
                                    :ui :select
                                    :options ["Quick overview" "Moderate depth" "Comprehensive"])
                     :sources ($sources :ai "Find key sources about {?topic} at {?scope} level" :approve)
                     :synthesis ($synthesis :ai "Synthesize findings from {$sources}")
                     :questions (?questions :ask "What follow-up questions do you have?")
                     :answers ($answers :ai "Answer {?questions} based on {$sources}")
                     :report ($report :ai "Compile final research report on {?topic}")}

                   (str/includes? (str/lower-case goal) "write")
                   '{:topic (?topic :ask "What would you like to write about?")
                     :audience (?audience :ask "Who is your target audience?")
                     :tone (?tone :ask "What tone should the writing have?"
                                  :ui :select
                                  :options ["Professional" "Casual" "Academic" "Creative"])
                     :outline ($outline :ai "Create outline for {?topic} targeting {?audience}" :approve)
                     :draft ($draft :ai "Write {?tone} first draft following {$outline}" :approve)
                     :feedback (?feedback :ask "What improvements would you suggest?" :ui :textarea)
                     :revision ($revision :ai "Revise {$draft} incorporating: {?feedback}")
                     :final ($final :ai "Polish and finalize {$revision}")}

                   (str/includes? (str/lower-case goal) "plan")
                   '{:objective (?objective :ask "What are you trying to accomplish?")
                     :timeline (?timeline :ask "What's your timeline?")
                     :resources (?resources :ask "What resources do you have available?" :ui :textarea)
                     :constraints (?constraints :ask "What constraints should we consider?" :ui :textarea)
                     :analysis ($analysis :ai "Analyze feasibility of {?objective} given {?resources} and {?constraints}")
                     :strategy ($strategy :ai "Develop strategy for {?objective} within {?timeline}" :approve)
                     :milestones ($milestones :ai "Break {$strategy} into concrete milestones")
                     :risks ($risks :ai "Identify risks in {$strategy} and mitigation strategies")
                     :final-plan ($final-plan :ai "Compile complete plan with {$strategy}, {$milestones}, {$risks}")}

                   (str/includes? (str/lower-case goal) "brainstorm")
                   '{:challenge (?challenge :ask "What problem or challenge are you facing?")
                     :context (?context :ask "What's the context? Any constraints?" :ui :textarea)
                     :initial-ideas ($ideas :ai "Generate 10 diverse ideas for {?challenge} considering {?context}" :approve)
                     :favorite (?favorite :ask "Which ideas resonate most? (list numbers)")
                     :deep-dive ($deep-dive :ai "Elaborate on ideas {?favorite} from {$ideas}")
                     :concerns (?concerns :ask "What concerns do you have about these ideas?")
                     :refined ($refined :ai "Address {?concerns} and refine {$deep-dive}")
                     :action-plan ($action-plan :ai "Create action plan for implementing {$refined}")}

                   (str/includes? (str/lower-case goal) "analyze")
                   '{:subject (?subject :ask "What would you like to analyze?")
                     :focus (?focus :ask "What aspects are most important to examine?")
                     :data (?data :ask "Any specific data or context to consider?" :ui :textarea)
                     :framework ($framework :ai "Determine best analytical framework for {?subject} focusing on {?focus}")
                     :analysis ($analysis :ai "Analyze {?subject} using {$framework} with data: {?data}" :approve)
                     :insights ($insights :ai "Extract key insights from {$analysis}")
                     :implications (?interpretation :ask "What's your interpretation of these insights?")
                     :recommendations ($recommendations :ai "Generate recommendations based on {$insights} and {?interpretation}")}

                   :else
                   '{:goal (?goal :ask "Please describe your goal in more detail")
                     :context (?context :ask "What context or constraints should we consider?" :ui :textarea)
                     :approach ($approach :ai "Determine the best approach for {?goal} given {?context}" :approve)
                     :plan ($plan :ai "Create detailed plan following {$approach}")
                     :feedback (?feedback :ask "Any adjustments needed to this plan?")
                     :final ($final :ai "Finalize plan incorporating {?feedback}")
                     :next-steps ($next-steps :ai "Outline immediate next steps from {$final}")})]
        {:ok true :form form}))))

;; Real AI plan generator (requires actual AI client)
(defrecord AIPlanGenerator [ai-client]
  IPlanGenerator
  (generate-plan [_ goal opts]
    (let [prompt (make-planner-prompt goal (:context opts))]
      (-> (exec/generate ai-client prompt {})
          (p/then parse-plan)
          (p/then validate-plan)
          (p/catch (fn [e] {:ok false :error (ex-message e)}))))))

(defn make-mock-planner []
  (->MockPlanGenerator))

(defn make-ai-planner [ai-client]
  (->AIPlanGenerator ai-client))

;; =============================================================================
;; META-WORKFLOW STATE
;; =============================================================================

(defrecord PlanningSession
  [id              ; Session ID
   goal            ; User's original goal
   context         ; Additional context
   status          ; :planning :reviewing :editing :executing :complete
   generated-plan  ; The AI-generated plan {:ok :form}
   edited-plan     ; User's edited version (if any)
   final-plan      ; The plan being/was executed
   !workflow       ; Reactive workflow state (when executing)
   executor        ; Executor instance (when executing)
   result])        ; Final result (when complete)

(defn make-planning-session [id goal context]
  (map->PlanningSession
    {:id id
     :goal goal
     :context context
     :status :planning
     :generated-plan nil
     :edited-plan nil
     :final-plan nil
     :!workflow nil
     :executor nil
     :result nil}))

;; =============================================================================
;; PLANNING SESSION OPERATIONS
;; =============================================================================

(defn generate-plan!
  "Generate a plan for the session's goal"
  [!session planner]
  (let [{:keys [goal context]} @!session]
    (swap! !session assoc :status :planning)
    (-> (generate-plan planner goal {:context context})
        (p/then (fn [result]
                  (swap! !session assoc
                         :generated-plan result
                         :status (if (:ok result) :reviewing :error))
                  result)))))

(defn approve-plan!
  "Approve the plan and prepare for execution"
  [!session]
  (let [{:keys [generated-plan edited-plan]} @!session
        plan (or edited-plan generated-plan)]
    (when (:ok plan)
      (swap! !session assoc
             :final-plan (:form plan)
             :status :ready))))

(defn edit-plan!
  "Update the edited plan"
  [!session form-string]
  (let [result (-> form-string parse-plan validate-plan)]
    (swap! !session assoc :edited-plan result)
    result))

(defn execute-plan!
  "Execute the approved plan"
  [!session ai-client]
  (let [{:keys [id final-plan]} @!session
        !workflow (io/make-reactive-workflow (str id "-exec") final-plan)
        executor (exec/make-executor {:!workflow !workflow
                                      :ai-client ai-client})]
    (swap! !session assoc
           :status :executing
           :!workflow !workflow
           :executor executor)
    (exec/run! executor)))

(defn get-result
  "Get the final result when workflow is complete"
  [!session]
  (when-let [!workflow (:!workflow @!session)]
    (when (io/workflow-complete? @!workflow)
      (:values @!workflow))))

;; =============================================================================
;; REACTIVE BINDINGS
;; =============================================================================

(defn reactive-session-status [!session]
  (r/reaction (:status @!session)))

(defn reactive-generated-plan [!session]
  (r/reaction (:generated-plan @!session)))

(defn reactive-workflow-progress [!session]
  (r/reaction
    (when-let [!wf (:!workflow @!session)]
      (io/workflow-progress @!wf))))

;; =============================================================================
;; CONVENIENCE: ALL-IN-ONE PLANNER WORKFLOW
;; =============================================================================

(defn plan-and-execute!
  "Complete flow: generate plan, get approval, execute
   Returns a promise that resolves when complete"
  [{:keys [goal context planner ai-client on-plan-ready on-approval on-complete]}]
  (let [!session (r/atom (make-planning-session (str "session-" (random-uuid)) goal context))]
    ;; Generate plan
    (-> (generate-plan! !session planner)
        (p/then (fn [plan]
                  (when (and (:ok plan) on-plan-ready)
                    (on-plan-ready plan !session))
                  plan))
        (p/then (fn [plan]
                  (when (:ok plan)
                    ;; Wait for approval (this would normally be async via UI)
                    (if on-approval
                      (on-approval !session)
                      (approve-plan! !session)))))
        (p/then (fn [_]
                  (when (= :ready (:status @!session))
                    (execute-plan! !session ai-client))))
        (p/then (fn [_]
                  ;; Wait for completion
                  (p/loop []
                    (if (and (:!workflow @!session)
                             (io/workflow-complete? @(:!workflow @!session)))
                      (let [result (get-result !session)]
                        (swap! !session assoc :status :complete :result result)
                        (when on-complete (on-complete result))
                        result)
                      (p/do (p/delay 100)
                            (p/recur)))))))))

(comment
  ;; Example usage
  (def planner (make-mock-planner))

  (def !session (r/atom (make-planning-session "test" "Help me write a blog post" nil)))

  (generate-plan! !session planner)

  @!session
  )
