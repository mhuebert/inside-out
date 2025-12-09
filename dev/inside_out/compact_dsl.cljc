(ns inside-out.compact-dsl
  "Exploration: Compact, fluid DSL for AI coordination

   Goals:
   - Reads like natural planning language
   - Dependencies implied by reference, not declared
   - Rich semantics from minimal syntax
   - Visibility into state of long-running flows")

;; =============================================================================
;; DESIGN PRINCIPLE: Implied Dependencies
;; =============================================================================
;;
;; In inside-out, dependencies are DISCOVERED from the expression:
;;
;;   {:total (+ ?a ?b)}
;;
;; We never say 'total depends on a and b'. It's implied by usage.
;; This is powerful for AI coordination - we describe WHAT we want,
;; and the runtime figures out the dependency graph.

;; =============================================================================
;; IDEA 1: Natural Language-ish Flow Syntax
;; =============================================================================
;;
;; What if we could write:
;;
;;   (flow product-launch
;;     ?requirements     "User describes what they need"
;;     ?analysis         (claude "Analyze market fit for:" ?requirements)
;;     ?design-options   (claude "Generate 3 design options for:" ?analysis)
;;     ?selected         (human "Choose best design:" ?design-options)
;;     ?detailed-spec    (claude "Elaborate design:" ?selected :with ?requirements)
;;     ?final            (human "Approve for production:" ?detailed-spec))
;;
;; Reading this:
;; - Names describe what each step IS
;; - Strings are prompts/instructions
;; - References (?x) imply dependencies
;; - (human ...) and (claude ...) indicate who/what fills the slot
;; - The whole thing reads almost like a plan

;; Minimal syntax for describing flows
(defn flow-step [name & args]
  ;; Parse: agent, prompt/description, dependencies
  (let [[agent & rest] (if (symbol? (first args))
                         args
                         (cons :human args))
        prompt (first (filter string? rest))
        deps (filter #(and (symbol?) (str/starts-with? (str %) "?")) rest)]
    {:name name
     :agent agent
     :prompt prompt
     :depends-on deps}))

;; =============================================================================
;; IDEA 2: Literate Flow Definition
;; =============================================================================
;;
;; What if the flow definition looked like a markdown plan?
;;
;; ```
;; # Launch New Product
;;
;; ## Inputs
;; - ?requirements: What the user needs (human)
;; - ?constraints: Budget, timeline, etc. (human)
;;
;; ## Analysis Phase
;; - ?market-analysis: Analyze market for ?requirements (claude)
;; - ?competitor-scan: Find competitors for ?requirements (claude, parallel)
;; - ?feasibility: Check ?constraints against ?market-analysis (claude)
;;
;; ## Design Phase
;; - ?options: Generate designs given ?feasibility (claude)
;; - ?selected: User chooses from ?options (human, approval)
;;
;; ## Output
;; - ?plan: Combine ?selected, ?market-analysis, ?feasibility (computed)
;; ```
;;
;; This is readable as a plan AND parseable as a flow definition.
;; The syntax hints tell us:
;; - (human) vs (claude) - who fills the slot
;; - (parallel) - can run concurrently with siblings
;; - (approval) - requires explicit human sign-off
;; - (computed) - derived from other slots

(def flow-dsl-example
  "# Product Launch

   ?requirements   | human    | What does the user need?
   ?analysis       | claude   | Analyze market for ?requirements
   ?options        | claude   | Generate 3 options from ?analysis
   ?selected       | human    | Choose from ?options | approval
   ?spec           | claude   | Detail ?selected given ?requirements
   ?final          | human    | Review ?spec | approval")

(defn parse-flow-line [line]
  ;; Parse: ?name | agent | description | flags
  ;; Returns nil for non-step lines
  (when-let [[_ name agent desc flags]
             (re-matches #"\s*(\?[\w-]+)\s*\|\s*(\w+)\s*\|\s*([^|]+)(?:\|\s*(.+))?" line)]
    {:name (symbol name)
     :agent (keyword agent)
     :description (str/trim desc)
     :flags (when flags (set (map (comp keyword str/trim) (str/split flags #","))))
     :deps (->> (re-seq #"\?[\w-]+" desc)
                (map symbol))}))

(comment
  (parse-flow-line "   ?analysis       | claude   | Analyze market for ?requirements")
  ;; => {:name ?analysis, :agent :claude, :description "Analyze market for ?requirements",
  ;;     :flags nil, :deps [?requirements]}
  )

;; =============================================================================
;; IDEA 3: Plan as Nested Structure
;; =============================================================================
;;
;; Hierarchical plans with implicit sequencing:
;;
;; (plan launch-product
;;   (phase :discovery
;;     ?requirements (ask "What do you need?")
;;     ?constraints  (ask "What are your constraints?"))
;;
;;   (phase :analysis                          ; After discovery
;;     (parallel
;;       ?market     (ai "Analyze market" ?requirements)
;;       ?competitors (ai "Find competitors" ?requirements)))
;;
;;   (phase :design                            ; After analysis
;;     ?options    (ai "Generate options" ?market ?competitors)
;;     ?selected   (approve ?options))         ; Human approval gate
;;
;;   (phase :delivery
;;     ?spec       (ai "Detailed spec" ?selected ?requirements)
;;     ?final      (approve ?spec)))           ; Final sign-off
;;
;; This reads as:
;; 1. Phases run sequentially
;; 2. Steps within a phase can specify (parallel) or default to sequential
;; 3. (approve x) means human reviews x before continuing
;; 4. The whole plan is the "shape" of data being produced

(defn phase [name & steps]
  {:type :phase
   :name name
   :steps (vec steps)})

(defn parallel [& steps]
  {:type :parallel
   :steps (vec steps)})

(defn ask [prompt]
  {:type :human-input
   :prompt prompt})

(defn ai [prompt & deps]
  {:type :ai-call
   :prompt prompt
   :deps (vec deps)})

(defn approve [target]
  {:type :approval
   :target target})

;; =============================================================================
;; IDEA 4: Visibility into Long-Running Flows
;; =============================================================================
;;
;; For long-running flows, we need:
;;
;; 1. STATE SUMMARY - at a glance, where are we?
;;
;;    launch-product: [=====>    ] 60%
;;    ├─ discovery:   [==========] complete
;;    ├─ analysis:    [==========] complete
;;    ├─ design:      [====>     ] awaiting approval
;;    │  └─ ?options: generated, ?selected: pending
;;    └─ delivery:    [          ] blocked
;;
;; 2. DEPENDENCY GRAPH - what's blocking what?
;;
;;    ?selected (pending)
;;      └── blocks → ?spec, ?final
;;
;; 3. HISTORY - what happened so far?
;;
;;    14:32 ?requirements set by user
;;    14:33 ?market generated by claude (took 2.3s)
;;    14:33 ?competitors generated by claude (took 1.8s)
;;    14:35 ?options generated by claude (took 4.1s)
;;    14:35 ?options awaiting approval...
;;
;; 4. PROJECTION - what's coming next?
;;
;;    Next: User approves ?options
;;    Then: Claude generates ?spec
;;    Then: User approves ?final
;;    ETA: ~5 min remaining

(defn flow-summary [flow]
  {:total-steps (count (:steps flow))
   :completed (count (filter :completed (:steps flow)))
   :pending (filter :pending (:steps flow))
   :blocked (filter :blocked (:steps flow))
   :current (first (filter #(= :in-progress (:status %)) (:steps flow)))})

(defn blocking-graph [flow]
  ;; Returns a map of {blocked-step => set of blocking-steps}
  {})

(defn flow-history [flow]
  ;; Returns seq of {:timestamp, :step, :action, :actor, :duration}
  [])

(defn flow-projection [flow]
  ;; Returns seq of predicted next steps with time estimates
  [])

;; =============================================================================
;; IDEA 5: Interactive Planning
;; =============================================================================
;;
;; The plan itself can be the subject of human-AI collaboration:
;;
;; (plan-with-ai project-x
;;   :initial-goal "Build a mobile app for tracking habits"
;;
;;   ;; AI proposes initial plan structure
;;   ?plan-draft (ai "Break this goal into phases and steps")
;;
;;   ;; Human reviews and adjusts
;;   ?plan (review ?plan-draft
;;           :operations [:add-step :remove-step :reorder :modify])
;;
;;   ;; Plan execution (each step in ?plan becomes a slot)
;;   ?results (execute ?plan)
;;
;;   ;; Reflect and improve
;;   ?retrospective (ai "What could we improve?" ?results)
;;   ?updated-plan (incorporate ?plan ?retrospective))
;;
;; Here the PLAN is a first-class object that can be:
;; - Generated by AI
;; - Reviewed/modified by humans
;; - Executed step by step
;; - Reflected upon and improved

(defn dynamic-plan
  "A plan whose structure can evolve during execution"
  [initial-goal]
  {:goal initial-goal
   :versions []        ; History of plan versions
   :current nil        ; Current plan version
   :execution nil})    ; Execution state

;; =============================================================================
;; SYNTHESIS: Combining Ideas
;; =============================================================================
;;
;; A compact DSL might look like:
;;
;; (flow research-paper
;;
;;   ;; Declarations read like prose
;;   :topic          (from-user "Research topic")
;;   :literature     (ai-search "Find papers about" :topic)
;;   :key-papers     (user-select :literature :max 10)
;;   :summaries      (ai-map :key-papers "Summarize this paper")
;;   :gaps           (ai "Identify research gaps in" :summaries)
;;   :thesis         (user-compose "Your thesis based on" :gaps)
;;
;;   ;; Approval gate
;;   :thesis-approved (user-approve :thesis)
;;
;;   ;; Parallel section drafting
;;   :sections       (parallel
;;                     :intro    (ai "Write intro for" :thesis :given :summaries)
;;                     :methods  (from-user "Describe your methods")
;;                     :results  (from-user "Describe your results")
;;                     :discuss  (ai "Discuss" :results :in-context :literature))
;;
;;   ;; Assembly
;;   :draft          (ai "Assemble paper from" :sections)
;;   :reviewed       (user-edit :draft)
;;   :final          (user-approve :reviewed))
;;
;; Key insights:
;; 1. Keywords (:topic) instead of symbols (?topic) - feels more declarative
;; 2. Verbs describe the operation: from-user, ai, ai-search, ai-map, user-select
;; 3. Dependencies are positional arguments - reads naturally
;; 4. parallel explicitly marks concurrent work
;; 5. user-approve creates gates
;; 6. The whole flow is data - can be serialized, resumed, visualized

)
