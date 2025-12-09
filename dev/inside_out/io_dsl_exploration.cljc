(ns inside-out.io-dsl-exploration
  "Exploration: Extending the ```io DSL for AI Coordination

   The io DSL establishes key principles:
   1. Form structure IS the specification
   2. ? prefix = human input, $ prefix = AI generation
   3. Dependencies implied by reference
   4. String interpolation for prompts
   5. Metadata for generation hints

   This file explores extensions for:
   - Long-running multi-step workflows
   - Approval gates and human checkpoints
   - Transient UIs that emerge from the form
   - Visibility into workflow state")

;; =============================================================================
;; CORE IO DSL SUMMARY
;; =============================================================================
;;
;; Current primitives:
;;
;; ?var           Human input, ask once
;; $var           AI generation, generate once
;; ?              Anonymous human input (each occurrence distinct)
;; $              Anonymous AI value (each occurrence distinct)
;;
;; (?var :ask "Question?")           Prompt for human input
;; ($var :ai "Instructions")         Instructions for AI generation
;;
;; "{?var}"                          String interpolation
;; "{description referencing $var}"  AI-interpreted interpolation
;;
;; [... ($x :take N)]                Sequence of N items
;; [... (?x :take N)]                Ask user for N items
;;
;; Dependencies flow naturally from references.

;; =============================================================================
;; EXTENSION 1: Approval Gates
;; =============================================================================
;;
;; For AI outputs that need human review before downstream use:
;;
;; ```io
;; {:draft    ($draft :ai "Write first draft about ?topic")
;;  :approved ($draft :approve "Is this draft ready to publish?")
;;  :final    (if $approved $draft nil)}
;; ```
;;
;; Or more compactly:
;;
;; ```io
;; {:draft ($draft :ai "..." :approve)
;;  :final $draft}  ; downstream sees approved value
;; ```
;;
;; The :approve metadata means:
;; - Present the generated value to the user
;; - Wait for approval/rejection
;; - On rejection, optionally regenerate or accept feedback

(def approval-example
  '{:plan         ($plan :ai "Create a marketing plan for ?product"
                         :approve "Does this plan look good?")
    :budget       (?budget :ask "What's your budget for implementing $plan?")
    :timeline     ($timeline :ai "Suggest timeline for $plan within $budget")})

;; =============================================================================
;; EXTENSION 2: Revision Loops
;; =============================================================================
;;
;; For iterative refinement:
;;
;; ```io
;; {:draft-v1  $draft
;;  :feedback  (?feedback :on $draft-v1)
;;  :draft-v2  ($draft :revise ?feedback)
;;  :final     $draft-v2}
;; ```
;;
;; Or with explicit revision syntax:
;;
;; ```io
;; {:draft ($content :ai "Write about ?topic"
;;                   :revisions [{:feedback ?f1 :revised $r1}
;;                               {:feedback ?f2 :revised $r2}])
;;  :final $content}
;; ```
;;
;; The workflow automatically:
;; 1. Generates initial $content
;; 2. Shows to user, collects ?f1
;; 3. Revises to $r1
;; 4. Shows $r1, collects ?f2
;; 5. Revises to $r2 (final)

(def revision-example
  '{:essay ($essay :ai "Write essay about ?topic"
                   :max-revisions 3
                   :until :approved)
    :final $essay})

;; =============================================================================
;; EXTENSION 3: Parallel Branches
;; =============================================================================
;;
;; For concurrent work:
;;
;; ```io
;; {:perspectives
;;    {:optimist  ($opt :ai "Optimistic view of ?situation")
;;     :pessimist ($pes :ai "Pessimistic view of ?situation")
;;     :realist   ($real :ai "Realistic view of ?situation")}
;;  :synthesis ($synth :ai "Synthesize $opt, $pes, $real")}
;; ```
;;
;; Or using sequence notation with named generations:
;;
;; ```io
;; {:analyses [... ($analysis :agents [:market :tech :legal] :take 3)]
;;  :summary  ($summary :ai "Combine analyses" :from $analysis)}
;; ```

(def parallel-example
  '{:topic ?topic
    :research {:academic ($academic :ai "Academic perspective on ?topic")
               :industry ($industry :ai "Industry perspective on ?topic")
               :popular  ($popular :ai "Popular media view of ?topic")}
    :synthesis ($synthesis :ai "Synthesize perspectives on ?topic")})

;; =============================================================================
;; EXTENSION 4: Conditional Flows
;; =============================================================================
;;
;; ```io
;; {:user-type  (?type :options [:new :returning])
;;  :onboarding (when (= ?type :new)
;;                {:intro     $intro
;;                 :tutorial  $tutorial})
;;  :dashboard  $dashboard}
;; ```
;;
;; The form shape itself can be conditional on values.
;; Only relevant paths are executed.

(def conditional-example
  '{:project-type (?type :options [:web :mobile :api])
    :spec (case ?type
            :web    {:frontend $frontend-spec :backend $backend-spec}
            :mobile {:platform (?platform :options [:ios :android :both])
                     :spec ($mobile-spec :ai "Spec for ?platform app")}
            :api    {:endpoints $api-spec :auth $auth-spec})
    :timeline ($timeline :ai "Timeline for ?type project")})

;; =============================================================================
;; EXTENSION 5: Transient UI Hints
;; =============================================================================
;;
;; Metadata can suggest how to present inputs/outputs:
;;
;; ```io
;; {:topic   (?topic :ui :text-input :placeholder "Enter research topic")
;;  :sources [... ($source :take 5 :ui :card-grid)]
;;  :selected (?selected :from $sources :ui :multi-select :max 3)
;;  :summary ($summary :ai "Summarize ?selected" :ui :prose)}
;; ```
;;
;; The :ui metadata hints at rendering:
;; - :text-input, :textarea, :select, :multi-select
;; - :card-grid, :list, :table
;; - :prose, :code, :diff
;; - :approval-panel, :rating

(def ui-hints-example
  '{:requirements (?req :ui :textarea
                        :placeholder "Describe what you need...")
    :options      [... ($opt :ai "Design option for ?req"
                             :take 3
                             :ui :card-grid)]
    :selected     (?sel :from $options
                        :ui :single-select)
    :spec         ($spec :ai "Detailed spec for ?sel"
                         :ui :prose
                         :approve)})

;; =============================================================================
;; EXTENSION 6: State Visibility
;; =============================================================================
;;
;; For long-running flows, we need visibility into state.
;; The form structure itself provides this!
;;
;; Given:
;; ```io
;; {:a ?a :b ($b :from ?a) :c ($c :from $b) :d (?d :approve $c)}
;; ```
;;
;; State at any moment:
;;
;; {:a {:status :complete :value "user input"}
;;  :b {:status :generating :progress 0.4}
;;  :c {:status :pending :blocked-by [:b]}
;;  :d {:status :pending :blocked-by [:c]}}
;;
;; This naturally answers:
;; - What's complete?
;; - What's in progress?
;; - What's blocked and by what?
;; - What needs human input?

(defn flow-state [form values]
  ;; Compute the state of each slot in the form
  ;; based on current values and dependencies
  {:complete (keys (filter (comp some? val) values))
   :pending (keys (filter (comp nil? val) values))
   :next-human (first (filter #(and (nil? (values %))
                                    (human-input? %)
                                    (deps-satisfied? % values))
                              (keys form)))
   :next-ai (first (filter #(and (nil? (values %))
                                 (ai-slot? %)
                                 (deps-satisfied? % values))
                           (keys form)))})

;; =============================================================================
;; EXTENSION 7: Meta-Planning
;; =============================================================================
;;
;; The io form can be used to PLAN a workflow, not just execute it:
;;
;; ```io
;; {:goal ?goal
;;  :plan ($plan :ai "Break ?goal into steps, output as io form")
;;  :approved-plan (?approved :approve $plan)
;;  :execution (eval-io $approved-plan)
;;  :result $execution}
;; ```
;;
;; This is recursive - the AI generates an io form, which gets executed!

(def meta-planning-example
  '{:goal ?goal
    :constraints ?constraints
    :plan ($plan :ai "Create an io workflow for achieving ?goal
                      given ?constraints. Output valid io DSL.")
    :approved (?approved :approve $plan
                         :on-reject :regenerate)
    :result (io/execute $approved)})

;; =============================================================================
;; EXAMPLE: Complete Research Assistant
;; =============================================================================

(def research-assistant
  '{;; Phase 1: Topic selection
    :topic (?topic :ui :text-input
                   :ask "What would you like to research?")

    ;; Phase 2: AI-driven exploration (parallel)
    :exploration
    {:papers    [... ($paper :ai "Find academic papers about ?topic"
                             :take 5)]
     :articles  [... ($article :ai "Find news articles about ?topic"
                               :take 3)]
     :experts   [... ($expert :ai "Identify experts on ?topic"
                              :take 3)]}

    ;; Phase 3: Human curation
    :selected-sources (?selected :from [:papers :articles]
                                 :ui :checklist
                                 :ask "Select the most relevant sources")

    ;; Phase 4: AI synthesis with approval
    :summary ($summary :ai "Synthesize key findings from ?selected"
                       :ui :prose
                       :approve "Is this summary accurate?")

    ;; Phase 5: Human-guided deep dive
    :questions [... (?q :ask "What questions remain?"
                        :take 3)]

    :answers [... ($a :ai "Answer ?q based on ?selected"
                      :take (count :questions))]

    ;; Phase 6: Final output
    :report ($report :ai "Compile full report with ?topic, $summary, $answers"
                     :ui :document
                     :approve)})

;; =============================================================================
;; RUNTIME SEMANTICS
;; =============================================================================
;;
;; Executing an io form:
;;
;; 1. Parse the form, identify all ?/$ slots
;; 2. Build dependency graph from references
;; 3. Topological sort to determine order
;; 4. For each slot in order:
;;    - If ?user: present transient UI, wait for input
;;    - If $ai: generate value (using :ai metadata if present)
;;    - If :approve: present approval UI, handle accept/reject
;;    - If conditional: evaluate condition, recurse into relevant branch
;;    - If sequence: repeat child pattern per :take
;; 5. Return completed form with all values filled
;;
;; The "transient UI" emerges naturally:
;; - At any moment, exactly one input is needed
;; - The UI for that input appears
;; - Upon completion, it disappears and next UI appears
;; - Long-running flows are a sequence of transient UIs

;; =============================================================================
;; KEY INSIGHT: Forms as Coordination Language
;; =============================================================================
;;
;; The io DSL is powerful because:
;;
;; 1. DECLARATIVE - describe WHAT you want, not HOW to get it
;; 2. COMPACT - minimal syntax, maximum expressiveness
;; 3. READABLE - reads almost like natural language
;; 4. COMPOSITIONAL - forms can contain forms
;; 5. INSPECTABLE - the form structure reveals dependencies
;; 6. RESUMABLE - state is just "which slots are filled"
;;
;; For AI coordination, this means:
;; - Workflows are DATA, not code
;; - They can be generated, modified, composed by AI
;; - They can be visualized, explained, debugged
;; - They naturally support human-in-the-loop patterns
;; - Transient UIs emerge from the structure itself

)
