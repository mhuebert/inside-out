(ns inside-out.ai-workflow-demo
  "Concrete demo: AI-assisted writing workflow with transient UIs

   This demonstrates how inside-out's existing primitives can orchestrate
   a human-AI collaborative writing task."
  (:require [clojure.string :as str]
            [inside-out.forms :as forms]
            [promesa.core :as p]
            [re-db.reactive :as r]
            #?(:cljs [reagent.core :as reagent])))

;; =============================================================================
;; MOCK AI - Replace with real AI client
;; =============================================================================

(defn mock-ai-call
  "Simulates an AI call with delay"
  [{:keys [task input]}]
  (p/do
    (p/delay 800)  ; Simulate latency
    (case task
      :expand-outline
      [{:title "Introduction" :notes "Hook the reader, state thesis"}
       {:title "Background" :notes "Context and prior work"}
       {:title "Main Argument" :notes "Core claims with evidence"}
       {:title "Counterarguments" :notes "Address objections"}
       {:title "Conclusion" :notes "Synthesize and call to action"}]

      :draft-section
      (str "## " (:title input) "\n\n"
           "This section covers " (str/lower-case (:title input)) ". "
           "Key points include: " (:notes input) ". "
           "[AI-generated draft content would go here...]\n\n"
           "Lorem ipsum dolor sit amet, consectetur adipiscing elit. "
           "Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.")

      :improve-draft
      (str (:draft input) "\n\n"
           "---\n"
           "_Improvements based on feedback: " (:feedback input) "_\n"
           "[Enhanced content would appear here...]")

      ;; Default
      {:result "Unknown task"})))

;; =============================================================================
;; WORKFLOW STATE HELPERS
;; =============================================================================

(defn slot-status
  "Returns the status of a workflow slot"
  [field]
  (cond
    (nil? @field) :empty
    (:loading? field) :loading
    (and (:requires-approval field)
         (not (:approved @(forms/!meta field)))) :pending-approval
    :else :complete))

(defn approve-slot! [field]
  (swap! (forms/!meta field) assoc :approved true))

(defn reject-slot! [field reason]
  (swap! (forms/!meta field) assoc :rejected true :rejection-reason reason))

;; =============================================================================
;; TRANSIENT UI COMPONENTS
;; =============================================================================

#?(:cljs
   (do
     (defn transient-panel
       "A transient UI panel that appears for a workflow step"
       [{:keys [title status on-complete]} & children]
       [:div.border.rounded-lg.p-4.mb-4.bg-white.shadow-sm
        {:class (case status
                  :active "border-blue-500 ring-2 ring-blue-200"
                  :complete "border-green-500 bg-green-50"
                  :pending "border-gray-300 opacity-60"
                  "border-gray-300")}
        [:div.flex.justify-between.items-center.mb-3
         [:h3.font-semibold.text-lg title]
         (case status
           :complete [:span.text-green-600 "Complete"]
           :loading [:span.text-blue-600.animate-pulse "Processing..."]
           :pending [:span.text-gray-400 "Waiting..."]
           nil)]
        (into [:div] children)])

     (defn approval-ui
       "Transient UI for approving/rejecting AI output"
       [{:keys [field on-approve on-reject label]}]
       [:div.space-y-3
        [:div.bg-gray-50.p-3.rounded.font-mono.text-sm.whitespace-pre-wrap
         (str @field)]
        [:div.flex.gap-2
         [:button.px-4.py-2.bg-green-600.text-white.rounded.hover:bg-green-700
          {:on-click on-approve}
          (or label "Approve")]
         [:button.px-4.py-2.bg-red-600.text-white.rounded.hover:bg-red-700
          {:on-click #(let [reason (js/prompt "Reason for rejection?")]
                        (when reason (on-reject reason)))}
          "Reject / Revise"]]])

     (defn feedback-ui
       "Transient UI for providing feedback on content"
       [{:keys [content-field feedback-field on-submit placeholder]}]
       [:div.space-y-3
        [:div.bg-gray-50.p-3.rounded.whitespace-pre-wrap
         @content-field]
        [:textarea.w-full.border.rounded.p-2
         {:placeholder (or placeholder "Your feedback...")
          :value (or @feedback-field "")
          :on-change #(reset! feedback-field (.. % -target -value))}]
        [:button.px-4.py-2.bg-blue-600.text-white.rounded.hover:bg-blue-700
         {:on-click on-submit
          :disabled (str/blank? @feedback-field)}
         "Submit Feedback"]])

     (defn section-editor-ui
       "Transient UI for editing a document section"
       [{:keys [section-field on-save]}]
       (let [{:keys [title draft feedback]} @section-field]
         [:div.space-y-3
          [:h4.font-medium title]
          [:textarea.w-full.border.rounded.p-2.h-32.font-mono.text-sm
           {:value draft
            :on-change #(swap! section-field assoc :draft (.. % -target -value))}]
          (when feedback
            [:div.text-sm.text-gray-600.italic
             "Feedback: " feedback])
          [:button.px-4.py-2.bg-blue-600.text-white.rounded
           {:on-click on-save}
           "Save Section"]]))))

;; =============================================================================
;; THE WORKFLOW: AI-Assisted Article Writing
;; =============================================================================
;;
;; Flow:
;; 1. Human provides topic/thesis (transient: text input)
;; 2. AI generates outline (transient: approval panel)
;; 3. Human approves/modifies outline
;; 4. AI drafts each section (parallel, transient: approval per section)
;; 5. Human provides feedback on each section
;; 6. AI improves sections based on feedback
;; 7. Human does final review (transient: final approval)
;; 8. Output: complete article

(defn article-workflow-schema
  "Defines the shape of the article writing workflow"
  []
  '{:topic ?topic                    ; Human input
    :thesis ?thesis                  ; Human input
    :outline ?outline                ; AI generates, human approves
    :sections ?sections              ; AI drafts each, human reviews
    :final-article ?final})          ; Assembled output

#?(:cljs
   (defn article-workflow-ui
     "Main UI for the article writing workflow"
     []
     (forms/with-form
       [?workflow
        {:topic ?topic
         :thesis ?thesis
         :outline ?outline
         :sections (?sections :many {:title ?title
                                     :notes ?notes
                                     :draft ?draft
                                     :feedback ?feedback
                                     :final ?final})
         :final-article (when (every? :final @?sections)
                          {:topic @?topic
                           :thesis @?thesis
                           :content (str/join "\n\n" (map :final @?sections))})}]

       (let [step (cond
                    (str/blank? @?topic) :enter-topic
                    (str/blank? @?thesis) :enter-thesis
                    (nil? @?outline) :generate-outline
                    (not (:approved @(forms/!meta ?outline))) :approve-outline
                    (empty? @?sections) :generate-sections
                    (some #(nil? (:draft %)) @?sections) :drafting
                    (some #(and (:draft %) (nil? (:feedback %))) @?sections) :reviewing
                    (some #(and (:feedback %) (nil? (:final %))) @?sections) :improving
                    :else :complete)]

         [:div.max-w-2xl.mx-auto.p-6

          [:h1.text-2xl.font-bold.mb-6 "AI-Assisted Article Writer"]

          ;; Progress indicator
          [:div.mb-6.flex.gap-2
           (for [[s label] [[:enter-topic "Topic"]
                            [:enter-thesis "Thesis"]
                            [:generate-outline "Outline"]
                            [:generate-sections "Draft"]
                            [:reviewing "Review"]
                            [:complete "Done"]]]
             [:div.px-3.py-1.rounded.text-sm
              {:key s
               :class (cond
                        (= s step) "bg-blue-600 text-white"
                        (#{:enter-topic} step) "bg-gray-200"
                        :else "bg-green-100 text-green-800")}
              label])]

          ;; STEP 1: Topic input (transient)
          (when (= step :enter-topic)
            [transient-panel {:title "What do you want to write about?" :status :active}
             [:input.w-full.border.rounded.p-3.text-lg
              {:placeholder "Enter your topic..."
               :value @?topic
               :on-change #(reset! ?topic (.. % -target -value))
               :on-key-down #(when (= "Enter" (.-key %))
                               (reset! ?topic (.. % -target -value)))}]])

          ;; STEP 2: Thesis input (transient)
          (when (= step :enter-thesis)
            [transient-panel {:title "What's your main thesis?" :status :active}
             [:div.mb-2.text-gray-600 "Topic: " [:strong @?topic]]
             [:textarea.w-full.border.rounded.p-3
              {:placeholder "Your main argument or thesis..."
               :value @?thesis
               :on-change #(reset! ?thesis (.. % -target -value))}]
             [:button.mt-2.px-4.py-2.bg-blue-600.text-white.rounded
              {:on-click identity
               :disabled (str/blank? @?thesis)}
              "Continue"]])

          ;; STEP 3: Generate outline (AI call)
          (when (= step :generate-outline)
            [transient-panel {:title "Generating outline..." :status :loading}
             [:div.text-gray-600 "AI is creating an outline for your article..."
              [:div.mt-4
               [:button.px-4.py-2.bg-blue-600.text-white.rounded
                {:on-click #(p/let [result (mock-ai-call {:task :expand-outline
                                                          :input {:topic @?topic
                                                                  :thesis @?thesis}})]
                              (reset! ?outline result))}
                "Generate Outline"]]]])

          ;; STEP 4: Approve outline (transient approval UI)
          (when (= step :approve-outline)
            [transient-panel {:title "Review Outline" :status :active}
             [:div.space-y-2.mb-4
              (for [[i section] (map-indexed vector @?outline)]
                [:div.flex.items-center.gap-2.p-2.bg-gray-50.rounded
                 {:key i}
                 [:span.font-medium (str (inc i) ". " (:title section))]
                 [:span.text-gray-500.text-sm (:notes section)]])]
             [:div.flex.gap-2
              [:button.px-4.py-2.bg-green-600.text-white.rounded
               {:on-click #(do (approve-slot! ?outline)
                               ;; Create section slots from outline
                               (doseq [section @?outline]
                                 (forms/add-many! ?sections
                                                  {'?title (:title section)
                                                   '?notes (:notes section)})))}
               "Approve Outline"]
              [:button.px-4.py-2.bg-gray-600.text-white.rounded
               {:on-click #(reset! ?outline nil)}
               "Regenerate"]]])

          ;; STEP 5+: Section drafting and review
          (when (#{:generate-sections :drafting :reviewing :improving} step)
            [:div.space-y-4
             (doall
               (for [{:as ?section :syms [?title ?notes ?draft ?feedback ?final]} ?sections
                     :let [section-step (cond
                                          (nil? @?draft) :needs-draft
                                          (nil? @?feedback) :needs-feedback
                                          (nil? @?final) :needs-improvement
                                          :else :done)]]
                 ^{:key (:sym ?section)}
                 [transient-panel
                  {:title @?title
                   :status (case section-step
                             :needs-draft :loading
                             :needs-feedback :active
                             :needs-improvement :loading
                             :done :complete)}

                  (case section-step
                    :needs-draft
                    [:div
                     [:div.text-gray-500.mb-2 @?notes]
                     [:button.px-3.py-1.bg-blue-600.text-white.rounded.text-sm
                      {:on-click #(p/let [result (mock-ai-call {:task :draft-section
                                                                :input {:title @?title
                                                                        :notes @?notes}})]
                                    (reset! ?draft result))}
                      "Draft Section"]]

                    :needs-feedback
                    [feedback-ui {:content-field ?draft
                                  :feedback-field ?feedback
                                  :on-submit #(prn "Feedback submitted:" @?feedback)
                                  :placeholder "How can this section be improved?"}]

                    :needs-improvement
                    [:div
                     [:div.text-gray-500.mb-2 "Improving based on feedback..."]
                     [:button.px-3.py-1.bg-blue-600.text-white.rounded.text-sm
                      {:on-click #(p/let [result (mock-ai-call {:task :improve-draft
                                                                :input {:draft @?draft
                                                                        :feedback @?feedback}})]
                                    (reset! ?final result))}
                      "Improve Draft"]]

                    :done
                    [:div.text-green-700 "Section complete"])]))])

          ;; COMPLETE: Show final article
          (when (= step :complete)
            [transient-panel {:title "Article Complete!" :status :complete}
             [:div.prose
              [:h2 @?topic]
              [:p.italic @?thesis]
              [:hr]
              (for [{:syms [?title ?final]} ?sections]
                [:div {:key @?title}
                 [:h3 @?title]
                 [:div.whitespace-pre-wrap @?final]])]
             [:button.mt-4.px-4.py-2.bg-blue-600.text-white.rounded
              {:on-click #(js/console.log "Final:" (clj->js @?workflow))}
              "Export Article"]])

          ;; Debug: show workflow state
          [:details.mt-8
           [:summary.cursor-pointer.text-gray-500 "Debug: Workflow State"]
           [:pre.text-xs.bg-gray-100.p-2.rounded.overflow-auto
            (with-out-str (cljs.pprint/pprint @?workflow))]]]))))

;; =============================================================================
;; INSIGHTS FROM THIS DEMO
;; =============================================================================
;;
;; 1. Inside-out's :many fields naturally model iterative/parallel work
;;    - Each section is a sub-form with its own lifecycle
;;    - Add sections dynamically from AI-generated outline
;;
;; 2. Computed form values track readiness automatically
;;    - Final article only assembles when all sections complete
;;    - Reactive updates propagate through the dependency graph
;;
;; 3. Transient UIs emerge from workflow state
;;    - Which UI to show is derived from field states
;;    - UIs appear/disappear based on what's needed
;;
;; 4. Approval gates are just metadata + conditional rendering
;;    - :approved flag in !meta controls progression
;;    - Same pattern could work for any human-in-the-loop checkpoint
;;
;; 5. The workflow definition IS the data schema
;;    - No separate "workflow engine" configuration
;;    - The form expression defines both structure and flow
;;
;; WHAT'S MISSING (for production):
;; - Persistence (serialize workflow state)
;; - Real AI integration (replace mock calls)
;; - Error handling (AI failures, timeouts)
;; - Streaming responses (progressive updates)
;; - Undo/redo (state history)
;; - Collaboration (multiple users editing)
;; - Workflow templates (reusable patterns)

)
