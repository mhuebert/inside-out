(ns inside-out.io.ui
  "UI Components for io workflows

   Includes:
   - State visualizers (progress, dependency graph)
   - Transient UI components (inputs, approvals)
   - Workflow chrome (header, navigation)

   Uses Tailwind CSS and DaisyUI for styling."
  (:require [clojure.string :as str]
            [inside-out.io :as io]
            [inside-out.io.executor :as exec]
            [re-db.reactive :as r]
            [reagent.core :as reagent]))

;; =============================================================================
;; UTILITY
;; =============================================================================

(defn slot-icon [slot-state]
  (case (:type slot-state)
    :human "👤"
    :ai "🤖"
    "•"))

(defn status-color [status]
  (case status
    :complete "text-success"
    :in-progress "text-info"
    :awaiting-approval "text-warning"
    :error "text-error"
    :pending "text-base-content/50"
    :ready "text-primary"
    "text-base-content"))

(defn status-bg [status]
  (case status
    :complete "bg-success/10"
    :in-progress "bg-info/10"
    :awaiting-approval "bg-warning/10"
    :error "bg-error/10"
    :pending "bg-base-200"
    :ready "bg-primary/10"
    "bg-base-100"))

(defn status-badge [status]
  (let [text (case status
               :complete "Complete"
               :in-progress "In Progress"
               :awaiting-approval "Needs Approval"
               :error "Error"
               :pending "Pending"
               :ready "Ready"
               (name status))]
    [:span.badge.badge-sm
     {:class (case status
               :complete "badge-success"
               :in-progress "badge-info"
               :awaiting-approval "badge-warning"
               :error "badge-error"
               :pending "badge-ghost"
               :ready "badge-primary"
               "badge-ghost")}
     text]))

;; =============================================================================
;; PROGRESS VISUALIZER
;; =============================================================================

(defn progress-bar
  "Horizontal progress bar showing workflow completion"
  [{:keys [!workflow]}]
  (let [progress (r/reaction (io/workflow-progress @!workflow))]
    (fn []
      [:div.w-full
       [:div.flex.justify-between.mb-1
        [:span.text-sm.font-medium "Progress"]
        [:span.text-sm.text-base-content/70 @progress "%"]]
       [:progress.progress.progress-primary.w-full
        {:value @progress :max 100}]])))

(defn progress-steps
  "Step-by-step progress indicator"
  [{:keys [!workflow]}]
  (fn []
    (let [wf @!workflow
          order (:order wf)]
      [:ul.steps.steps-vertical.lg:steps-horizontal.w-full
       (for [sym order
             :let [slot (get-in wf [:slots sym])
                   status (:status slot)]]
         ^{:key (str sym)}
         [:li.step
          {:class (case status
                    :complete "step-primary"
                    :in-progress "step-info"
                    :awaiting-approval "step-warning"
                    :error "step-error"
                    "")}
          [:span.text-xs
           (slot-icon slot)
           " "
           (name sym)]])])))

;; =============================================================================
;; SLOT LIST VISUALIZER
;; =============================================================================

(defn slot-card
  "Card showing a single slot's state"
  [slot-state values]
  (let [{:keys [sym type status value meta deps]} slot-state]
    [:div.card.bg-base-100.shadow-sm.border
     {:class (status-bg status)}
     [:div.card-body.p-4
      [:div.flex.items-center.justify-between
       [:div.flex.items-center.gap-2
        [:span.text-lg (slot-icon slot-state)]
        [:h3.font-semibold (name sym)]]
       [status-badge status]]

      ;; Prompt/Ask text
      (when-let [prompt (or (:ai meta) (:ask meta))]
        [:p.text-sm.text-base-content/70.mt-2.italic
         (str "\"" prompt "\"")])

      ;; Dependencies
      (when (seq deps)
        [:div.mt-2
         [:span.text-xs.text-base-content/50 "Depends on: "]
         (for [d deps]
           ^{:key (str d)}
           [:span.badge.badge-xs.badge-ghost.mr-1 (name d)])])

      ;; Value (if complete or awaiting approval)
      (when value
        [:div.mt-3.p-2.bg-base-200.rounded.text-sm.font-mono.max-h-32.overflow-auto
         (if (string? value)
           (if (> (count value) 200)
             (str (subs value 0 200) "...")
             value)
           (pr-str value))])]]))

(defn slot-list
  "List of all slots with their states"
  [{:keys [!workflow]}]
  (fn []
    (let [wf @!workflow
          values (:values wf)]
      [:div.grid.gap-3.md:grid-cols-2.lg:grid-cols-3
       (for [sym (:order wf)
             :let [slot (get-in wf [:slots sym])]]
         ^{:key (str sym)}
         [slot-card slot values])])))

;; =============================================================================
;; DEPENDENCY GRAPH VISUALIZER
;; =============================================================================

(defn dependency-graph
  "Visual representation of the dependency graph"
  [{:keys [!workflow]}]
  (fn []
    (let [wf @!workflow]
      [:div.p-4.bg-base-200.rounded-lg
       [:h3.font-semibold.mb-3 "Dependency Graph"]
       [:div.space-y-2
        (for [sym (:order wf)
              :let [slot (get-in wf [:slots sym])
                    deps (:deps slot)]]
          ^{:key (str sym)}
          [:div.flex.items-center.gap-2
           [:span.font-mono.text-sm
            {:class (status-color (:status slot))}
            (slot-icon slot) " " (name sym)]
           (when (seq deps)
             [:<>
              [:span.text-base-content/30 "←"]
              (for [d deps]
                ^{:key (str sym "-" d)}
                [:span.badge.badge-xs
                 {:class (status-color (get-in wf [:slots d :status]))}
                 (name d)])])])]])))

;; =============================================================================
;; TRANSIENT UI COMPONENTS
;; =============================================================================

(defn text-input
  "Transient text input for human slots"
  [{:keys [slot-sym meta on-submit placeholder]}]
  (let [!value (reagent/atom "")]
    (fn []
      [:div.form-control.w-full
       (when-let [label (:ask meta)]
         [:label.label
          [:span.label-text label]])
       [:input.input.input-bordered.w-full
        {:type "text"
         :placeholder (or placeholder "Enter your response...")
         :value @!value
         :on-change #(reset! !value (.. % -target -value))
         :on-key-down #(when (= "Enter" (.-key %))
                         (on-submit @!value)
                         (reset! !value ""))}]
       [:div.mt-2
        [:button.btn.btn-primary
         {:on-click #(do (on-submit @!value)
                         (reset! !value ""))
          :disabled (str/blank? @!value)}
         "Submit"]]])))

(defn textarea-input
  "Transient textarea for longer human input"
  [{:keys [slot-sym meta on-submit placeholder]}]
  (let [!value (reagent/atom "")]
    (fn []
      [:div.form-control.w-full
       (when-let [label (:ask meta)]
         [:label.label
          [:span.label-text label]])
       [:textarea.textarea.textarea-bordered.w-full.h-32
        {:placeholder (or placeholder "Enter your response...")
         :value @!value
         :on-change #(reset! !value (.. % -target -value))}]
       [:div.mt-2
        [:button.btn.btn-primary
         {:on-click #(do (on-submit @!value)
                         (reset! !value ""))
          :disabled (str/blank? @!value)}
         "Submit"]]])))

(defn select-input
  "Transient select input"
  [{:keys [slot-sym meta on-submit options]}]
  (let [!value (reagent/atom nil)
        opts (or options (:options meta) [])]
    (fn []
      [:div.form-control.w-full
       (when-let [label (:ask meta)]
         [:label.label
          [:span.label-text label]])
       [:select.select.select-bordered.w-full
        {:value (or @!value "")
         :on-change #(reset! !value (.. % -target -value))}
        [:option {:value "" :disabled true} "Select an option..."]
        (for [opt opts]
          ^{:key (str opt)}
          [:option {:value (str opt)} (str opt)])]
       [:div.mt-2
        [:button.btn.btn-primary
         {:on-click #(on-submit @!value)
          :disabled (nil? @!value)}
         "Submit"]]])))

(defn approval-panel
  "Transient approval UI for AI-generated content"
  [{:keys [slot-sym value meta on-approve on-reject]}]
  (let [!reason (reagent/atom "")]
    (fn []
      [:div.space-y-4
       ;; Header
       [:div.flex.items-center.gap-2
        [:span.text-lg "🤖"]
        [:span.font-semibold "AI Generated Content"]
        [status-badge :awaiting-approval]]

       ;; Prompt context
       (when-let [prompt (:ai meta)]
         [:div.text-sm.text-base-content/70.italic
          "Prompt: \"" prompt "\""])

       ;; Content
       [:div.bg-base-200.rounded-lg.p-4.max-h-96.overflow-auto
        [:div.prose.prose-sm.max-w-none
         (if (string? value)
           [:pre.whitespace-pre-wrap value]
           [:pre (pr-str value)])]]

       ;; Actions
       [:div.flex.gap-3
        [:button.btn.btn-success
         {:on-click on-approve}
         "✓ Approve"]
        [:div.flex-1
         [:input.input.input-bordered.input-sm.w-full
          {:placeholder "Reason for revision (optional)"
           :value @!reason
           :on-change #(reset! !reason (.. % -target -value))}]]
        [:button.btn.btn-error
         {:on-click #(on-reject @!reason)}
         "✗ Revise"]]])))

(defn loading-indicator
  "Loading indicator for AI generation"
  [{:keys [slot-sym meta]}]
  [:div.flex.items-center.gap-3.p-4
   [:span.loading.loading-spinner.loading-md]
   [:div
    [:div.font-medium "Generating..."]
    (when-let [prompt (:ai meta)]
      [:div.text-sm.text-base-content/70.italic
       (str "\"" (subs prompt 0 (min 50 (count prompt))) "...\"")])]])

;; =============================================================================
;; TRANSIENT UI ROUTER
;; =============================================================================

(defn transient-ui
  "Render the appropriate transient UI for the current slot"
  [{:keys [executor]}]
  (let [!summary (exec/reactive-summary executor)]
    (fn []
      (let [{:keys [current status]} @!summary
            !workflow (:!workflow executor)
            slot (when current (get-in @!workflow [:slots current]))]
        (when slot
          [:div.card.bg-base-100.shadow-lg.border.border-primary/20
           [:div.card-body
            (case status
              :awaiting-approval
              [approval-panel
               {:slot-sym current
                :value (:value slot)
                :meta (:meta slot)
                :on-approve #(exec/submit-approval! executor current true)
                :on-reject #(exec/submit-approval! executor current false %)}]

              :awaiting-input
              (let [ui-type (get-in slot [:meta :ui] :text)]
                (case ui-type
                  :textarea [textarea-input
                             {:slot-sym current
                              :meta (:meta slot)
                              :on-submit #(exec/submit-input! executor current %)}]
                  :select [select-input
                           {:slot-sym current
                            :meta (:meta slot)
                            :on-submit #(exec/submit-input! executor current %)}]
                  ;; Default: text input
                  [text-input
                   {:slot-sym current
                    :meta (:meta slot)
                    :on-submit #(exec/submit-input! executor current %)}]))

              :generating
              [loading-indicator {:slot-sym current :meta (:meta slot)}]

              ;; Default
              nil)]])))))

;; =============================================================================
;; COMPLETE WORKFLOW UI
;; =============================================================================

(defn workflow-header
  "Header showing workflow title and overall status"
  [{:keys [!workflow title]}]
  (fn []
    (let [wf @!workflow
          progress (io/workflow-progress wf)
          complete? (io/workflow-complete? wf)]
      [:div.flex.items-center.justify-between.mb-6
       [:div
        [:h1.text-2xl.font-bold (or title "Workflow")]
        [:p.text-sm.text-base-content/70
         (str (count (io/complete-slots wf)) " of " (count (:slots wf)) " steps complete")]]
       [:div.flex.items-center.gap-4
        [:div.radial-progress.text-primary
         {:style {"--value" progress "--size" "3rem"}}
         [:span.text-xs progress "%"]]
        (when complete?
          [:span.badge.badge-success.badge-lg "Complete"])]])))

(defn workflow-result
  "Display the final workflow result"
  [{:keys [!workflow]}]
  (fn []
    (let [wf @!workflow]
      (when (io/workflow-complete? wf)
        [:div.card.bg-success/10.border.border-success
         [:div.card-body
          [:h3.card-title.text-success "✓ Workflow Complete"]
          [:div.mt-4
           [:h4.font-semibold.mb-2 "Results:"]
           [:pre.bg-base-200.p-4.rounded.text-sm.overflow-auto
            (with-out-str (cljs.pprint/pprint (:values wf)))]]]]))))

(defn workflow-view
  "Complete workflow UI with header, progress, transient UI, and results"
  [{:keys [executor title show-slots? show-graph?]}]
  (let [{:keys [!workflow]} executor]
    [:div.max-w-4xl.mx-auto.p-6
     [workflow-header {:!workflow !workflow :title title}]

     ;; Progress
     [:div.mb-6
      [progress-bar {:!workflow !workflow}]]

     ;; Main content area
     [:div.grid.gap-6.lg:grid-cols-3
      ;; Left: Transient UI (main interaction area)
      [:div.lg:col-span-2
       [transient-ui {:executor executor}]
       [workflow-result {:!workflow !workflow}]]

      ;; Right: Status sidebar
      [:div.space-y-4
       (when show-graph?
         [dependency-graph {:!workflow !workflow}])
       [:div.collapse.collapse-arrow.bg-base-200
        [:input {:type "checkbox" :defaultChecked show-slots?}]
        [:div.collapse-title.font-medium "All Slots"]
        [:div.collapse-content
         [slot-list {:!workflow !workflow}]]]]]]))

;; =============================================================================
;; STANDALONE DEMO
;; =============================================================================

(defn demo-workflow
  "Demo workflow for testing"
  []
  (let [!wf (io/make-reactive-workflow
              :demo
              '{:topic (?topic :ask "What topic would you like to explore?")
                :outline ($outline :ai "Create a 3-point outline for {?topic}" :approve)
                :draft ($draft :ai "Write a short draft based on {$outline}")})
        executor (exec/make-executor {:!workflow !wf})]
    ;; Start the workflow
    (exec/run! executor)
    ;; Return the UI
    (fn []
      [workflow-view
       {:executor executor
        :title "Demo: Topic Explorer"
        :show-slots? true
        :show-graph? true}])))
