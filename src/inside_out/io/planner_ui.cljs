(ns inside-out.io.planner-ui
  "UI for the meta-planning experience

   Allows users to:
   - Describe their goal
   - See AI-generated plan
   - Edit the plan if needed
   - Execute and monitor progress"
  (:require [clojure.string :as str]
            [inside-out.io :as io]
            [inside-out.io.executor :as exec]
            [inside-out.io.planner :as planner]
            [inside-out.io.ui :as ui]
            [promesa.core :as p]
            [re-db.reactive :as r]
            [reagent.core :as reagent]))

;; =============================================================================
;; PLAN VISUALIZATION
;; =============================================================================

(defn slot-badge [sym]
  [:span.badge.badge-sm.mr-1.font-mono
   {:class (if (io/human-slot? sym)
             "badge-primary"
             "badge-secondary")}
   (name sym)])

(defn plan-slot-row [{:keys [sym type meta deps]}]
  [:div.flex.items-start.gap-3.p-3.rounded-lg.border.bg-base-100
   {:class (if (= type :human) "border-primary/30" "border-secondary/30")}
   [:div.text-2xl (if (= type :human) "👤" "🤖")]
   [:div.flex-1
    [:div.flex.items-center.gap-2.mb-1
     [slot-badge sym]
     (when (:approve meta)
       [:span.badge.badge-warning.badge-xs "needs approval"])]
    [:div.text-sm
     (or (:ask meta) (:ai meta) "(no prompt)")]
    (when (seq deps)
      [:div.text-xs.text-base-content/50.mt-1
       "Uses: "
       (for [d deps]
         ^{:key (str d)}
         [:span.font-mono.mr-1 (str "{" (name d) "}")])])]])

(defn plan-preview
  "Visual preview of a parsed plan"
  [{:keys [form]}]
  (let [dep-graph (io/build-dependency-graph form)
        order (io/topological-sort dep-graph)]
    [:div.space-y-2
     [:div.flex.items-center.gap-2.mb-3
      [:span.text-sm.font-medium "Execution Order:"]
      [:div.flex.flex-wrap.gap-1
       (for [[i sym] (map-indexed vector order)]
         ^{:key (str sym)}
         [:span.badge.badge-ghost.badge-sm
          (str (inc i) ". " (name sym))])]]
     [:div.space-y-2
      (for [sym order
            :let [info (get dep-graph sym)]]
        ^{:key (str sym)}
        [plan-slot-row info])]]))

(defn plan-code-view
  "Raw code view of the plan"
  [{:keys [form]}]
  [:pre.bg-base-200.p-4.rounded-lg.text-sm.font-mono.overflow-auto.max-h-96
   (with-out-str (cljs.pprint/pprint form))])

;; =============================================================================
;; PLAN EDITOR
;; =============================================================================

(defn plan-editor
  "Editable code view of the plan"
  [{:keys [initial-form on-change on-validate]}]
  (let [!code (reagent/atom (with-out-str (cljs.pprint/pprint initial-form)))
        !error (reagent/atom nil)
        !valid (reagent/atom true)]
    (fn []
      [:div.space-y-2
       [:textarea.textarea.textarea-bordered.w-full.h-64.font-mono.text-sm
        {:value @!code
         :on-change (fn [e]
                      (let [v (.. e -target -value)]
                        (reset! !code v)
                        (let [result (planner/parse-plan v)]
                          (reset! !error (when-not (:ok result) (:error result)))
                          (reset! !valid (:ok result))
                          (when (and on-change (:ok result))
                            (on-change (:form result)))
                          (when on-validate
                            (on-validate result)))))}]
       (when @!error
         [:div.alert.alert-error.text-sm
          [:span @!error]])
       (when @!valid
         [:div.text-success.text-sm "✓ Valid io form"])])))

;; =============================================================================
;; GOAL INPUT
;; =============================================================================

(defn goal-input
  "Input form for the user's goal"
  [{:keys [on-submit]}]
  (let [!goal (reagent/atom "")
        !context (reagent/atom "")
        !submitting (reagent/atom false)]
    (fn []
      [:div.card.bg-base-100.shadow-lg
       [:div.card-body
        [:h2.card-title "What would you like to accomplish?"]
        [:p.text-sm.text-base-content/70.mb-4
         "Describe your goal and I'll create a step-by-step plan for us to work on together."]

        [:div.form-control.mb-4
         [:label.label [:span.label-text "Your Goal"]]
         [:input.input.input-bordered.input-lg.w-full
          {:placeholder "e.g., Help me write a compelling blog post about climate technology"
           :value @!goal
           :on-change #(reset! !goal (.. % -target -value))
           :on-key-down #(when (and (= "Enter" (.-key %))
                                    (not (str/blank? @!goal)))
                           (reset! !submitting true)
                           (on-submit {:goal @!goal :context @!context}))}]]

        [:div.form-control.mb-4
         [:label.label [:span.label-text "Additional Context (optional)"]]
         [:textarea.textarea.textarea-bordered.w-full
          {:placeholder "Any specific requirements, constraints, or context..."
           :value @!context
           :on-change #(reset! !context (.. % -target -value))
           :rows 3}]]

        [:div.card-actions.justify-end
         [:button.btn.btn-primary.btn-lg
          {:disabled (or (str/blank? @!goal) @!submitting)
           :on-click #(do (reset! !submitting true)
                          (on-submit {:goal @!goal :context @!context}))}
          (if @!submitting
            [:span.loading.loading-spinner]
            "Generate Plan")]]]])))

;; =============================================================================
;; PLAN REVIEW
;; =============================================================================

(defn plan-review
  "Review screen for the generated plan"
  [{:keys [plan on-approve on-edit on-regenerate]}]
  (let [!view (reagent/atom :visual)  ; :visual or :code
        !editing (reagent/atom false)
        !edited-form (reagent/atom nil)]
    (fn []
      [:div.card.bg-base-100.shadow-lg
       [:div.card-body
        [:div.flex.items-center.justify-between.mb-4
         [:h2.card-title "Your Plan"]
         [:div.btn-group
          [:button.btn.btn-sm
           {:class (when (= @!view :visual) "btn-active")
            :on-click #(reset! !view :visual)}
           "Visual"]
          [:button.btn.btn-sm
           {:class (when (= @!view :code) "btn-active")
            :on-click #(reset! !view :code)}
           "Code"]]]

        [:p.text-sm.text-base-content/70.mb-4
         "Review the plan below. You can approve it, edit it, or regenerate a new one."]

        ;; Plan display
        [:div.mb-6
         (if @!editing
           [plan-editor {:initial-form (:form plan)
                         :on-change #(reset! !edited-form %)
                         :on-validate identity}]
           (if (= @!view :visual)
             [plan-preview plan]
             [plan-code-view plan]))]

        ;; Validation issues
        (when-let [issues (seq (:issues plan))]
          [:div.alert.alert-warning.mb-4
           [:ul
            (for [issue issues]
              ^{:key issue}
              [:li issue])]])

        ;; Actions
        [:div.flex.gap-3.flex-wrap
         (if @!editing
           [:<>
            [:button.btn.btn-primary
             {:on-click #(do (when @!edited-form
                               (on-edit @!edited-form))
                             (reset! !editing false))}
             "Save Changes"]
            [:button.btn.btn-ghost
             {:on-click #(do (reset! !editing false)
                             (reset! !edited-form nil))}
             "Cancel"]]
           [:<>
            [:button.btn.btn-primary.btn-lg
             {:on-click #(on-approve (or @!edited-form (:form plan)))}
             "✓ Approve & Execute"]
            [:button.btn.btn-outline
             {:on-click #(reset! !editing true)}
             "Edit Plan"]
            [:button.btn.btn-ghost
             {:on-click on-regenerate}
             "Regenerate"]])]]])))

;; =============================================================================
;; EXECUTION VIEW
;; =============================================================================

(defn execution-view
  "View during plan execution"
  [{:keys [executor on-complete]}]
  [:div.space-y-6
   [:div.flex.items-center.gap-3
    [:h2.text-xl.font-bold "Executing Plan"]
    [:span.loading.loading-spinner.loading-sm]]

   [ui/progress-bar {:!workflow (:!workflow executor)}]

   [ui/transient-ui {:executor executor}]

   [:div.collapse.collapse-arrow.bg-base-200
    [:input {:type "checkbox"}]
    [:div.collapse-title.font-medium "View All Steps"]
    [:div.collapse-content
     [ui/slot-list {:!workflow (:!workflow executor)}]]]])

;; =============================================================================
;; COMPLETE VIEW
;; =============================================================================

(defn complete-view
  "View when plan execution is complete"
  [{:keys [result goal on-restart]}]
  [:div.card.bg-success/10.border.border-success
   [:div.card-body
    [:h2.card-title.text-success "✓ Plan Complete!"]
    [:p.mb-4 "Successfully completed: " [:strong goal]]

    [:div.bg-base-200.rounded-lg.p-4.mb-4
     [:h3.font-semibold.mb-2 "Results"]
     [:pre.text-sm.overflow-auto.max-h-96
      (with-out-str (cljs.pprint/pprint result))]]

    [:div.card-actions
     [:button.btn.btn-primary
      {:on-click on-restart}
      "Start New Plan"]]]])

;; =============================================================================
;; MAIN PLANNER UI
;; =============================================================================

(defn planner-view
  "Complete meta-planning UI"
  [{:keys [planner ai-client]}]
  (let [;; State
        !stage (reagent/atom :goal)  ; :goal :generating :review :executing :complete
        !goal (reagent/atom nil)
        !context (reagent/atom nil)
        !plan (reagent/atom nil)
        !session (reagent/atom nil)
        !result (reagent/atom nil)

        ;; Handlers
        handle-submit (fn [{:keys [goal context]}]
                        (reset! !goal goal)
                        (reset! !context context)
                        (reset! !stage :generating)
                        (-> (planner/generate-plan planner goal {:context context})
                            (p/then (fn [result]
                                      (reset! !plan result)
                                      (reset! !stage :review)))))

        handle-approve (fn [form]
                         (let [id (str "plan-" (random-uuid))
                               !wf (io/make-reactive-workflow id form)
                               executor (exec/make-executor
                                          {:!workflow !wf
                                           :ai-client ai-client
                                           :on-complete (fn [wf]
                                                          (reset! !result (:values wf))
                                                          (reset! !stage :complete))})]
                           (reset! !session {:!workflow !wf :executor executor})
                           (reset! !stage :executing)
                           (exec/run! executor)))

        handle-edit (fn [form]
                      (swap! !plan assoc :form form))

        handle-regenerate (fn []
                            (reset! !stage :generating)
                            (-> (planner/generate-plan planner @!goal {:context @!context})
                                (p/then (fn [result]
                                          (reset! !plan result)
                                          (reset! !stage :review)))))

        handle-restart (fn []
                         (reset! !stage :goal)
                         (reset! !goal nil)
                         (reset! !context nil)
                         (reset! !plan nil)
                         (reset! !session nil)
                         (reset! !result nil))]

    (fn []
      [:div.max-w-4xl.mx-auto.p-6
       ;; Header
       [:div.text-center.mb-8
        [:h1.text-3xl.font-bold.mb-2 "io Planner"]
        [:p.text-base-content/70
         "Describe your goal and let AI create a collaborative plan"]]

       ;; Stage indicator
       [:ul.steps.w-full.mb-8
        [:li.step {:class (when (#{:goal :generating :review :executing :complete} @!stage) "step-primary")} "Goal"]
        [:li.step {:class (when (#{:generating :review :executing :complete} @!stage) "step-primary")} "Plan"]
        [:li.step {:class (when (#{:executing :complete} @!stage) "step-primary")} "Execute"]
        [:li.step {:class (when (#{:complete} @!stage) "step-primary")} "Done"]]

       ;; Content based on stage
       (case @!stage
         :goal
         [goal-input {:on-submit handle-submit}]

         :generating
         [:div.flex.flex-col.items-center.justify-center.py-12
          [:span.loading.loading-spinner.loading-lg.text-primary]
          [:p.mt-4.text-lg "Generating your plan..."]]

         :review
         (if (:ok @!plan)
           [plan-review {:plan @!plan
                         :on-approve handle-approve
                         :on-edit handle-edit
                         :on-regenerate handle-regenerate}]
           [:div.alert.alert-error
            [:div
             [:h3.font-bold "Failed to generate plan"]
             [:p (:error @!plan)]
             [:button.btn.btn-sm.mt-2
              {:on-click handle-regenerate}
              "Try Again"]]])

         :executing
         (when-let [{:keys [executor]} @!session]
           [execution-view {:executor executor
                            :on-complete identity}])

         :complete
         [complete-view {:result @!result
                         :goal @!goal
                         :on-restart handle-restart}]

         ;; Default
         [:div "Unknown stage"])])))

;; =============================================================================
;; STANDALONE DEMO
;; =============================================================================

(defn demo []
  [planner-view {:planner (planner/make-mock-planner)
                 :ai-client (exec/make-mock-client)}])
