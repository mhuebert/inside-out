(ns inside-out.io-demo
  "Capstone Demo: IO Workflow System

   A complete demonstration of the io DSL for AI coordination:
   - Meta-planning: describe a goal, AI generates the workflow
   - Workflow creation from templates
   - Live state visualization
   - Transient UIs for human input
   - AI generation with approval gates
   - Persistence and resume
   - Export/import"
  (:require [clojure.string :as str]
            [inside-out.io :as io]
            [inside-out.io.executor :as exec]
            [inside-out.io.ui :as ui]
            [inside-out.io.persist :as persist]
            [inside-out.io.planner :as planner]
            [inside-out.io.planner-ui :as planner-ui]
            [promesa.core :as p]
            [re-db.reactive :as r]
            [reagent.core :as reagent]
            [reagent.dom :as rdom]))

;; =============================================================================
;; CUSTOM AI CLIENT (Mock with realistic responses)
;; =============================================================================

(defn smart-mock-response [prompt]
  "Generate contextually appropriate mock responses"
  (cond
    (str/includes? prompt "outline")
    "1. Introduction\n   - Hook: Start with a compelling question\n   - Thesis: State your main argument\n\n2. Main Body\n   - Point 1: First supporting argument\n   - Point 2: Second supporting argument\n   - Point 3: Third supporting argument\n\n3. Conclusion\n   - Summary of key points\n   - Call to action"

    (str/includes? prompt "draft")
    "# Introduction\n\nIn today's rapidly evolving landscape, understanding this topic has never been more crucial. This exploration will guide you through the key aspects and help you develop a comprehensive understanding.\n\n# Main Points\n\nThe first thing to consider is the fundamental nature of the subject. When we examine it closely, we find several interconnected elements that form the foundation of our understanding.\n\nSecondly, the practical applications cannot be overstated. From everyday decisions to strategic planning, this knowledge proves invaluable.\n\nFinally, looking toward the future, we can anticipate significant developments that will reshape how we approach this domain.\n\n# Conclusion\n\nAs we've explored, this topic offers rich opportunities for growth and innovation. The key is to remain curious and continue learning."

    (str/includes? prompt "sources")
    "1. **Academic Paper**: \"Foundations of the Field\" (2023) - Comprehensive overview of core concepts\n2. **Industry Report**: \"State of the Art Analysis\" - Current trends and future predictions\n3. **Expert Interview**: Dr. Jane Smith on emerging developments\n4. **Case Study**: Real-world application success story\n5. **Book Chapter**: \"Deep Dive into Methodology\" from leading textbook"

    (str/includes? prompt "ideas")
    "1. **Innovative Solution A**: Leverage emerging technology for improved efficiency\n2. **Creative Approach B**: Combine traditional methods with modern tools\n3. **Disruptive Option C**: Challenge assumptions with a fresh perspective\n4. **Incremental Improvement D**: Build on existing strengths\n5. **Collaborative Strategy E**: Partner with complementary organizations"

    (str/includes? prompt "plan")
    "## Action Plan\n\n### Phase 1: Foundation (Week 1-2)\n- Research and gather requirements\n- Identify key stakeholders\n- Define success metrics\n\n### Phase 2: Development (Week 3-4)\n- Build core functionality\n- Iterate based on feedback\n- Document progress\n\n### Phase 3: Launch (Week 5)\n- Final testing\n- Stakeholder review\n- Go-live preparation"

    :else
    (str "Response to: " prompt "\n\nThis is a thoughtfully generated response that addresses the key aspects of your query. The analysis considers multiple perspectives and provides actionable insights.")))

(defrecord SmartMockClient []
  exec/IAIClient
  (generate [_ prompt opts]
    (p/do
      (p/delay (+ 800 (rand-int 1200)))
      (smart-mock-response prompt))))

(defn make-smart-client []
  (->SmartMockClient))

;; =============================================================================
;; APP STATE
;; =============================================================================

(defonce !app-state
  (r/atom {:view :home           ; :home :workflow :create :planner
           :current-workflow nil ; ID of active workflow
           :manager nil          ; WorkflowManager instance
           :planner nil}))       ; Planner instance

(defn init-manager! []
  (when-not (:manager @!app-state)
    (swap! !app-state assoc
           :manager (persist/make-manager)
           :planner (planner/make-mock-planner))))

;; =============================================================================
;; NAVIGATION
;; =============================================================================

(defn navigate! [view & [workflow-id]]
  (swap! !app-state assoc
         :view view
         :current-workflow workflow-id))

;; =============================================================================
;; TEMPLATE GALLERY
;; =============================================================================

(defn template-card [{:keys [key name description form]}]
  [:div.card.bg-base-100.shadow-lg.hover:shadow-xl.transition-shadow.cursor-pointer
   {:on-click #(let [manager (:manager @!app-state)
                     executor (persist/create-from-template!
                                manager key
                                :ai-client (make-smart-client)
                                :auto-save? true)]
                 (exec/run! executor)
                 (navigate! :workflow (str (clojure.core/name key) "-" (random-uuid))))}
   [:div.card-body
    [:h2.card-title name]
    [:p.text-base-content/70 description]
    [:div.mt-4
     [:span.text-xs.text-base-content/50 "Slots: "]
     (for [[sym _] (io/extract-slots form)]
       ^{:key (str sym)}
       [:span.badge.badge-sm.badge-ghost.mr-1
        {:class (if (io/human-slot? sym) "badge-primary" "badge-secondary")}
        (name sym)])]]])

(defn template-gallery []
  [:div
   [:h2.text-xl.font-bold.mb-4 "Start from Template"]
   [:div.grid.gap-4.md:grid-cols-2
    (for [[k template] persist/templates]
      ^{:key k}
      [template-card (assoc template :key k)])]])

;; =============================================================================
;; SAVED WORKFLOWS LIST
;; =============================================================================

(defn saved-workflow-card [{:keys [id progress complete? updated-at slot-count]}]
  [:div.card.bg-base-100.shadow.border
   {:class (when complete? "border-success/30")}
   [:div.card-body.p-4
    [:div.flex.justify-between.items-start
     [:div
      [:h3.font-semibold.truncate {:title id} (subs id 0 (min 30 (count id)))]
      [:p.text-xs.text-base-content/50 updated-at]]
     (if complete?
       [:span.badge.badge-success "Complete"]
       [:div.radial-progress.text-primary.text-xs
        {:style {"--value" progress "--size" "2.5rem" "--thickness" "3px"}}
        (str progress "%")])]
    [:div.flex.gap-2.mt-3
     [:button.btn.btn-sm.btn-primary.flex-1
      {:on-click #(do
                    (-> (persist/load! (:manager @!app-state) id
                                       :ai-client (make-smart-client))
                        (.then (fn [executor]
                                 (when executor
                                   (exec/run! executor)
                                   (navigate! :workflow id))))))}
      (if complete? "View" "Resume")]
     [:button.btn.btn-sm.btn-ghost
      {:on-click #(persist/export-workflow (:manager @!app-state) id)}
      "Export"]
     [:button.btn.btn-sm.btn-error.btn-ghost
      {:on-click #(when (js/confirm "Delete this workflow?")
                    (persist/delete! (:manager @!app-state) id))}
      "×"]]]])

(defn saved-workflows-list []
  (let [!workflows (r/atom [])]
    (-> (persist/list-saved (:manager @!app-state))
        (.then #(reset! !workflows %)))
    (fn []
      [:div
       [:h2.text-xl.font-bold.mb-4 "Saved Workflows"]
       (if (empty? @!workflows)
         [:div.text-base-content/50.text-center.py-8
          "No saved workflows yet. Start from a template above!"]
         [:div.grid.gap-3
          (for [id @!workflows
                :let [index (get (persist/get-index (:manager @!app-state)) id)]]
            ^{:key id}
            [saved-workflow-card (or index {:id id :progress 0})])])])))

;; =============================================================================
;; CUSTOM WORKFLOW CREATOR
;; =============================================================================

(defn workflow-creator []
  (let [!form-text (reagent/atom "")]
    (fn []
      [:div.card.bg-base-100.shadow
       [:div.card-body
        [:h2.card-title "Create Custom Workflow"]
        [:p.text-sm.text-base-content/70.mb-4
         "Write your io form using ?human and $ai slots:"]
        [:textarea.textarea.textarea-bordered.w-full.h-48.font-mono.text-sm
         {:placeholder "{:topic ?topic\n :analysis ($analysis :ai \"Analyze {?topic}\")\n :summary ($summary :ai \"Summarize {$analysis}\")}"
          :value @!form-text
          :on-change #(reset! !form-text (.. % -target -value))}]
        [:div.flex.gap-2.mt-4
         [:button.btn.btn-primary
          {:disabled (str/blank? @!form-text)
           :on-click #(try
                        (let [form (cljs.reader/read-string @!form-text)
                              id (str "custom-" (random-uuid))
                              manager (:manager @!app-state)
                              executor (persist/create! manager id form
                                                        :ai-client (make-smart-client)
                                                        :auto-save? true)]
                          (exec/run! executor)
                          (navigate! :workflow id))
                        (catch js/Error e
                          (js/alert (str "Invalid form: " (.-message e)))))}
          "Create Workflow"]
         [:button.btn.btn-ghost
          {:on-click #(reset! !form-text
                              (pr-str '{:topic (?topic :ask "What topic?")
                                        :outline ($outline :ai "Outline for {?topic}" :approve)
                                        :draft ($draft :ai "Draft based on {$outline}")}))}
          "Load Example"]]]])))

;; =============================================================================
;; PLANNER CTA
;; =============================================================================

(defn planner-cta []
  [:div.card.bg-gradient-to-r.from-primary.to-secondary.text-primary-content.shadow-xl
   [:div.card-body.text-center
    [:h2.card-title.text-2xl.justify-center "🎯 Describe Your Goal"]
    [:p.opacity-90
     "Tell me what you want to accomplish, and I'll create a step-by-step plan for us to work on together."]
    [:div.card-actions.justify-center.mt-4
     [:button.btn.btn-lg.glass
      {:on-click #(navigate! :planner)}
      "Start Planning →"]]]])

;; =============================================================================
;; HOME VIEW
;; =============================================================================

(defn home-view []
  [:div.space-y-8
   [:div.text-center.py-8
    [:h1.text-4xl.font-bold.mb-2 "io Workflow System"]
    [:p.text-lg.text-base-content/70
     "Human-AI collaborative workflows with transient UIs"]]

   ;; Main CTA: AI Planner
   [planner-cta]

   [:div.divider "or choose a template"]

   [template-gallery]

   [:div.divider]

   [saved-workflows-list]

   [:div.divider]

   [workflow-creator]])

;; =============================================================================
;; WORKFLOW VIEW
;; =============================================================================

(defn workflow-view []
  (let [id (:current-workflow @!app-state)
        manager (:manager @!app-state)
        executor (persist/get-executor manager id)]
    (if executor
      [:div
       [:div.flex.items-center.gap-4.mb-6
        [:button.btn.btn-ghost.btn-sm
         {:on-click #(navigate! :home)}
         "← Back"]
        [:h1.text-xl.font-bold.flex-1.truncate id]
        [:button.btn.btn-sm.btn-ghost
         {:on-click #(persist/save! manager id)}
         "Save"]
        [:button.btn.btn-sm.btn-ghost
         {:on-click #(persist/export-workflow manager id)}
         "Export"]]
       [ui/workflow-view
        {:executor executor
         :title nil
         :show-slots? true
         :show-graph? true}]]
      [:div.text-center.py-8
       [:p "Workflow not found"]
       [:button.btn.btn-primary.mt-4
        {:on-click #(navigate! :home)}
        "Go Home"]])))

;; =============================================================================
;; PLANNER VIEW
;; =============================================================================

(defn planner-view-wrapper []
  [:div
   [:div.flex.items-center.gap-4.mb-6.max-w-4xl.mx-auto.px-6
    [:button.btn.btn-ghost.btn-sm
     {:on-click #(navigate! :home)}
     "← Back"]]
   [planner-ui/planner-view
    {:planner (:planner @!app-state)
     :ai-client (make-smart-client)}]])

;; =============================================================================
;; MAIN APP
;; =============================================================================

(defn app []
  (init-manager!)
  (fn []
    [:div.min-h-screen.bg-base-200
     [:div.container.mx-auto.py-8.px-4
      (case (:view @!app-state)
        :home [home-view]
        :workflow [workflow-view]
        :planner [planner-view-wrapper]
        [home-view])]]))

;; =============================================================================
;; MOUNT
;; =============================================================================

(defn ^:dev/after-load mount! []
  (rdom/render [app] (js/document.getElementById "app")))

(defn init! []
  (mount!))

;; =============================================================================
;; DEV HELPERS
;; =============================================================================

(comment
  ;; Quick test in REPL
  (def !wf (io/make-reactive-workflow
             :test
             '{:topic ?topic
               :outline ($outline :ai "Outline for {?topic}" :approve)
               :draft ($draft :ai "Write draft based on {$outline}")}))

  (def ex (exec/make-executor {:!workflow !wf
                               :ai-client (make-smart-client)}))

  (exec/run! ex)

  (exec/submit-input! ex '?topic "The Future of AI")

  (exec/submit-approval! ex '$outline true)

  @!wf
  )
