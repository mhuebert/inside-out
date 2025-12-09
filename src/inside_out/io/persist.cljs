(ns inside-out.io.persist
  "Persistence layer for io workflows

   Supports:
   - LocalStorage persistence for browser
   - JSON serialization
   - Workflow resume from saved state
   - Auto-save on state changes"
  (:require [clojure.string :as str]
            [inside-out.io :as io]
            [inside-out.io.executor :as exec]
            [re-db.reactive :as r]))

;; =============================================================================
;; STORAGE PROTOCOL
;; =============================================================================

(defprotocol IWorkflowStorage
  "Protocol for workflow persistence"
  (save-workflow [this id data]
    "Save workflow data. Returns promise.")
  (load-workflow [this id]
    "Load workflow data. Returns promise resolving to data or nil.")
  (delete-workflow [this id]
    "Delete workflow data. Returns promise.")
  (list-workflows [this]
    "List all saved workflow IDs. Returns promise."))

;; =============================================================================
;; LOCALSTORAGE IMPLEMENTATION
;; =============================================================================

(def ^:private storage-prefix "io-workflow:")

(defn- storage-key [id]
  (str storage-prefix id))

(defrecord LocalStorageBackend []
  IWorkflowStorage

  (save-workflow [_ id data]
    (js/Promise.resolve
      (try
        (let [json (js/JSON.stringify (clj->js data))]
          (.setItem js/localStorage (storage-key id) json)
          true)
        (catch js/Error e
          (js/console.error "Failed to save workflow:" e)
          false))))

  (load-workflow [_ id]
    (js/Promise.resolve
      (try
        (when-let [json (.getItem js/localStorage (storage-key id))]
          (js->clj (js/JSON.parse json) :keywordize-keys true))
        (catch js/Error e
          (js/console.error "Failed to load workflow:" e)
          nil))))

  (delete-workflow [_ id]
    (js/Promise.resolve
      (.removeItem js/localStorage (storage-key id))
      true))

  (list-workflows [_]
    (js/Promise.resolve
      (let [keys (for [i (range (.-length js/localStorage))
                       :let [k (.key js/localStorage i)]
                       :when (str/starts-with? k storage-prefix)]
                   (subs k (count storage-prefix)))]
        (vec keys)))))

(defn make-local-storage []
  (->LocalStorageBackend))

;; =============================================================================
;; IN-MEMORY STORAGE (for testing)
;; =============================================================================

(defrecord MemoryStorageBackend [!store]
  IWorkflowStorage

  (save-workflow [_ id data]
    (js/Promise.resolve
      (do (swap! !store assoc id data)
          true)))

  (load-workflow [_ id]
    (js/Promise.resolve
      (get @!store id)))

  (delete-workflow [_ id]
    (js/Promise.resolve
      (do (swap! !store dissoc id)
          true)))

  (list-workflows [_]
    (js/Promise.resolve
      (vec (keys @!store)))))

(defn make-memory-storage []
  (->MemoryStorageBackend (atom {})))

;; =============================================================================
;; WORKFLOW MANAGER
;; =============================================================================

(defrecord WorkflowManager
  [storage           ; IWorkflowStorage implementation
   !active-workflows ; Atom<Map<id, executor>>
   !index])          ; Atom<Map<id, summary>>

(defn make-manager
  "Create a workflow manager with the given storage backend"
  ([] (make-manager (make-local-storage)))
  ([storage]
   (->WorkflowManager
     storage
     (atom {})
     (atom {}))))

(defn- update-index!
  "Update the index with workflow summary"
  [{:keys [!index]} id !workflow]
  (let [wf @!workflow]
    (swap! !index assoc id
           {:id id
            :progress (io/workflow-progress wf)
            :complete? (io/workflow-complete? wf)
            :updated-at (str (:updated-at wf))
            :slot-count (count (:slots wf))})))

;; =============================================================================
;; PERSISTENCE OPERATIONS
;; =============================================================================

(defn save!
  "Save a workflow to storage"
  [{:keys [storage !active-workflows !index]} id]
  (when-let [executor (get @!active-workflows id)]
    (let [!workflow (:!workflow executor)
          data (io/serialize-workflow @!workflow)]
      (save-workflow storage id data))))

(defn load!
  "Load a workflow from storage and create an executor"
  [{:keys [storage !active-workflows] :as manager} id & {:keys [ai-client]}]
  (-> (load-workflow storage id)
      (.then (fn [data]
               (when data
                 (let [workflow (io/deserialize-workflow data)
                       !workflow (r/atom workflow)
                       executor (exec/make-executor
                                  {:!workflow !workflow
                                   :ai-client ai-client})]
                   (swap! !active-workflows assoc id executor)
                   (update-index! manager id !workflow)
                   executor))))))

(defn create!
  "Create a new workflow and optionally save it"
  [{:keys [storage !active-workflows] :as manager} id form & {:keys [ai-client auto-save?]}]
  (let [!workflow (io/make-reactive-workflow id form)
        executor (exec/make-executor
                   {:!workflow !workflow
                    :ai-client ai-client})]
    (swap! !active-workflows assoc id executor)
    (update-index! manager id !workflow)

    ;; Set up auto-save if requested
    (when auto-save?
      (add-watch !workflow ::auto-save
                 (fn [_ _ _ _]
                   (save! manager id))))

    ;; Initial save
    (save! manager id)
    executor))

(defn delete!
  "Delete a workflow from storage and memory"
  [{:keys [storage !active-workflows !index]} id]
  (when-let [executor (get @!active-workflows id)]
    (remove-watch (:!workflow executor) ::auto-save))
  (swap! !active-workflows dissoc id)
  (swap! !index dissoc id)
  (delete-workflow storage id))

(defn list-saved
  "List all saved workflows"
  [{:keys [storage]}]
  (list-workflows storage))

(defn get-executor
  "Get the executor for an active workflow"
  [{:keys [!active-workflows]} id]
  (get @!active-workflows id))

(defn get-index
  "Get the workflow index (summaries of all known workflows)"
  [{:keys [!index]}]
  @!index)

;; =============================================================================
;; AUTO-SAVE SETUP
;; =============================================================================

(defn enable-auto-save!
  "Enable auto-save for a workflow"
  [{:as manager :keys [!active-workflows]} id]
  (when-let [executor (get @!active-workflows id)]
    (add-watch (:!workflow executor) ::auto-save
               (fn [_ _ old new]
                 (when (not= old new)
                   (save! manager id))))))

(defn disable-auto-save!
  "Disable auto-save for a workflow"
  [{:keys [!active-workflows]} id]
  (when-let [executor (get @!active-workflows id)]
    (remove-watch (:!workflow executor) ::auto-save)))

;; =============================================================================
;; WORKFLOW TEMPLATES
;; =============================================================================

(def templates
  "Pre-defined workflow templates"
  {:research
   {:name "Research Assistant"
    :description "AI-assisted research and synthesis"
    :form '{:topic (?topic :ask "What would you like to research?")
            :sources ($sources :ai "Find 5 key sources about {?topic}" :approve)
            :summary ($summary :ai "Synthesize findings from {$sources}")
            :questions (?questions :ask "What questions remain?")
            :answers ($answers :ai "Answer {?questions} based on {$sources}")
            :report ($report :ai "Compile final report on {?topic}")}}

   :writing
   {:name "Writing Assistant"
    :description "AI-assisted writing with revision"
    :form '{:topic (?topic :ask "What would you like to write about?")
            :audience (?audience :ask "Who is your target audience?")
            :outline ($outline :ai "Create outline for {?topic} targeting {?audience}" :approve)
            :draft ($draft :ai "Write first draft following {$outline}" :approve)
            :feedback (?feedback :ask "What improvements would you suggest?")
            :revision ($revision :ai "Revise {$draft} based on: {?feedback}")
            :final ($final :ai "Polish and finalize {$revision}")}}

   :brainstorm
   {:name "Brainstorming Session"
    :description "AI-assisted ideation and evaluation"
    :form '{:challenge (?challenge :ask "What problem are you trying to solve?")
            :constraints (?constraints :ask "What constraints should we consider?")
            :ideas ($ideas :ai "Generate 10 creative solutions for {?challenge} given {?constraints}" :approve)
            :evaluation ($evaluation :ai "Evaluate pros/cons of each idea in {$ideas}")
            :selection (?selection :ask "Which ideas would you like to pursue?")
            :plan ($plan :ai "Create action plan for {?selection}")}}

   :planning
   {:name "Project Planner"
    :description "AI-assisted project planning"
    :form '{:goal (?goal :ask "What is your project goal?")
            :timeline (?timeline :ask "What is your timeline?")
            :resources (?resources :ask "What resources do you have?")
            :breakdown ($breakdown :ai "Break {?goal} into milestones for {?timeline}" :approve)
            :tasks ($tasks :ai "Detail tasks for each milestone in {$breakdown}")
            :risks ($risks :ai "Identify risks and mitigations for {$breakdown}")
            :plan ($plan :ai "Compile project plan with {$breakdown}, {$tasks}, {$risks}")}}})

(defn create-from-template!
  "Create a new workflow from a template"
  [manager template-key & {:keys [id ai-client auto-save?]}]
  (when-let [template (get templates template-key)]
    (let [wf-id (or id (str (name template-key) "-" (random-uuid)))]
      (create! manager wf-id (:form template)
               :ai-client ai-client
               :auto-save? auto-save?))))

;; =============================================================================
;; EXPORT/IMPORT
;; =============================================================================

(defn export-workflow
  "Export workflow state as a downloadable JSON"
  [{:keys [!active-workflows]} id]
  (when-let [executor (get @!active-workflows id)]
    (let [data (io/serialize-workflow @(:!workflow executor))
          json (js/JSON.stringify (clj->js data) nil 2)
          blob (js/Blob. #js [json] #js {:type "application/json"})
          url (js/URL.createObjectURL blob)
          a (js/document.createElement "a")]
      (set! (.-href a) url)
      (set! (.-download a) (str "workflow-" id ".json"))
      (.click a)
      (js/URL.revokeObjectURL url))))

(defn import-workflow!
  "Import workflow from JSON file"
  [manager file & {:keys [ai-client auto-save?]}]
  (js/Promise.
    (fn [resolve reject]
      (let [reader (js/FileReader.)]
        (set! (.-onload reader)
              (fn [e]
                (try
                  (let [json (.. e -target -result)
                        data (js->clj (js/JSON.parse json) :keywordize-keys true)
                        id (:id data)
                        workflow (io/deserialize-workflow data)
                        !workflow (r/atom workflow)
                        executor (exec/make-executor
                                   {:!workflow !workflow
                                    :ai-client ai-client})]
                    (swap! (:!active-workflows manager) assoc id executor)
                    (update-index! manager id !workflow)
                    (when auto-save?
                      (enable-auto-save! manager id))
                    (resolve executor))
                  (catch js/Error e
                    (reject e)))))
        (.readAsText reader file)))))
