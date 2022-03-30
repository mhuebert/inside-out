(ns ^:nextjournal.clerk/no-cache inside-out.clerk-ui
  (:require applied-science.js-interop
            [clojure.string :as str]
            [inside-out.forms :as forms]
            [inside-out.util :refer [merge-props]]
            [nextjournal.clerk.viewer :as viewer])
  #?(:cljs (:require-macros inside-out.clerk-ui)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clerk ClojureScript/Reagent viewer
;;
;; (for using compiled ClojureScript in a notebook)

;; our API is a `hiccup` macro which will compile the contents as ClojureScript
;; and render it using Reagent.
(defmacro cljs
  "Evaluate expressions in ClojureScript instead of Clojure. If the result is
   a vector, it is passed to Reagent and interpreted as hiccup."
  [& exprs]
  (let [name (symbol (str "reagent-view-" (hash exprs)))]
    (if (:ns &env)
      ;; in ClojureScript, define a function
      `(defn ~(with-meta name {:export true}) [] ~@exprs)
      ;; in Clojure, return a map with a reference to the fully qualified sym
      {:reagent/var `'~(symbol (str *ns*) (str name))})))

(def reagent-viewer
  (viewer/process-render-fn
   {:pred #(and (map? %) (contains? % :reagent/var))
    :fetch-fn (fn [_ x] x)
    :render-fn '(fn render-var [{var :reagent/var}]
                  (let [path (->> (str/split (str var) #"[./]")
                                  (mapv munge))
                        reagent-fn (applied-science.js-interop/get-in js/window path)
                        wrapper (fn [f] (let [result (f)]
                                          (if (vector? result)
                                            result
                                            (v/inspect result))))]
                    (when reagent-fn
                      (v/html [:div.my-1 [wrapper reagent-fn]]))))}))


(defn setup-viewers! []
  (swap! viewer/!viewers update :root #(into [reagent-viewer] %)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Form-specific UI stuff

(def invalid-border-color "red")
(def invalid-text-color "red")
(def invalid-bg-color "light-pink")

(defn view-message [{:keys [type content]}]
  [:div.text-xs.mt-1
   {:style (case type
             (:error :invalid) {:color invalid-text-color
                                :background-color invalid-bg-color}
             nil)}
   content])

(def ring-color-invalid
  {"--tw-ring-color" invalid-border-color})

;; helper hiccup elements
(def input-text-element
  :input.inline-flex.justify-center.rounded-md.border.border-gray-300.shadow-sm.bg-white.font-medium.text-gray-700.focus:outline-none.focus:ring-2.focus:ring-offset-2.focus:ring-indigo-500.sm:mt-0.sm:col-start-1.px-4.py-2.text-base.sm:text-sm)
(def show-code :pre.whitespace-pre-wrap.text-sm.my-3.text-xs)
(def label-small :div.mt-3.text-gray-500.font-bold)

#?(:cljs
   (defn input-text
     "A text-input element that reads metadata from a ?field to display appropriately"
     [?field attrs]
     (let [messages (forms/visible-messages ?field)]
       [:<>
        [input-text-element
         (merge-props {:placeholder (:label ?field)
                       :value @?field
                       :on-change (forms/change-handler ?field)
                       :on-blur (forms/blur-handler ?field)
                       :on-focus (forms/focus-handler ?field)
                       :class (when (:invalid (forms/types messages))
                                "ring-2 ring-offset-2 ring-red-500 focus:ring-red-500")}
                      attrs)]
        (when (seq messages)
          (into [:div.mt-1] (map view-message) messages))])))

#?(:cljs
   (defn input-checkbox
     "A text-input element that reads metadata from a ?field to display appropriately"
     [?field attrs]
     (let [messages (forms/visible-messages ?field)]
       [:<>
        [:input
         (merge-props {:type "checkbox"
                       :placeholder (:label ?field)
                       :checked (boolean @?field)
                       :on-change (fn [^js e] (.persist e) (js/console.log e) (reset! ?field (.. e -target -checked)))
                       :on-blur (forms/blur-handler ?field)
                       :on-focus (forms/focus-handler ?field)
                       :class (when (:invalid (forms/types messages))
                                "ring-2 ring-offset-2 ring-red-500 focus:ring-red-500")}
                      attrs)]
        (when (seq messages)
          (into [:div.mt-1] (map view-message) messages))])))

