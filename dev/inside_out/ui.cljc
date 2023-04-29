(ns inside-out.ui
  (:require [clojure.walk :as walk]
            [inside-out.forms :as forms]
            [inside-out.reagent]
            [inside-out.util :refer [merge-props]]
            [mhuebert.clerk-cljs]
            [re-db.reactive :as r])
  #?(:cljs (:require-macros inside-out.ui)))

(def nonbreaking-space "the &nbsp; character (for taking up space when no messages are present)"
  \u00A0)
(def empty-message {:content nonbreaking-space})

(def invalid-border-color "red")
(def invalid-text-color "red")
(def invalid-bg-color "light-pink")

(defn view-message [{:keys [type content]}]
  (when content
    [:div.text-xs.mt-1
     {:style (case type
               (:error :invalid) {:color invalid-text-color
                                  :background-color invalid-bg-color}
               nil)}
     content]))

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
          (into [:div.mt-1] (map view-message) messages))
        (when (forms/in-progress? ?field)
          "Loading...")])))

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

#?(:clj
   (defmacro with-form [bindings & body]
     ;; computes form inside a reactive sesson (for resource cleanup)
     `(r/session (forms/with-form ~bindings ~@body))))

#?(:clj
   (defmacro cljs
     ;; computes in cljs environment (browser)
     [& body]
     `(~'mhuebert.clerk-cljs/show-cljs
       (do ~@(walk/postwalk #({`with-form 'inside-out.reagent/with-form
                               'with-form 'inside-out.reagent/with-form} % %) body)))))