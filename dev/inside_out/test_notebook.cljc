^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns inside-out.test-notebook
  (:require [nextjournal.clerk :as-alias clerk]
            [mhuebert.clerk-cljs :refer [cljs]]
            [inside-out.forms :as forms]
            [inside-out.reagent :refer [with-form]]))

(cljs
 (with-form [foo {:bar ?bar}]
   [:<>
    [:span (str @foo)]
    [:span (forms/visible-messages foo)]
    [:input {:on-change #(reset! ?bar (.. % -target -value))
             :placeholder "type here..."
             :type :text
             :value @?bar}]
    [:input {:type :button
             :value "clear"
             :on-click #(forms/clear! foo)}]])
 )


#_(with-form [foo {:bar ?bar}]
    [:div
     [:span (str @foo)]
     [:span (str (forms/visible-messages foo))]
     [:input {:on-change #(reset! ?bar (.. % -target -value))
              :type :text
              :value @?bar}]
     [:input {:type :button
              :value "clear"
              :on-click #(forms/clear! foo)}]])