^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns inside-out.test-notebook
  (:require [nextjournal.clerk :as clerk]
            [inside-out.clerk-cljs :refer [cljs]]))

^{::clerk/visibility {:code :hide}}
(cljs (require '[inside-out.forms :as forms :refer [with-form]]
               '[clojure.string :as str]))

(cljs
 (reagent.core/with-let [_ (js/console.log "mounting")
                         !a (reagent.core/atom 0)]
   (js/console.log "rendering")
   [:div {:on-click #(swap! !a inc)} @!a]))

(cljs
 (with-form [contact-info {:person/name ?name}]
             [:div
              [:input.border.p-3
               {:value @?name
                :on-change (fn [event]
                             (reset! ?name (.. event -target -value)))
                :placeholder "Your name:"}]
              [:pre (str @contact-info)]]))