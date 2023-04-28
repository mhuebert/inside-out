^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns inside-out.test-notebook
  (:require [nextjournal.clerk :as-alias clerk]
            [mhuebert.clerk-cljs :refer [show-cljs]]
            [inside-out.forms :as forms]
            [inside-out.reagent :refer [with-form]]
            [inside-out.ui :as ui]
            [promesa.core :as p]))

;; Debounce

(show-cljs
  (with-form [foo {:bar (?bar :init "a")}
              :meta {?bar
                     {:validators
                      [(-> (fn [value _]
                             (forms/message :info (str "debounce@1000 " value)))
                           (forms/debounce 1000))
                       (-> (fn [value _]
                             (forms/message :invalid (str "debounce@400  " value)))
                           (forms/debounce 400))]}}]
    [:div.font-mono
     (ui/input-text ?bar {})]))

;; Promises

(show-cljs
  (with-form [foo {:bar (?bar :init "a")}
              :meta {?bar {:validators [(fn [value context]
                                          (p/do (p/delay 1000)
                                                (forms/message :info (str "delay@1000 " value))))
                                        (fn [value context]
                                          (p/do (p/delay 500)
                                                (forms/message :invalid (str "delay@500  " value))))]}}]
    [:div.font-mono
     (ui/input-text ?bar {})]))

;; Promise, computed on blur

(show-cljs
  (with-form [foo {:bar (?bar :init "a")}
              :meta {?bar {:validators [(-> (fn [value context]
                                              (p/do (p/delay 1000)
                                                    (forms/message :info (str "delay@1000 " value))))
                                            (forms/on-blur))]}}]
    [:div.font-mono
     (ui/input-text ?bar {})]))

;; A debounced promise

(show-cljs
  (with-form [foo {:bar (?bar :init "a")}
              :meta {?bar {:validators [(-> (fn [value context]
                                              (p/do (p/delay 1000)
                                                    (forms/message :info (str "delay@1000 " value))))
                                            (forms/debounce 300))]}}]
    [:div.font-mono
     (ui/input-text ?bar {})]))

