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
                      [(forms/debounce 1000
                         (fn [value _]
                           (forms/message :info (str "debounce@700 " value))))
                       (forms/debounce 400
                         (fn [value _]
                           (forms/message :invalid (str "debounce@300 " value))))]}}]
    [:div
     (ui/input-text ?bar {})]))

;; Promises

(show-cljs
  (with-form [foo {:bar (?bar :init "a")}
              :meta {?bar {:validators [(fn [value context]
                                          (p/do (p/delay 1000)
                                                (forms/message :info (str "delay@700 " value))))
                                        (fn [value context]
                                          (p/do (p/delay 500)
                                                (forms/message :invalid (str "delay@300 " value))))]}}]
    [:div
     (ui/input-text ?bar {})]))

;; Promise, computed on blur

(show-cljs
  (with-form [foo {:bar (?bar :init "a")}
              :meta {?bar {:validators [(->
                                         (fn [value context]
                                           (p/do (p/delay 1000)
                                                 (prn :running-on-blur)
                                                 (forms/message :info (str "delay@400 " value))))
                                         (forms/validator :compute-when #{:blurred}))]}}]
    [:div
     (ui/input-text ?bar {})]))
