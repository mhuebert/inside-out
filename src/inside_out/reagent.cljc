(ns inside-out.reagent
  (:require [inside-out.forms :as forms]
            [re-db.react :refer [use-reactive]]
            [re-db.integrations.reagent])
  #?(:cljs (:require-macros [inside-out.reagent])))

(defmacro with-form [bindings & body]
  `(use-reactive
    (forms/with-form ~bindings ~@body)))