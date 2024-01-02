(ns inside-out.reagent
  (:require [inside-out.forms :as forms]
            [re-db.react]
            [re-db.integrations.reagent])
  #?(:cljs (:require-macros [inside-out.reagent])))

(defmacro with-form [bindings & body]
  `(re-db.react/use-derefs
    (forms/with-form ~bindings ~@body)))