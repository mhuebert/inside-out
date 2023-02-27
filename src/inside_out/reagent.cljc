(ns inside-out.reagent
  (:require [inside-out.forms :as forms]
            [re-db.integrations.reagent]
            [reagent.core :as r])
  #?(:cljs (:require-macros [inside-out.reagent])))

(defmacro with-form [bindings & body]
  `(r/with-let [f# (forms/with-form ~bindings ~@body)]
     @f#))