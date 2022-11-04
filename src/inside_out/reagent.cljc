(ns inside-out.reagent
  (:require [reagent.core]
            [inside-out.macros :as macros])
  #?(:cljs (:require-macros [inside-out.reagent])))

(defmacro with-form [bindings & body]
  (macros/with-form* (if (:ns &env) 'reagent.core/with-let 'let)
                     {}
                     bindings
                     body))