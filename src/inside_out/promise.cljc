(ns inside-out.promise
  "Minimal copy of kitchen-async, or a poor man's poor man's core.async."
  ;; https://github.com/athos/kitchen-async
  (:refer-clojure :exclude [do try -> let when cond])
  (:require [applied-science.js-interop :as j]
            [inside-out.util :refer [guard]]
            [clojure.core :as core])
  #?(:cljs (:require-macros inside-out.promise)))

#?(:cljs
   (defn promise? [x] (js-fn? (some-> x (unchecked-get "then")))))

#?(:cljs
   (defn then [^js p f]
     (if (promise? p)
       (.then p f)
       (f p)))
   :clj
   (defn then [p f] (f p)))

(defn catch [p handle-e]
  #?(:cljs (.catch p handle-e)
     :clj  p))

(defn- do* [exprs]
  (if (< (count exprs) 2)
    (first exprs)
    (letfn [(rec [expr exprs]
              (if (empty? exprs)
                expr
                `(then ~expr (fn [_#] ~(rec (first exprs) (rest exprs))))))]
      (rec (first exprs) (rest exprs)))))

(defmacro do [& body]
  (if (:ns &env)
    (do* body)
    `(clojure.core/do ~@body)))

(defmacro let [bindings & body]
  (letfn [(rec [[name init & bindings] body]
            (if name
              `(then ~init (fn [~name] ~(rec bindings body)))
              (do* body)))]
    (rec bindings body)))

(defmacro -> [x & forms]
  (if forms
    (core/let [[form & forms] forms]
      `(-> (then ~x
                 ~(clojure.core/cond
                    (seq? form)
                    `(fn [v#] (~(first form) v# ~@(rest form)))

                    (or (keyword? form)
                        (and (symbol? form) (= \. (first (name form)))))
                    `(fn [v#] (~form v#))

                    :else form))
           ~@forms))
    x))

(defmacro if [condition x y]
  `(let [res# ~condition]
     (clojure.core/if res# ~x ~y)))

(defmacro when [condition & body]
  `(if ~condition (do ~@body) nil))

(defmacro cond [& conditions]
  (loop [conditions (partition 2 (reverse conditions))
         out nil]
    (if (seq conditions)
      (core/let [[test result] (first conditions)]
        (recur (rest conditions)
               `(~'inside-out.promise/if ~test ~result ~out)))
      out)))

(defn timeout [ms]
  #?(:cljs
     (js/Promise. (fn [resolve reject] (js/setTimeout resolve ms)))))