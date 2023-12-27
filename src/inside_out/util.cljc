(ns inside-out.util
  (:refer-clojure :exclude [assoc-in update-vals])
  (:require [clojure.string :as str]
            [clojure.core :as core]))

(defn guard
  ([f] (fn [x] (when (f x) x)))
  ([x f] (when (f x) x)))

(defn first-when [f coll]
  (some (guard f) coll))

(defn assoc-in [root path value]
  (if (= path [])
    value
    (clojure.core/assoc-in root path value)))

(defn update-vals [m f]
  #?(:clj (clojure.core/update-vals m f)
     :cljs (reduce-kv (fn [m k v] (assoc m k (f v))) (empty m) m)))

(defn field-sym? [sym]
  (and (symbol? sym)
       (let [sym-str (str sym)]
         (or (str/starts-with? (name sym) "?")
             (some-> (namespace sym) (str/starts-with? "?"))

             (str/starts-with? sym-str "...")))))

(defn many-sym? [sym]
  (and (field-sym? sym) (str/starts-with? (str sym) "...")))

(defn as-str [c]
  (if (vector? c)
    (str/join " " c)
    c))

(defn merge-classes [c1 c2]
  (str (as-str c1) " " (as-str c2)))

(defn merge-props
  ([p1 p2 & more]
   (reduce merge-props (merge-props p1 p2) more))
  ([p1 p2]
   (let [p (merge p1 p2)]
     (cond-> p
             (:class p)
             (assoc :class (merge-classes (:class p1) (:class p2)))
             (:style p)
             (assoc :style (merge (:style p1) (:style p2)))))))