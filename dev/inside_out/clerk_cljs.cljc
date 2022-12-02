(ns inside-out.clerk-cljs
  (:require #?(:cljs ["@nextjournal/lang-clojure"])
            applied-science.js-interop
            [applied-science.js-interop :as j]
            [clojure.walk :as walk]
            [nextjournal.clerk :as-alias clerk])
  #?(:cljs (:require-macros inside-out.clerk-cljs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clerk ClojureScript/Reagent viewer
;;
;; (for using compiled ClojureScript in a notebook)

;; our API is a `hiccup` macro which will compile the contents as ClojureScript
;; and render it using Reagent.


(defn stable-form

  [form]
  (let [!counter (atom 0)
        !syms (atom {})]
    (walk/postwalk (fn [x]

                     (cond #?(:cljs (regexp? x)
                              :clj (instance? java.util.regex.Pattern x))
                           (str "regexp-" (swap! !counter inc))
                           (and (symbol? x)
                                (not (namespace x)))
                           (or (@!syms x)
                               (let [y (symbol (str "symbol-" (swap! !counter inc)))]
                                 (swap! !syms assoc x y)
                                 y))
                           :else x)) form)))

(defmacro cljs
  "Evaluate expressions in ClojureScript instead of Clojure. If the result is
   a vector, it is passed to Reagent and interpreted as hiccup."
  [& exprs]
  (let [fn-name (str "cljs_fn_" (hash (stable-form exprs)))]
    (if (:ns &env)
      ;; in ClojureScript, define a function
      `(j/assoc! ~'js/window ~fn-name (fn [] ~@exprs))
      ;; in Clojure, return a map with a reference to the fully qualified sym
      `(clerk/with-viewer
         {:transform-fn nextjournal.clerk/mark-presented
          :render-fn '(fn render-var [_]
                        (if-let [cljs-fn (j/get js/window ~fn-name)]
                          (let [result (try (cljs-fn)
                                            (catch js/Error e
                                              (js/console.error e)
                                              [nextjournal.clerk.render/error-view e]))]
                            (if (and (vector? result)
                                     (not (:vector (meta result))))
                              [:div.my-1 result]
                              [nextjournal.clerk.render/inspect result]))
                          "cljs fn not defined (try reloading)"))}
         nil))))
