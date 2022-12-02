(ns inside-out.clerk-cljs
  (:require applied-science.js-interop
            #?(:cljs ["@nextjournal/lang-clojure"])
            [nextjournal.clerk :as-alias clerk]
            [applied-science.js-interop :as j])
  #?(:cljs (:require-macros inside-out.clerk-cljs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clerk ClojureScript/Reagent viewer
;;
;; (for using compiled ClojureScript in a notebook)

;; our API is a `hiccup` macro which will compile the contents as ClojureScript
;; and render it using Reagent.

(defmacro cljs
  "Evaluate expressions in ClojureScript instead of Clojure. If the result is
   a vector, it is passed to Reagent and interpreted as hiccup."
  [& exprs]
  (let [fn-name (str "cljs_fn_" (hash exprs))]
    (if (:ns &env)
      ;; in ClojureScript, define a function
      `(j/assoc! ~'js/window ~fn-name (fn [] ~@exprs))
      ;; in Clojure, return a map with a reference to the fully qualified sym
      `(clerk/with-viewer
         {:transform-fn nextjournal.clerk/mark-presented
          :render-fn '(fn render-var [_]
                        (when-let [cljs-fn (j/get js/window ~fn-name)]
                          (try (let [result (cljs-fn)]
                                 (if (and (vector? result)
                                          (not (:vector (meta result))))
                                   [:div.my-1 result]
                                   [nextjournal.clerk.render/inspect result]))
                               (catch js/Error e (str e)))))}
         nil))))
