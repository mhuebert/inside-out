(ns ^:nextjournal.clerk/no-cache inside-out.clerk-cljs
  (:require applied-science.js-interop
            [nextjournal.clerk :as clerk]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clerk ClojureScript/Reagent viewer
;;
;; (for using compiled ClojureScript in a notebook)

;; our API is a `hiccup` macro which will compile the contents as ClojureScript
;; and render it using Reagent.

(defmacro cljs
  "Evaluates `exprs` in a ClojureScript sci context. If result is a vector, it is
   interpreted as hiccup (-> React)."
  [& exprs]
  `(clerk/with-viewer
     {:render-fn '(fn [_# _#]
                    (nextjournal.clerk.render/render-reagent
                     [(fn []
                        (let [result# (do ~@exprs)]
                          (if (vector? result#)
                            result#
                            [nextjournal.clerk.render/inspect result#])))]))}
     {}
     nil))
