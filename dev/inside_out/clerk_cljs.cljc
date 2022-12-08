(ns inside-out.clerk-cljs
  (:require #?(:cljs ["@nextjournal/lang-clojure"])
            #?(:cljs [nextjournal.clerk.render.hooks :as hooks])
            applied-science.js-interop
            [applied-science.js-interop :as j]
            [clojure.walk :as walk]
            [nextjournal.clerk :as-alias clerk])
  #?(:cljs (:require-macros inside-out.clerk-cljs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clerk ClojureScript/Reagent viewer
;;
;; (for using compiled ClojureScript in a notebook)


(defn stable-hash-form
  "Replaces gensyms and regular expressions with stable symbols for consistent hashing"
  [form]
  (let [!counter (atom 0)
        !syms (atom {})]
    (walk/postwalk (fn [x]
                     (cond #?(:cljs (regexp? x)
                              :clj  (instance? java.util.regex.Pattern x))
                           (symbol (str "stable-regexp-" (swap! !counter inc)))
                           (and (symbol? x)
                                (not (namespace x)))
                           (or (@!syms x)
                               (let [y (symbol (str "stable-symbol-" (swap! !counter inc)))]
                                 (swap! !syms assoc x y)
                                 y))
                           :else x)) form)))

(def stable-hash (comp hash stable-hash-form))

(defmacro cljs
  "Evaluate expressions in ClojureScript instead of Clojure.
   Result is treated as hiccup if it is a vector (unless tagged with ^:inspect),
   otherwise passed to Clerk's `inspect`."
  [& exprs]
  (let [fn-name (stable-hash exprs)]
    (if (:ns &env)
      ;; in ClojureScript, define a function
      `(let [f# (fn [] ~@exprs)]
         (j/update-in! ~'js/window [:clerk-cljs ~fn-name]
                       (fn [x#]
                         (cond (not x#) (reagent.core/atom {:f f#})
                               (:loading? @x#) (doto x# (reset! {:f f#}))
                               :else x#))))
      ;; in Clojure, return a map with a reference to the fully qualified sym
      `(clerk/with-viewer
         {:transform-fn nextjournal.clerk/mark-presented
          :render-fn '(fn render-var []
                        ;; ensure that a reagent atom exists for this fn
                        (applied-science.js-interop/update-in!
                         js/window [:clerk-cljs ~fn-name] (fn [x] (or x (reagent.core/atom {:loading? true}))))
                        ;; when we stop using this
                        (nextjournal.clerk.render.hooks/use-effect
                         (constantly
                          #(j/assoc-in! js/window [:clerk-cljs ~fn-name] nil))
                         [~fn-name])
                        (let [res @(j/get-in js/window [:clerk-cljs ~fn-name])]
                          (if (:loading? res)
                            [:div.my-2 {:style {:color "rgba(0,0,0,0.5)"}} "Loading..."]
                            (let [result (try ((:f res))
                                              (catch js/Error e
                                                (js/console.error e)
                                                [nextjournal.clerk.render/error-view e]))]
                              (if (and (vector? result)
                                       (not (:inspect (meta result))))
                                [:div.my-1 result]
                                [nextjournal.clerk.render/inspect result])))))}
         nil))))