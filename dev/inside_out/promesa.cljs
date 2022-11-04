(ns inside-out.promesa
  (:refer-clojure :exclude [delay spread promise

                            await map mapcat run!
                            future let loop recur -> ->>
                            with-redefs
                            doseq])
  (:require [clojure.core :as c]
            [promesa.core :as p]
            [promesa.exec :as exec]
            [promesa.protocols :as pt]
            [sci.core :as sci]))

(def pns (sci/create-ns 'promesa.core nil))
(def ptns (sci/create-ns 'promesa.protocols nil))

(defn ^:macro do!
  "Execute potentially side effectful code and return a promise resolved
  to the last expression. Always awaiting the result of each
  expression."
  [_ _ & exprs]
  `(pt/-bind
    (pt/-promise nil)
    (fn [_#]
      ~(condp = (count exprs)
         0 `(pt/-promise nil)
         1 `(pt/-promise ~(first exprs))
         (reduce (fn [acc e]
                   `(pt/-bind (pt/-promise ~e) (fn [_#] ~acc)))
                 `(pt/-promise ~(last exprs))
                 (reverse (butlast exprs)))))))

(defn ^:macro let
  "A `let` alternative that always returns promise and waits for all the
  promises on the bindings."
  [_ _ bindings & body]
  `(pt/-bind
    (pt/-promise nil)
    (fn [_#]
      ~(c/->> (reverse (partition 2 bindings))
              (reduce (fn [acc [l r]]
                        `(pt/-bind (pt/-promise ~r) (fn [~l] ~acc)))
                      `(promesa.core/do! ~@body))))))

(defn ^:macro ->
  "Like the clojure.core/->, but it will handle promises in values
  and make sure the next form gets the value realized instead of
  the promise. Example using to fetch data in the browser with CLJS:
  Example:
  (p/-> (js/fetch #js {...}) ; returns a promise
        .-body)
  The result of a thread is a promise that will resolve to the
  end of the thread chain."
  [_ _ x & forms]
  (c/let [fns (mapv (fn [arg]
                      (c/let [[f & args] (if (sequential? arg)
                                           arg
                                           (list arg))]
                        `(fn [p#] (~f p# ~@args)))) forms)]
    `(p/chain (p/promise ~x) ~@fns)))

(defn ^:macro ->>
  "Like the clojure.core/->>, but it will handle promises in values
  and make sure the next form gets the value realized instead of
  the promise. Example using to fetch data in the browser with CLJS:
  Example:
  (p/->> (js/fetch #js {...}) ; returns a promise
         .-body
         read-string
         (mapv inc)
  The result of a thread is a promise that will resolve to the
  end of the thread chain."
  [_ _ x & forms]
  (c/let [fns (mapv (fn [arg]
                      (c/let [[f & args] (if (sequential? arg)
                                           arg
                                           (list arg))]
                        `(fn [p#] (~f ~@args p#)))) forms)]
    `(p/chain (p/promise ~x) ~@fns)))

(defn ^:macro with-redefs
  "Like clojure.core/with-redefs, but it will handle promises in
  body and wait until they resolve or reject before restoring the
  bindings. Useful for mocking async APIs.
  Example:
  (defn async-func [] (p/delay 1000 :slow-original))
  (p/with-redefs [async-func (fn [] (p/resolved :fast-mock))]
    (async-func))
  The result is a promise that will resolve to the last body form and
  upon resolving restores the bindings to their original values."
  [_ _ bindings & body]
  (c/let [names (take-nth 2 bindings)
          vals (take-nth 2 (drop 1 bindings))
          orig-val-syms (c/map (comp gensym #(str % "-orig-val__") name) names)
          temp-val-syms (c/map (comp gensym #(str % "-temp-val__") name) names)
          binds (c/map vector names temp-val-syms)
          resets (reverse (c/map vector names orig-val-syms))
          bind-value (fn [[k v]]
                       (list 'clojure.core/alter-var-root (list 'var k)
                             (list 'clojure.core/constantly v)))]
    `(c/let [~@(c/interleave orig-val-syms names)
             ~@(c/interleave temp-val-syms vals)]
       ~@(c/map bind-value binds)
       (p/-> (p/do! ~@body)
             (p/finally
              (fn []
                ~@(c/map bind-value resets)))))))

(def ^:private
  loop-run-fn (sci/new-dynamic-var '*loop-run-fn* exec/run! {:ns pns}))

(defn ^:macro loop
  [_ _ bindings & body]
  (c/let [bindings (partition 2 2 bindings)
          names (mapv first bindings)
          fvals (mapv second bindings)
          tsym (gensym "loop")
          dsym (gensym "deferred")
          rsym (gensym "run")]
    `(c/let [~rsym promesa.core/*loop-run-fn*
             ~dsym (promesa.core/deferred)
             ~tsym (fn ~tsym [params#]
                     (c/-> (promesa.core/all params#)
                           (promesa.core/then (fn [[~@names]]
                                                ;; (prn "exec" ~@names)
                                                (promesa.core/do! ~@body)))
                           (promesa.core/handle
                            (fn [res# err#]
                              ;; (prn "result" res# err#)
                              (cond
                                (not (nil? err#))
                                (promesa.core/reject! ~dsym err#)

                                (and (map? res#) (= (:type res#) :promesa.core/recur))
                                (do (~rsym (fn [] (~tsym (:args res#))))
                                    nil)

                                :else
                                (promesa.core/resolve! ~dsym res#))))))]
       (~rsym (fn [] (~tsym ~fvals)))
       ~dsym)))

(defn ^:macro recur
  [_ _ & args]
  `(array-map :type :promesa.core/recur :args [~@args]))

(defn ^:macro doseq
  "Simplified version of `doseq` which takes one binding and a seq, and
  runs over it using `promesa.core/run!`"
  [_ _ [binding xs] & body]
  `(promesa.core/run!
    (fn [~binding]
      (promesa.core/do ~@body))
    ~xs))

(def promesa-namespace
  {'*loop-run-fn* loop-run-fn
   '->            (sci/copy-var -> pns)
   '->>           (sci/copy-var ->> pns)
   'all           (sci/copy-var p/all pns)
   'any           (sci/copy-var p/any pns)
   'catch         (sci/copy-var p/catch pns)
   'chain         (sci/copy-var p/chain pns)
   'create        (sci/copy-var p/create pns)
   'deferred      (sci/copy-var p/deferred pns)
   'delay         (sci/copy-var p/delay pns)
   'do            (sci/copy-var do! pns)
   'do!           (sci/copy-var do! pns)
   'error         (sci/copy-var p/error pns)
   'finally       (sci/copy-var p/finally pns)
   'handle        (sci/copy-var p/handle pns)
   'let           (sci/copy-var let pns)
   'loop          (sci/copy-var loop pns)
   'map           (sci/copy-var p/map pns)
   'mapcat        (sci/copy-var p/mapcat pns)
   'promise       (sci/copy-var p/promise pns)
   'promise?      (sci/copy-var p/promise? pns)
   'race          (sci/copy-var p/race pns)
   'recur         (sci/copy-var recur pns)
   'reject!       (sci/copy-var p/reject! pns)
   'rejected      (sci/copy-var p/rejected pns)
   'resolve!      (sci/copy-var p/resolve! pns)
   'resolved      (sci/copy-var p/resolved pns)
   'run!          (sci/copy-var p/run! pns)
   'then          (sci/copy-var p/then pns)
   'thenable?     (sci/copy-var p/thenable? pns)
   'with-redefs   (sci/copy-var with-redefs pns)
   'wrap          (sci/copy-var p/wrap pns)
   'doseq         (sci/copy-var doseq pns)})

(def promesa-protocols-namespace
  {'-bind (sci/copy-var pt/-bind ptns)
   '-promise (sci/copy-var pt/-promise ptns)})

(def namespaces {'promesa.core promesa-namespace
                 'promesa.protocols promesa-protocols-namespace})

(def config {:namespaces namespaces})