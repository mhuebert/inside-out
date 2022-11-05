(ns inside-out.sci-config
  (:require [sci.core :as sci]
            [nextjournal.clerk.sci-env :refer [!sci-ctx]]
            [inside-out.ui]
            [reagent.core]
            [inside-out.forms]
            [inside-out.macros :as macros]
            [inside-out.reagent :as reagent]
            [sci.configs.funcool.promesa :as promesa.sci]))

(defn ^:macro with-form [&form &env bindings & body]
  (macros/with-form* 'reagent.core/with-let
                     {}
                     bindings
                     body))

(defn ^:macro form [&form &env expr & {:as options}]
  (macros/form* &form &env expr options))

(defn ^:macro try-submit+ [&form &env form submit-expr]
  (inside-out.forms/try-submit+* true form submit-expr))

(def namespaces
  {'inside-out.forms
   (let [sns (sci/create-ns 'inside-out.forms)]
     (merge (sci/copy-ns inside-out.forms sns)
            {'form (sci/copy-var form sns)
             'with-form (sci/copy-var with-form sns)
             'try-submit+ (sci/copy-var try-submit+ sns)}))
   'inside-out.ui (sci/copy-ns inside-out.ui (sci/create-ns 'inside-out.ui))})

(swap! !sci-ctx
       sci/merge-opts
       {:namespaces (merge namespaces
                           (:namespaces promesa.sci/config))
        :bindings {'println println
                   'prn prn}
        :aliases {'forms 'inside-out.forms}})