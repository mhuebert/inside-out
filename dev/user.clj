(ns user
  (:require [clojure.string :as str]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.config :as config]
            nextjournal.clerk.viewer
            [nextjournal.clerk.dev-launcher :as launcher]
            [nextjournal.clerk.viewer.builder :as builder]
            [clojure.java.shell :refer [sh]]))

;; one-time


(defn start []
  (launcher/start {:browse? true
                   :out-path "public"
                   :watch-paths ["dev"]
                   :show-filter-fn #(str/includes? % "notebooks")
                   :extra-namespaces '[inside-out.sci-config]
                   }))

(defn publish! [& _]
  (clerk/build! {:index "dev/inside_out/notebook.clj"
                 :compile-css true
                 :bundle? false
                 :out-path "public/build"
                 :extra-namespaces '[inside-out.sci-config]}))

(comment

 (do (shadow.cljs.devtools.api/stop-worker :viewer)
     (start))

 (publish!)

 (require 'nextjournal.clerk.viewer.builder :reload)
 (require 'nextjournal.clerk.builder :reload)

 (do
   ;; start a dev server on port 7999
   (defonce !server (atom nil))
   (some-> @!server future-cancel)
   (reset! !server
           (future (sh "python" "-m" "SimpleHTTPServer" "7999" :dir "public/build"))))




 (clerk/clear-cache!)


 )