(ns user
  (:require [clojure.string :as str]
            [nextjournal.clerk :as clerk]
            nextjournal.clerk.viewer
            [nextjournal.clerk.builder.cljs :as builder]
            [clojure.java.shell :refer [sh]]))

;; one-time


(defn start []
  (builder/start-watch {:browse? true
                        :out-path "public"
                        :watch-paths ["dev"]
                        :show-filter-fn #(str/includes? % "notebooks")
                        :extra-namespaces '[inside-out.sci-config]}))

(comment

 (shadow.cljs.devtools.api/stop-worker :viewer)

 (start)

 (do
   ;; start a dev server on port 7999
   (defonce !server (atom nil))
   (some-> @!server future-cancel)
   (reset! !server
           (future (sh "python" "-m" "SimpleHTTPServer" "7999" :dir "public/build"))))




 (clerk/clear-cache!)


 )