(ns user
  (:require [nextjournal.clerk :as clerk]
            [nextjournal.clerk.config :as config]
            nextjournal.clerk.viewer
            [clojure.string :as str]
            [nextjournal.clerk.dev-launcher :as launcher]))

(swap! config/!resource->url merge {"/js/viewer.js" "/js/viewer.js"})

(defn start []
  (launcher/start {:browse? true
                   :watch-paths ["dev"]
                   :show-filter-fn #(str/includes? % "notebooks")
                   :extra-namespaces '[inside-out.sci-config]})
  (Thread/sleep 500)
  (clerk/show! "dev/inside_out/notebook.clj"))

(defn publish! [_]
  (clerk/build-static-app! {:paths ["dev/inside_out/notebook.clj"]
                            :bundle? false
                            :browse? false
                            :out-path "public"}))

(comment


 (do
   (clerk/clear-cache!)
   (shadow.cljs.devtools.api/stop-worker :viewer)
   (start))

 (do (clerk/clear-cache!)
     (clerk/show! "dev/inside_out/test_notebook.clj"))

 (publish!)
 )