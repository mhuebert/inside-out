(ns user
  (:require [nextjournal.clerk :as clerk]
            [nextjournal.clerk.config :as config]
            [inside-out.clerk-ui :as io-clerk]
            nextjournal.clerk.viewer
            [clojure.string :as str]))

(defn start []
  (io-clerk/setup-viewers!)
  (swap! config/!resource->url merge {"/js/viewer.js" "http://localhost:8765/js/main.js"})
  (clerk/serve! {:browse? true
                 :watch-paths ["dev"]
                 :show-filter-fn #(str/includes? % "notebooks")})
  (Thread/sleep 500)
  (clerk/show! "dev/inside_out/notebook.cljc"))

(defn publish! [_]
  (io-clerk/setup-viewers!)
  (swap! config/!resource->url merge {"/js/viewer.js" "/js/main.js"})
  (clerk/build-static-app! {:paths ["dev/inside_out/notebook.cljc"]
                            :bundle? false
                            :browse? false
                            :out-path "public"}))

(comment

 (start)

 (clerk/serve! {:browse? true})


 (publish!))