(ns dev
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.config :as config]
            [nextjournal.clerk.viewer :as clerk.viewer]
            [shadow.cljs.devtools.api :as shadow]))

(defn start
  {:shadow/requires-server true}
  []
  (shadow/watch :clerk)
  (swap! config/!resource->url (fn [x]
                                 (-> x
                                     (assoc "/js/viewer.js" "http://localhost:8765/js/main.js")
                                     (dissoc "/css/viewer.css"))))
  (clerk/serve! {:resource->url {"/js/viewer.js" "http://localhost:8765/js/main.js"
                                 "/css/viewer.css" nil}
                 :port 7777
                 :browse? true
                 :out-path "public"
                 :watch-paths ["dev"]
                 :show-filter-fn #(str/includes? % "notebooks")})
  (clerk/show! "dev/inside_out/notebook.cljc"))

(defn publish! [& [opts]]
  (clerk/build! (merge {:resource->url {"/js/viewer.js" "/js/main.js"}
                        :index "dev/inside_out/notebook.cljc"
                        :compile-css false
                        :bundle false
                        :out-path "public/build"}
                       opts)))

(comment
 (shadow.cljs.devtools.api/watch :clerk)
 (start)
 (publish!)
 (clerk/clear-cache!)
 )