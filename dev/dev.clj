(ns dev
  (:require [clojure.string :as str]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.config :as config]
            [nextjournal.clerk.viewer]))

(defn start []
  (swap! config/!resource->url (fn [x]
                                 (-> x
                                     (assoc "/js/viewer.js" "http://localhost:8765/js/main.js")
                                     (dissoc "/css/viewer.css"))))
  (clerk/serve! {:browse? true
                 :out-path "public"
                 :watch-paths ["dev"]
                 :show-filter-fn #(str/includes? % "notebooks")})
  (clerk/show! "dev/inside_out/notebook.cljc"))

(defn publish! [& [opts]]
  (swap! config/!resource->url merge {"/js/viewer.js" "/js/main.js"})
  (clerk/build! (merge {:index "dev/inside_out/notebook.cljc"
                        :compile-css false
                        :bundle false
                        :out-path "public/build"} opts)))

(comment
 (shadow.cljs.devtools.api/watch :clerk)
 (start)
 (publish!)
 (clerk/clear-cache!)
 )