(ns inside-out.clerk-build
  (:require [nextjournal.clerk :as clerk]
            [nextjournal.clerk.builder :as builder]
            [shadow.cljs.devtools.api :as shadow]
            [nextjournal.clerk.analyzer :as analyzer]
            [nextjournal.clerk.config :as config]
            [babashka.fs :as fs]))

(defn build! []
  (let [opts {:paths ["dev/inside_out/notebook.cljc"]
              :out-path builder/default-out-path}]
    (shadow/release :clerk)
    (let [release-path "public/js/clerk.js"
          content-addressed (fs/file (str "bundle-" (analyzer/valuehash (slurp release-path)) ".js"))
          out-file (fs/file (:out-path opts) content-addressed)]
      (fs/create-dirs (fs/parent out-file))
      (when-not (fs/exists? out-file)
        (fs/copy release-path out-file))
      (swap! config/!resource->url assoc "/js/viewer.js" content-addressed)
      (clerk/build! opts))))

(comment
 (build!))