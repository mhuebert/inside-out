^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns inside-out.test-notebook
  (:require [nextjournal.clerk :as-alias clerk]
            [clojure.string :as str]
            [inside-out.clerk-cljs :refer [cljs]]))

(cljs (str/join "," [1, 2, 3, 4]))
(cljs ^:vector  [1 2 3 4])
(cljs (map inc (range 100)))