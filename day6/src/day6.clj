(ns day6
  (:require
   [clojure.string :as str]))

(defn- find-first-start-of-packet-marker [stream marker-length]
  (let [suffix-length (dec marker-length)
        find-marker (fn [[coll _ :as value] [i e]]
                      (if (= (count (set coll)) marker-length)
                        value
                        [(conj (take suffix-length coll) e) (inc i)]))]
    (last (reduce find-marker ['() -1] (map-indexed vector stream)))))

(defn -main [filename]
  (let [stream (str/split (slurp filename) #"")]
    (println (find-first-start-of-packet-marker stream 4))
    (println (find-first-start-of-packet-marker stream 14))))
