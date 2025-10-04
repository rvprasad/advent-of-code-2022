(ns day6
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn read-stream [filename]
  (str/split (slurp filename) #""))

(defn find-first-start-of-packet-marker [stream marker-length]
  (let [enumerated (map-indexed vector stream)
        suffix-length (- marker-length 1)
        get-new-marker (fn [marker elem]
                         (conj (take suffix-length marker) elem))
        get-new-pos (fn [marker idx pos]
                      (if (= (count (set marker)) marker-length)
                        (+ idx 1)
                        pos))
        find-marker (fn [[marker pos :as value] [idx elem]]
                      (if (not= pos -1)
                        value
                        (let [new-marker (get-new-marker marker elem)
                              new-pos (get-new-pos new-marker idx pos)]
                          [new-marker new-pos])))]
    (get (reduce find-marker ['() -1] enumerated) 1)))

(defn -main [filename]
  (let [stream (read-stream filename)]
    (println (find-first-start-of-packet-marker stream 4))
    (println (find-first-start-of-packet-marker stream 14))))
