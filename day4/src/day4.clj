(ns day4
  (:require
   [clojure.string :as str]))

(defn- read-pair-of-ranges [s]
  (->> s
       slurp
       str/split-lines
       (map #(str/split % #"[,-]"))
       (map #(map Integer/parseInt %))
       (map #(partition 2 %))))

(defn- ranges-subsume? [[[x1 y1] [x2 y2]]]
  (or (and (>= x1 x2) (<= y1 y2)) (and (<= x1 x2) (>= y1 y2))))

(defn- ranges-overlap? [[[x1 y1] [x2 y2]]]
  (or (<= x2 x1 y2) (<= x2 y1 y2) (<= x1 x2 y1) (<= x1 y2 y1)))

(defn -main [filename]
  (let [pair-of-ranges (read-pair-of-ranges filename)]
    (println (count (filter ranges-subsume? pair-of-ranges)))
    (println (count (filter ranges-overlap? pair-of-ranges)))))
