(ns day4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-pair-of-ranges [filename]
  (with-open [file (io/reader filename)]
    (vec (->>
          file
          (line-seq)
          (mapv #(str/split % #"[,-]"))
          (mapv #(mapv Integer/parseInt %))
          (mapv #(partition 2 %))))))

(defn do-ranges-subsume [[[a b] [c d]]] (or (and (>= a c) (<= b d))
                                            (and (<= a c) (>= b d))))

(defn do-ranges-overlap [[[a b] [c d]]] (or (and (>= a c) (<= a d))
                                            (and (>= b c) (<= b d))
                                            (and (>= c a) (<= c b))
                                            (and (>= d a) (<= d b))))

(defn -main [filename]
  (let [pair-of-ranges (read-pair-of-ranges filename)]
    (println (count (filter do-ranges-subsume pair-of-ranges)))
    (println (count (filter do-ranges-overlap pair-of-ranges)))))
