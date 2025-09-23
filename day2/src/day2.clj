(ns day2
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn read-strategy-guide [filename]
  (with-open [file (io/reader filename)]
    (doall (->>
            file
            (line-seq)
            (map #(str/split % #" "))))))

(defn scorer [input game-score obj-score]
  (let [get-score (fn [[a b :as k]] (+ (get game-score k) (get obj-score b)))]
    (reduce + (map get-score input))))

(defn score-according-to-strategy [strategy]
  (let [game-score {["A" "X"] 3
                    ["A" "Y"] 6
                    ["A" "Z"] 0
                    ["B" "X"] 0
                    ["B" "Y"] 3
                    ["B" "Z"] 6
                    ["C" "X"] 6
                    ["C" "Y"] 0
                    ["C" "Z"] 3}
        obj-score {"X" 1, "Y" 2, "Z" 3}]
    (scorer strategy game-score obj-score)))

(defn score-according-to-requirement [strategy]
  (let [game-score {"X" 0 "Y" 3 "Z" 6}
        obj-score {["A" "X"] 3
                   ["A" "Y"] 1
                   ["A" "Z"] 2
                   ["B" "X"] 1
                   ["B" "Y"] 2
                   ["B" "Z"] 3
                   ["C" "X"] 2
                   ["C" "Y"] 3
                   ["C" "Z"] 1}]
    (scorer strategy obj-score game-score)))

(defn -main [filename]
  (let [strategy (read-strategy-guide filename)]
    (println (score-according-to-strategy strategy))
    (println (score-according-to-requirement strategy))))
