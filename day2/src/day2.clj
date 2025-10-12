(ns day2
  (:require
   [clojure.string :as str]))

(defn- read-strategy-guide [s]
  (->> s
       slurp
       str/split-lines
       (map #(str/split % #" "))))

(defn- scorer [coll m1 m2]
  (reduce + (map (fn [[_ b :as k]] (+ (m1 k) (m2 b))) coll)))

(defn- score-according-to-strategy [strategy]
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

(defn- score-according-to-requirement [strategy]
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
