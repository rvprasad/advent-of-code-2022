(ns day1
  (:require [clojure.java.io :as io]))

(defn read-inventory-list [filename]
  (defn gather-inventory [inventories line]
    (if (= line "")
      (conj inventories ())
      (conj (pop inventories) (conj (peek inventories) (Integer. line)))))
  (with-open [file (io/reader filename)]
    (->>
     file
     (line-seq)
     (reduce gather-inventory (list ()))
     (filter not-empty))))

(defn get-inventory-with-most-calories [total-calories-in-inventories]
  (apply max total-calories-in-inventories))

(defn get-sum-of-calories-of-top-three-inventories [total-calories-in-inventories]
  (reduce + (take 3 (sort > total-calories-in-inventories))))

(defn -main [filename]
  (let [inventories (read-inventory-list filename)
        total-calories-in-inventories (map (partial reduce +) inventories)]
    (println (get-inventory-with-most-calories total-calories-in-inventories))
    (println (get-sum-of-calories-of-top-three-inventories
              total-calories-in-inventories))))
