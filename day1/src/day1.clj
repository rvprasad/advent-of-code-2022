(ns day1
  (:require
   [clojure.string :as str]))

(defn- gather-inventory [coll s]
  (if (= s "")
    (conj coll ())
    (conj (pop coll) (conj (peek coll) (Integer. s)))))

(defn- read-inventory-list [f]
  (->> f
       slurp
       str/split-lines
       (reduce gather-inventory (list ()))
       (filter not-empty)))

(defn -main [filename]
  (let [inventories (read-inventory-list filename)
        total-calories-in-inventories (map (partial reduce +) inventories)]
    (println (apply max total-calories-in-inventories))
    (println (reduce + (take 3 (sort > total-calories-in-inventories))))))
