(ns day3
  (:require [clojure.string :as str]
            [clojure.set]))

(def a-int (int \a))

(defn- get-priority [[c]]
  (let [i (int c)]
    (- i (if (>= i a-int) (dec a-int) (- (int \A) 27)))))

(defn- get-common-elem [coll]
  (seq (apply clojure.set/intersection (map set coll))))

(defn- sum-of-priorities [coll]
  (let [f (fn [coll] (let [mid (/ (count coll) 2)]
                       [(take mid coll) (take-last mid coll)]))]
    (reduce + (map (comp get-priority get-common-elem f) coll))))

(defn- sum-of-badges [coll]
  (reduce + (map (comp get-priority get-common-elem) (partition 3 coll))))

(defn -main [filename]
  (let [contents-of-rucksacks (str/split-lines (slurp filename))]
    (println (sum-of-priorities contents-of-rucksacks))
    (println (sum-of-badges contents-of-rucksacks))))
