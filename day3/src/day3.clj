(ns day3
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set]))

(defn read-content-of-rucksacks [filename]
  (with-open [file (io/reader filename)]
    (vec (line-seq file))))

(defn get-priority [[a]]
  (let [i (int a)]
    (if (>= i (int \a))
      (+ (- i (int \a)) 1)
      (+ (- i (int \A)) 27))))

(defn sum-of-priorities [contents]
  (let [split-content (fn [c] (let [mid (/ (count c) 2)]
                                [(take mid c) (take-last mid c)]))
        find-common-elem-in-splits (fn [[a b]]
                                     (seq (clojure.set/intersection
                                           (set a) (set b))))]
    (reduce + (map (comp get-priority find-common-elem-in-splits
                         split-content)
                   contents))))

(defn sum-of-badges [contents]
  (let [find-common-elem-in-groups (fn [[a b c]]
                                     (seq (clojure.set/intersection
                                           (set a) (set b) (set c))))]
    (reduce + (map (comp get-priority find-common-elem-in-groups)
                   (partition 3 contents)))))

(defn -main [filename]
  (let [contents-of-rucksacks (read-content-of-rucksacks filename)]
    (println (sum-of-priorities contents-of-rucksacks))
    (println (sum-of-badges contents-of-rucksacks))))
