(ns day25
  (:require
   [clojure.math :as math]
   [clojure.pprint :as pp]
   [clojure.string :as str]))

(defn- read-numbers [s]
  (->> s
       slurp
       str/split-lines))

(defn- convert-to-decimal [snafu]
  (->> snafu
       reverse
       (map-indexed vector)
       (reduce (fn [acc [i e]]
                 (->> (case e \2 2 \1 1 \0 0 \- -1 \= -2)
                      (* (math/pow 5 i))
                      (+ acc))) 0)))

(defn- in-range? [decimal pos pos-value]
  (let [pow-5-pos (math/pow 5 pos)
        delta (/ (dec pow-5-pos) 2)
        value (* pos-value pow-5-pos)]
    (<= (- value delta) decimal (+ value delta))))

(defn- get-digit-at [decimal pos]
  (first (filter #(in-range? decimal pos %) [-2 -1 0 1 2])))

(defn- convert-decimal-digit-to-snafu [x]
  (case x -2 "=" -1 "-" 0 "0" 1 "1" 2 "2"))

(defn- convert-to-snafu
  ([decimal] (->> (range 20)
                  (drop-while #(not (in-range? decimal % 2)))
                  first
                  (convert-to-snafu decimal)
                  reverse
                  str/join))
  ([decimal pos] (if (neg? pos)
                   []
                   (let [digit (get-digit-at decimal pos)]
                     (conj (convert-to-snafu
                            (- decimal (* digit (math/pow 5 pos)))
                            (dec pos))
                           (convert-decimal-digit-to-snafu digit))))))

(defn- solve-part-1 [numbers]
  (->> numbers
       (map convert-to-decimal)
       (apply +)
       convert-to-snafu))

(defn -main [filename]
  (pp/pprint (solve-part-1 (read-numbers filename))))
