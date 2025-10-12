(ns day13
  (:require
   [clojure.string :as str]))

(defn- read-packet-list [s]
  (->>
   (str/split (slurp s) #"\n\n")
   (map str/split-lines)
   (map (partial map read-string))))

(defn- get-order [l r]
  (cond
    (and (int? l) (int? r)) (- l r)
    (and (int? l) (vector? r)) (get-order [l] r)
    (and (vector? l) (int? r)) (get-order l [r])
    (and (empty? l) (empty? r)) 0
    (and (empty? l) (seq r)) -1
    (and (seq l) (empty? r)) 1
    (and (vector? l) (vector? r)) (let [[lh & lt] l
                                        [rh & rt] r]
                                    (if-let [result (get-order lh rh)]
                                      result
                                      (get-order (vec lt) (vec rt))))))

(defn- solve-part1 [xs]
  (->>
   xs
   (map #(get-order (first %) (second %)))
   (map-indexed #(if (< %2 0) (inc %1) 0))
   (reduce +)))

(defn- solve-part2 [xs]
  (->>
   xs
   (apply concat)
   (concat [[[2]] [[6]]])
   (sort get-order)
   (keep-indexed #(if (#{[[2]] [[6]]} %2) (inc %1) 1))
   (reduce *)))

(defn -main [filename]
  (let [packet-list (read-packet-list filename)]
    (println (solve-part1 packet-list))
    (println (solve-part2 packet-list))))
