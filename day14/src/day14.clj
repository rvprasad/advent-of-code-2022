(ns day14
  (:require
   [clojure.set]
   [clojure.string :as str]))

(defn- read-rocks [s]
  (->> s
       slurp
       str/split-lines
       (map #(str/split % #" -> "))
       (map (fn [xs] (map #(map Integer/parseInt (str/split % #",")) xs)))
       (map #(map vector (drop-last %) (rest %)))
       (apply concat)
       (map (fn [[[x1 y1] [x2 y2]]]
              (if (= x1 x2)
                (map #(vector x1 %) (range (min y1 y2) (inc (max y1 y2))))
                (map #(vector % y1) (range (min x1 x2) (inc (max x1 x2)))))))
       (apply concat)
       (into #{})))

(defn- get-new-pos [obs [x y]]
  (let [ny (inc y)]
    (cond
      (not (contains? obs [x ny])) [x ny]
      (not (contains? obs [(dec x) ny])) [(dec x) ny]
      (not (contains? obs [(inc x) ny])) [(inc x) ny]
      :else nil)))

(defn- solve-part1 [rocks start-pos highest-y]
  (loop [obstacles rocks
         [x y :as pos] start-pos]
    (if (> y highest-y)
      (count (clojure.set/difference obstacles rocks))
      (if-some [new-pos (get-new-pos obstacles pos)]
        (recur obstacles new-pos)
        (recur (conj obstacles pos) start-pos)))))

(defn- solve-part2 [rocks start-pos highest-y]
  (loop [obstacles rocks
         [x y :as pos] start-pos]
    (if (contains? obstacles pos)
      (count (clojure.set/difference obstacles rocks))
      (if-some [new-pos (get-new-pos obstacles pos)]
        (if (= (second new-pos) (+ 2 highest-y))
          (recur (conj obstacles pos) start-pos)
          (recur obstacles new-pos))
        (recur (conj obstacles pos) start-pos)))))

(defn -main [filename]
  (let [rocks (read-rocks filename)
        highest-y (apply max (map second rocks))
        start-pos [500 0]]
    (println (solve-part1 rocks start-pos highest-y))
    (println (solve-part2 rocks start-pos highest-y))))

