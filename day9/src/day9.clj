(ns day9
  (:require
   [clojure.string :as str]))

(defn- read-directions [s]
  (->> s
       slurp
       str/split-lines
       (map #(str/split % #" "))
       (map (fn [[d n]] [d (Integer. n)]))))

(defn- get-next-loc-in-dir [[x y] step]
  (condp = step
    \R [(inc x) y]
    \L [(dec x) y]
    \U [x (inc y)]
    \D [x (dec y)]))

(defn- is-close? [[x1 y1] [x2 y2]]
  (and (< (abs (- x1 x2)) 2) (< (abs (- y1 y2)) 2)))

(defn- get-next-loc-to-follow [[sx sy] [tx ty]]
  (cond
    (= sx tx) [sx ((if (< sy ty) inc dec) sy)]
    (= sy ty) [((if (< sx tx) inc dec) sx) sy]
    (and (< sx tx) (< sy ty)) [(inc sx) (inc sy)]
    (and (< sx tx) (> sy ty)) [(inc sx) (dec sy)]
    (and (> sx tx) (< sy ty)) [(dec sx) (inc sy)]
    (and (> sx tx) (> sy ty)) [(dec sx) (dec sy)]))

(defn- gather-new-loc [locs-acc curr-knot-loc]
  (let [next-knot-loc (last locs-acc)]
    (conj locs-acc
          (if (is-close? curr-knot-loc next-knot-loc)
            curr-knot-loc
            (get-next-loc-to-follow curr-knot-loc next-knot-loc)))))

(defn- move-rope [[tail-locs [head-loc & _ :as locs]] [step]]
  (let [new-head-loc (get-next-loc-in-dir head-loc step)
        new-locs (reduce gather-new-loc [new-head-loc] (rest locs))
        new-tail-locs (conj tail-locs (peek new-locs))]
    [new-tail-locs new-locs]))

(defn- count-unique-tail-positions [route len]
  (let [steps (mapcat (fn [[d n]] (repeat n d)) route)
        tail-locs (first (reduce move-rope
                                 [[] (vec (repeat len [0 0]))]
                                 steps))]
    (count (distinct tail-locs))))

(defn -main [filename]
  (let [route (read-directions filename)]
    (println (count-unique-tail-positions route 2))
    (println (count-unique-tail-positions route 10))))
