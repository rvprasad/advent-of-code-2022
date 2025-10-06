(ns day9
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn- read-directions [filename]
  (->>
   filename
   (slurp)
   (str/split-lines)
   (map #(str/split % #" "))
   (map (fn [[d n]] [d (Integer. n)]))))

(defn- get-next-loc-in-dir [[x y] step]
  (condp = step
    \R [(+ x 1) y]
    \L [(- x 1) y]
    \U [x (+ y 1)]
    \D [x (- y 1)]))

(defn- is-close [[x1 y1] [x2 y2]]
  (and (< (abs (- x1 x2)) 2) (< (abs (- y1 y2)) 2)))

(defn- get-next-loc-to-follow [[sx sy] [tx ty]]
  (cond
    (= sx tx) [sx ((if (< sy ty) + -) sy 1)]
    (= sy ty) [((if (< sx tx) + -) sx 1) sy]
    (and (< sx tx) (< sy ty)) [(+ sx 1) (+ sy 1)]
    (and (< sx tx) (> sy ty)) [(+ sx 1) (- sy 1)]
    (and (> sx tx) (< sy ty)) [(- sx 1) (+ sy 1)]
    (and (> sx tx) (> sy ty)) [(- sx 1) (- sy 1)]))

(defn- accumulate-new-loc [locs-acc curr-knot-loc]
  (let [next-knot-loc (last locs-acc)]
    (conj locs-acc
          (if (is-close curr-knot-loc next-knot-loc)
            curr-knot-loc
            (get-next-loc-to-follow curr-knot-loc next-knot-loc)))))

(defn- move-rope [[tail-locs [head-loc & _ :as locs]] [step]]
  (let [new-head-loc (get-next-loc-in-dir head-loc step)
        new-locs (reduce accumulate-new-loc [new-head-loc] (rest locs))
        new-tail-locs (conj tail-locs (peek new-locs))]
    [new-tail-locs new-locs]))

(defn- count-unique-tail-positions [route len]
  (let [steps (apply concat (map (fn [[d n]] (repeat n d)) route))
        tail-locs (first (reduce move-rope
                                 [[] (vec (repeat len [0 0]))]
                                 steps))]
    (count (distinct tail-locs))))

(defn -main [filename]
  (let [route (read-directions filename)]
    (println (count-unique-tail-positions route 2))
    (println (count-unique-tail-positions route 10))))
