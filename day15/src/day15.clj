(ns day15
  (:require
   [clojure.string :as str]
   [clojure.core.reducers :as reducers]))

(defn- read-sensor-2-beacon [s]
  (->> s
       slurp
       str/split-lines
       (map #(re-matches #"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)" %))
       (reduce (fn [s2b [_ sx sy bx by]]
                 (assoc s2b
                        [(Integer/parseInt sx) (Integer/parseInt sy)]
                        [(Integer/parseInt bx) (Integer/parseInt by)]))
               {})))

(defn- get-radius [[[sx sy :as s] [bx by]]]
  [s (+ (abs (- sx bx)) (abs (- sy by)))])

(defn- get-run-at-y [y [[sx sy :as s] r]]
  (let [x (- r (abs (- sy y)))]
    [(- sx x) (+ sx x)]))

(defn- merge-runs [[hd & tl :as acc] [x1 x2 :as r]]
  (if (nil? hd)
    [r]
    (let [[x3 x4] hd]
      (cond
        (<= x3 x1 x2 x4) acc
        (<= x3 x1 x4 x2) (conj tl [x3 x2])
        (<= x1 x3 x4 x2) (conj tl [x1 x2])
        (<= x1 x3 x2 x4) (conj tl [x1 x4])
        :else (conj acc r)))))

(defn- get-runs-of-unoccupied-cells [s-2-b y]
  (->> s-2-b
       (map get-radius)
       (filter (fn [[[_ sy] r]] (<= (- sy r) y (+ sy r))))
       (map (partial get-run-at-y y))
       sort
       (reduce merge-runs '())))

(defn- solve-part1 [s-2-b y]
  (->> (get-runs-of-unoccupied-cells s-2-b y)
       (map (fn [[x1 x2]] (- x2 x1)))
       (reduce +)))

(defn- solve-part2 [s-2-b]
  (let [f (fn [y]
            (->> (get-runs-of-unoccupied-cells s-2-b y)
                 (filter (fn [[x1 x2]] (and (<= x1 4000000) (>= x2 0))))
                 (map (fn [[x1 x2]] [(max x1 0) (min x2 4000000)]))
                 count
                 (= 2)))
        y (first (reducers/foldcat (reducers/filter f (vec (range 4000001)))))
        x (->> y
               (get-runs-of-unoccupied-cells s-2-b)
               sort
               first
               second
               inc)]
    (+ y (* 4000000 x))))

(defn -main [filename y]
  (let [sensor-2-beacon (read-sensor-2-beacon filename)]
    (println (solve-part1 sensor-2-beacon (Integer/parseInt y)))
    (println (solve-part2 sensor-2-beacon))))

