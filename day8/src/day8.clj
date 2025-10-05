(ns day8
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn- read-grid [filename]
  (->>
   filename
   (slurp)
   (#(str/split % #"\n"))
   (map #(str/split % #""))
   (map (partial map #(Integer. %)))
   (map-indexed (fn [i row] (map-indexed #(vector %1 i %2) row)))))

(defn- print-grid [grid]
  (println)
  (dorun (map println grid))
  (println))

(defn- get-rotated-grids-and-col-getters [grid]
  (let [num-row (- (count grid) 1)
        num-col (- (count (first grid)) 1)
        grid-flipped (map reverse grid)
        grid-flipped-transposed (apply map vector grid-flipped)
        grid-flipped-transposed-flipped (map reverse grid-flipped-transposed)]
    [[grid first] [grid-flipped #(- num-col (first %))]
     [grid-flipped-transposed second]
     [grid-flipped-transposed-flipped #(- num-row (second %))]]))

(defn- get-trees-visible-from-left [visible-trees [grid _]]
  (let [f (fn [[curr acc :as v] [_ _ h :as e]]
            (if (> h curr) [h (conj acc e)] v))]
    (concat visible-trees
            (apply concat (map #(second (reduce f [-1 []] %)) grid)))))

(defn- count-visible-trees [grid]
  (let [grids (get-rotated-grids-and-col-getters grid)]
    (count (distinct (reduce get-trees-visible-from-left [] grids)))))

(defn- get-scenic-scores-from-left [tree-2-score [grid get-pos]]
  (let [process (fn [[decr-stack acc :as d] e]
                  (if (empty? decr-stack)
                    [(list e) acc]
                    (let [new-decr-stack (drop-while #(> (nth e 2) (last %))
                                                     decr-stack)
                          prev_pos (if-some [tmp1 (first new-decr-stack)]
                                     (get-pos tmp1)
                                     0)
                          pos (get-pos e)
                          new_value (abs (* (get acc e 1) (- pos prev_pos)))]
                      [(conj new-decr-stack e) (assoc acc e new_value)])))
        scan (fn [acc row] (second (reduce process ['() acc] row)))]
    (reduce scan tree-2-score grid)))

(defn- max-scenic-score [grid]
  (let [grids (get-rotated-grids-and-col-getters grid)]
    (apply max (vals (reduce get-scenic-scores-from-left {} grids)))))

(defn -main [filename]
  (let [grid (read-grid filename)]
    (println (count-visible-trees grid))
    (println (max-scenic-score grid))))
