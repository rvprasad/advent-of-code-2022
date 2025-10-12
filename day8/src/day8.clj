(ns day8
  (:require
   [clojure.string :as str]))

(defn- read-grid [s]
  (->> s
       slurp
       str/split-lines
       (map #(str/split % #""))
       (map (partial map #(Integer. %)))
       (map-indexed (fn [i row] (map-indexed #(vector %1 i %2) row)))))

(defn- get-rotated-grids-and-col-getters [grid]
  (let [flipped (map reverse grid)
        flipped-transposed (apply map vector flipped)
        flipped-transposed-flipped (map reverse flipped-transposed)]
    [[grid first]
     [flipped #(- (dec (count (first grid))) (first %))]
     [flipped-transposed second]
     [flipped-transposed-flipped #(- (dec (count grid)) (second %))]]))

(defn- get-trees-visible-from-left [trees [grid _]]
  (let [f (fn [[c acc :as v] [_ _ h :as e]]
            (if (> h c) [h (conj acc e)] v))]
    (->> grid
         (mapcat #(second (reduce f [-1 []] %)))
         (concat trees))))

(defn- count-visible-trees [grid]
  (count (distinct (reduce get-trees-visible-from-left []
                           (get-rotated-grids-and-col-getters grid)))))

(defn- get-scenic-scores-from-left [tree-2-score [grid get-col]]
  (let [process (fn [[stack acc] e]
                  (if (empty? stack)
                    [(list e) acc]
                    (let [new-stack (drop-while #(> (nth e 2) (last %)) stack)
                          prev_pos (if-some [l (first new-stack)] (get-col l) 0)
                          pos (get-col e)
                          new_value (abs (* (acc e 1) (- pos prev_pos)))]
                      [(conj new-stack e) (assoc acc e new_value)])))]
    (reduce (fn [acc row] (second (reduce process ['() acc] row)))
            tree-2-score
            grid)))

(defn- max-scenic-score [grid]
  (apply max (vals (reduce get-scenic-scores-from-left {}
                           (get-rotated-grids-and-col-getters grid)))))

(defn -main [filename]
  (let [grid (read-grid filename)]
    (println (count-visible-trees grid))
    (println (max-scenic-score grid))))
