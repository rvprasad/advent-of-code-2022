(ns day18
  (:require
   [clojure.pprint :as pp]
   [clojure.set :as set]
   [clojure.string :as str]))

(defn- read-scan [s]
  (->> s
       slurp
       str/split-lines
       (map #(map Integer/parseInt (str/split % #",")))
       (map (fn [[x y z]] {:x x :y y :z z}))
       set))

(defn- get-neighbors [cube]
  (map (fn [[xd yd zd]] (-> cube
                            (update :x #(+ xd %))
                            (update :y #(+ yd %))
                            (update :z #(+ zd %))))
       [[0 0 -1] [0 0 1] [0 -1 0] [0 1 0] [-1 0 0] [1 0 0]]))

(defn- solve-part-1 [scan]
  (->> scan
       (map get-neighbors)
       (apply concat)
       (remove scan)
       count))

(defn- get-mins-and-maxs [scan]
  (->> [:x :y :z]
       (map #(map % scan))
       (map #(vector (apply min %) (apply max %)))))

(defn- identify-cliques
  ([cubes]
   (let [fst (first cubes)]
     (identify-cliques cubes #{fst} [] #{fst} [fst])))
  ([cubes seen cliques curr-cliq curr-cubes]
   (let [neighbors (->> curr-cubes
                        (map get-neighbors)
                        (apply concat)
                        (filter cubes)
                        (remove seen)
                        set)]
     (if-not (empty? neighbors)
       (identify-cliques cubes
                         (set/union seen neighbors)
                         cliques
                         (set/union curr-cliq neighbors)
                         neighbors)
       (if-some [fst (first (remove seen cubes))]
         (identify-cliques cubes
                           (conj seen fst)
                           (conj cliques curr-cliq)
                           #{fst}
                           [fst])
         (conj cliques curr-cliq))))))

(defn- solve-part-2 [scan]
  (let [[[nx mx] [ny my] [nz mz]] (get-mins-and-maxs scan)
        cubes (->> (for [x (range nx (inc mx))
                         y (range ny (inc my))
                         z (range nz (inc mz))]
                     {:x x :y y :z z})
                   (remove scan)
                   set)
        not-exposed? #(and (< nx (:x %) mx) (< ny (:y %) my) (< nz (:z %) mz))]
    (- (solve-part-1 scan)
       (->> cubes
            identify-cliques
            (filter #(every? not-exposed? %))
            (map solve-part-1)
            (apply +)))))

(defn -main [filename]
  (let [scan (read-scan filename)]
    (pp/pprint (solve-part-1 scan))
    (pp/pprint (solve-part-2 scan))))
