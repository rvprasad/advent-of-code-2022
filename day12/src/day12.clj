(ns day12
  (:require
   [clojure.set]
   [clojure.string :as str]))

(defn- read-height-map [s]
  (let [h-map (->> s
                   slurp
                   str/split-lines
                   (map seq)
                   (map-indexed (fn [r elems]
                                  (map-indexed (fn [c e] [[c r] e]) elems)))
                   (apply concat)
                   (into {}))
        helper (fn [sym] #(= (second %) sym))
        start-loc (first (first (filter (helper \S) h-map)))
        end-loc (first (first (filter (helper \E) h-map)))
        locs (keys h-map)]
    [(assoc h-map start-loc \a end-loc \z)
     start-loc
     end-loc
     (apply max (map second locs))
     (apply max (map first locs))]))

(defn- find-min-path-length-from-all-nodes [h-map end max-r max-c]
  (let [get-neighbors (fn [[c r :as n]]
                        (->> [[(inc c) r] [(dec c) r] [c (inc r)] [c (dec r)]]
                             (filter #(and (<= 0 (first %) max-c)
                                           (<= 0 (second %) max-r)))
                             (filter #(>= (int (h-map %)) (dec (int (h-map n)))))
                             set))
        get-new-dist (fn [node-2-dist [n neighbors]]
                       (map #(vector % (inc (node-2-dist n))) neighbors))]
    (loop [nodes #{end}
           node-2-dist {end 0}]
      (let [visited (set (keys node-2-dist))
            node-2-neighbors (->> nodes
                                  (map get-neighbors)
                                  (map #(clojure.set/difference % visited))
                                  (zipmap nodes))
            next-nodes (apply concat (vals node-2-neighbors))
            new-dist (mapcat #(get-new-dist node-2-dist %) node-2-neighbors)
            new-node-2-dist (merge node-2-dist (into {} new-dist))]
        (if (empty? next-nodes)
          new-node-2-dist
          (recur next-nodes new-node-2-dist))))))

(defn- find-min-path-length-from-any-a [h-map end max-r max-c]
  (let [node-2-dist (find-min-path-length-from-all-nodes h-map end max-r max-c)]
    (->> h-map
         keys
         (filter #(= (h-map %) \a))
         (map node-2-dist)
         (filter some?)
         (apply min))))

(defn -main [filename]
  (let [[h-map start end max-r max-c] (read-height-map filename)]
    (println ((find-min-path-length-from-all-nodes h-map end max-r max-c) start))
    (println (find-min-path-length-from-any-a h-map end max-r max-c))))
