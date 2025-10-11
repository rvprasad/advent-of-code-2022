(ns day12
  (:require
   [clojure.set]
   [clojure.string :as str]))

(defn- read-height-map [filename]
  (let [hmap (->>
              filename
              (slurp)
              (str/split-lines)
              (map seq)
              (map-indexed (fn [r elems]
                             (map-indexed (fn [c e] [[c r] e]) elems)))
              (apply concat)
              (into {}))
        helper (fn [sym] #(= (second %) sym))
        start-loc (first (first (filter (helper \S) hmap)))
        end-loc (first (first (filter (helper \E) hmap)))
        locs (keys hmap)]
    [(assoc hmap start-loc \a end-loc \z)
     start-loc
     end-loc
     (apply max (map second locs))
     (apply max (map first locs))]))

(defn- find-min-path-length-from-all-nodes [hmap end max-r max-c]
  (let [max-path-len (* (inc max-r) (inc max-c))
        get-neighbors (fn [[c r :as node]]
                        (->>
                         [[(inc c) r] [(dec c) r] [c (inc r)] [c (dec r)]]
                         (filter (fn [[c r]] (and (>= c 0) (<= c max-c)
                                                  (>= r 0) (<= r max-r))))
                         (filter #(>= (int (hmap %)) (dec (int (hmap node)))))
                         (set)))
        get-new-dist (fn [node-2-dist [n neighbors]]
                       (map #(vector % (inc (node-2-dist n))) neighbors))]
    (loop [nodes #{end}
           node-2-dist {end 0}]
      (let [visited-nodes (set (keys node-2-dist))
            node-2-neighbors (zipmap nodes (map #(clojure.set/difference
                                                  (get-neighbors %)
                                                  visited-nodes)
                                                nodes))
            next-nodes (apply concat (vals node-2-neighbors))
            new-dist (map (partial get-new-dist node-2-dist) node-2-neighbors)
            new-node-2-dist (merge node-2-dist 
                                   (into {} (apply concat new-dist)))]
        (if (empty? next-nodes)
          new-node-2-dist
          (recur next-nodes new-node-2-dist))))))

(defn- find-min-path-length-from-any-a [hmap end max-r max-c]
  (let [node-2-dist (find-min-path-length-from-all-nodes hmap end max-r max-c)]
    (->>
     hmap
     (keys)
     (filter #(= (hmap %) \a))
     (map node-2-dist)
     (filter some?)
     (apply min))))

(defn -main [filename]
  (let [[hmap start end max-r max-c] (read-height-map filename)]
    (println ((find-min-path-length-from-all-nodes hmap end max-r max-c) start))
    (println (find-min-path-length-from-any-a hmap end max-r max-c))))
