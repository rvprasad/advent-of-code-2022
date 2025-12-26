(ns day24
  (:require
   [clojure.pprint :as pp]
   [clojure.string :as str]))

(defn- read-map [s]
  (let [helper (fn [match strings]
                 (map (fn [xs] (mapv #(if (= % match) \h \.) xs)) strings))
        lines (str/split-lines (slurp s))
        trimmed-rows (->> lines
                          (drop 1)
                          (drop-last 1)
                          (map #(subs % 1 (dec (count (first lines)))))
                          (map vec))
        trimmed-cols (apply map vector trimmed-rows)]
    {:num-row (count trimmed-rows)
     :num-col (count trimmed-cols)
     :hurricanes {:right (helper \> trimmed-rows)
                  :left (helper \< trimmed-rows)
                  :up (helper \^ trimmed-cols)
                  :down (helper \v trimmed-cols)}}))

(defn- rotate [xs dir]
  (case dir
    (:left :up) (vec (concat (rest xs) [(first xs)]))
    (:right :down) (vec (concat [(last xs)] (butlast xs)))))

(defn- move-hurricanes [dir-2-hurricanes]
  (->> dir-2-hurricanes
       (map (fn [[d h]] [d (mapv #(rotate % d) h)]))
       (into {})))

(defn- get-neighbors [[r c]]
  (map (fn [[rd cd]] [(+ r rd) (+ c cd)]) [[-1 0] [1 0] [0 0] [0 -1] [0 1]]))

(defn- in-hurricane? [[r c] {:keys [hurricanes]}]
  (some #(= \h (get-in hurricanes %))
        [[:right r c] [:left r c] [:up c r] [:down c r]]))

(defn- is-valid? [[r c :as pos] start end map_]
  (or (= pos start) (= pos end) (and (< -1 r (:num-row map_))
                                     (< -1 c (:num-col map_)))))

(defn- solve-puzzle
  ([map_ start end] (solve-puzzle map_ [start] start end 0))
  ([map_ positions start end n]
   (if (some #(= % end) positions)
     [n map_]
     (let [new-map (update map_ :hurricanes move-hurricanes)
           new-positions (->> positions
                              (map get-neighbors)
                              (apply concat)
                              set
                              (filter #(is-valid? % start end map_))
                              (remove #(in-hurricane? % new-map)))]
       (recur new-map new-positions start end (inc n))))))

(defn- solve-part-2 [map_ start end]
  (let [[n1 map1] (solve-puzzle map_ start end)
        [n2 map2] (solve-puzzle map1 end start)]
    (+ n1 n2 (first (solve-puzzle map2 start end)))))

(defn -main [filename]
  (let [map_ (read-map filename)
        start [-1 0]
        end [(:num-row map_) (dec (:num-col map_))]]
    (pp/pprint (first (solve-puzzle map_ start end)))
    (pp/pprint (solve-part-2 map_ start end))))
