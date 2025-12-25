(ns day23
  (:require
   [clojure.pprint :as pp]
   [clojure.string :as str]))

(defn- read-positions [s]
  (->> s
       slurp
       str/split-lines
       (map-indexed (fn [ri row] (map-indexed (fn [ci e] [ri ci e]) row)))
       (apply concat)
       (filter (fn [[_ _ e]] (= \# e)))
       (map (fn [[r c _]] [r c]))
       (into #{})))

(defn- no-neighbors? [[r c] positions]
  (not-any? #(positions %) (for [rd [-1 0 1]
                                 cd [-1 0 1]
                                 :when (not= 0 rd cd)]
                             [(+ r rd) (+ c cd)])))

(defn- get-adjacents [r c dir]
  (let [vary #(map (fn [i] (+ i %)) [-1 0 1])]
    (case dir
      \n (map (fn [nc] [(dec r) nc]) (vary c))
      \s (map (fn [nc] [(inc r) nc]) (vary c))
      \w (map (fn [nr] [nr (dec c)]) (vary r))
      \e (map (fn [nr] [nr (inc c)]) (vary r)))))

(defn- propose-position [[r c :as curr-pos] positions dirs]
  (if (no-neighbors? curr-pos positions)
    curr-pos
    (->> dirs
         (take 4)
         (map #(get-adjacents r c %))
         (remove (fn [adjacents] (some #(contains? positions %) adjacents)))
         first
         second
         (#(if % % curr-pos)))))

(defn- move-elves [[positions dirs]]
  (let [new-positions (map #(propose-position % positions dirs) positions)
        freq (frequencies new-positions)]
    [(->> new-positions
          (map vector positions)
          (map (fn [[k v]] (if (not= 1 (get freq v)) k v)))
          set)
     (rest dirs)]))

(defn- solve-part-1 [positions dirs]
  (let [[rows cols] (as-> (iterate move-elves [positions dirs]) v
                      (nth v 10)
                      (first v)
                      (apply map vector v))]
    (- (* (- (apply max rows) (apply min rows) -1)
          (- (apply max cols) (apply min cols) -1))
       (count positions))))

(defn- solve-part-2 [positions dirs n]
  (let [[new-positions new-dirs] (move-elves [positions dirs])]
    (if (= new-positions positions)
      (inc n)
      (recur new-positions new-dirs (inc n)))))

(defn -main [filename]
  (let [positions (read-positions filename)
        dirs (cycle [\n \s \w \e])]
    (pp/pprint (solve-part-1 positions dirs))
    (pp/pprint (solve-part-2 positions dirs 0))))
