(ns day22
  (:require
   [clojure.pprint :as pp]
   [clojure.string :as str]))

(defn- read-map-and-path [s]
  (let [tmp (str/split (slurp s) #"\n\n")]
    [(vec (map #(vec (str/split % #""))
               (str/split-lines (first tmp))))
     (vec (map #(if (str/includes? "LR" %) % (parse-long %))
               (re-seq #"L|R|\d+" (second tmp))))]))

(defn- turn [curr-dir dir]
  (case dir
    "R" (case curr-dir ">" "v" "v" "<" "<" "^" "^" ">")
    "L" (case curr-dir ">" "^" "^" "<" "<" "v" "v" ">")))

(defn- move [shift pos get-next-pos]
  (loop [pos pos shift shift]
    (if (zero? shift)
      pos
      (recur (get-next-pos pos) (dec shift)))))

(defn- solve-puzzle [pos [hd & tl] get-next-pos]
  (if (nil? hd)
    pos
    (-> (case hd
          ("R" "L") (update pos :dir #(turn % hd))
          (move hd pos get-next-pos))
        (recur tl get-next-pos))))

(defn- get-line-and-idx [map_ row col dir]
  (case dir
    ("<" ">") [(vec (map_ row)) col]
    ("^" "v") [(vec (map #(% col) map_)) row]))

(defn- get-move-op [dir]
  (if (str/includes? "v>" dir) inc dec))

(defn- update-pos [dir pos idx]
  (case dir
    ("<" ">") (assoc pos :col idx)
    ("v" "^") (assoc pos :row idx)))

(defn- is-block? [v] (= "#" v))

(defn- skip-spaces  [idx idx-op line]
  (if (not= " " (line idx))
    idx
    (recur (idx-op idx) idx-op line)))

(defn- get-next-pos-1  [map_ {:keys [row col dir] :as pos}]
  (let [[line idx] (get-line-and-idx map_ row col dir)
        idx-op #(mod ((get-move-op dir) %) (count line))
        next-idx (skip-spaces (idx-op idx) idx-op line)]
    (->> (if (is-block? (line next-idx)) idx next-idx)
         (update-pos dir pos))))

(defn- solve-part-1 [path map_]
  (solve-puzzle {:row 0 :col (.indexOf (first map_) ".") :dir ">"}
                path
                (partial get-next-pos-1 map_)))

; - Faces: b: back, f: front, r: right, l: left, u: up, d: down
; - Rotations: 0: 0, 1: 90, 2: 180, 3: 270
; - (>, ^, <, v) neighbors and their rotation in the vertical T-shaped unfolding 
;   with :b at the intersection.
(def face-2-neighbor-n-rotation {:b [[:r 0] [:u 0] [:l 0] [:d 0]]
                                 :r [[:f 2] [:u 3] [:b 0] [:d 1]]
                                 :d [[:r 3] [:b 0] [:l 1] [:f 0]]
                                 :l [[:b 0] [:u 1] [:f 2] [:d 3]]
                                 :f [[:r 2] [:d 0] [:l 2] [:u 0]]
                                 :u [[:r 1] [:f 0] [:l 3] [:b 0]]})

(defn- identify-faces-as-laid-on-map [map_ face-width]
  (let [neighbor-deltas [[0 face-width] [(- face-width) 0]
                         [0 (- face-width)] [face-width 0]]
        is-valid? (fn [[r c]]
                    (and  (<= 0 r (dec (count map_)))
                          (<= 0 c (dec (count (first map_))))
                          (not= " " (get (get map_ r []) c nil))))
        acc {:b  [0 (.indexOf (first map_) ".") 0]}]
    (loop [work-list acc acc acc]
      (if (= 6 (count acc))
        acc
        (let [[[face [row col rotation]] & tl] work-list
              neighbors (map (fn [[rd cd]] [(+ row rd) (+ col cd)]) neighbor-deltas)
              new-acc (->> (face-2-neighbor-n-rotation face)
                           (split-at (- 4 rotation))
                           reverse
                           (apply concat)
                           (map (fn [[f o]] [f (mod (+ o rotation) 4)]))
                           (map vector neighbors)
                           (filter #(is-valid? (first %)))
                           (map (fn [[[r c] [f o]]] [f [r c o]]))
                           (remove #(acc (first %))))]
          (recur (into {} (concat tl new-acc))
                 (merge acc (into {} new-acc))))))))

(defn- get-face-maps-in-t-unfolding [map_ face-2-coords-n-rotation face-width]
  (let [rotate-block-90-deg (fn [block _] (map reverse (apply map vector block)))
        get-block (fn [r c]
                    (->> map_
                         (drop r)
                         (take face-width)
                         (map #(drop c %))
                         (map #(take face-width %))))
        get-map-of-face #(let [[r c o] (face-2-coords-n-rotation %)]
                           (mapv vec (reduce rotate-block-90-deg (get-block r c) (range o))))
        faces [:b :f :l :r :u :d]]
    (zipmap faces (map get-map-of-face faces))))

(defn- rotate-position-90-deg [{:keys [row col dir] :as pos} rot max-row-col]
  (let [[new-r new-c] (case rot
                        0 [row col]
                        1 [(- max-row-col col) row]
                        2 [(- max-row-col row) (- max-row-col col)]
                        3 [col (- max-row-col row)])
        dirs [">" "^" "<" "v"]
        new-dir (->> (.indexOf dirs dir)
                     (+ rot)
                     (#(mod % 4))
                     dirs)]
    (assoc pos :row new-r :col new-c :dir new-dir)))

(defn- get-next-pos-2 [face-2-map face-width {:keys [row col dir face] :as pos}]
  (let [[line idx] (get-line-and-idx (face-2-map face) row col dir)
        next-idx ((get-move-op dir) idx)]
    (if (< -1 next-idx face-width)
      (if (is-block? (line next-idx))
        pos
        (update-pos dir pos next-idx))
      (let [[neighbor n-rotation] ((face-2-neighbor-n-rotation face)
                                   (case dir ">" 0 "^" 1 "<" 2 "v" 3))
            new-pos (-> (update-pos dir pos (mod next-idx face-width))
                        (assoc :face neighbor)
                        (rotate-position-90-deg (mod (- 4 n-rotation) 4) (dec face-width)))
            obj-at-new-pos (((face-2-map (:face new-pos)) (:row new-pos))
                            (:col new-pos))]
        (if (is-block? obj-at-new-pos) pos new-pos)))))

(defn- translate-to-original-map [pos face-2-coords-n-rotation max-row-col]
  (let [[row col rot] (face-2-coords-n-rotation (:face pos))
        new-pos (rotate-position-90-deg pos rot max-row-col)]
    (assoc new-pos :row (+ (:row new-pos) row) :col (+ (:col new-pos) col))))

(defn- solve-part-2 [path map_]
  (let [face-width (/ (max (count map_) (count (first map_))) 4)
        face-2-coords-n-rotation (identify-faces-as-laid-on-map map_ face-width)
        face-2-map (get-face-maps-in-t-unfolding map_ face-2-coords-n-rotation face-width)]
    (-> {:row 0 :col 0 :dir ">" :face :b}
        (solve-puzzle path (partial get-next-pos-2 face-2-map face-width))
        (translate-to-original-map face-2-coords-n-rotation (dec face-width)))))

(defn -main [filename]
  (let [[map_ path] (read-map-and-path filename)
        max-col (apply max (map count map_))
        filled-map (->> map_
                        (map #(concat % (repeat (- max-col (count %)) " ")))
                        (mapv vec))
        calculate-password #(+ (* (inc (:row %)) 1000)
                               (* (inc (:col %)) 4)
                               (case (:dir %) ">" 0 "v" 1 "<" 2 "^" 3))]
    (->> [solve-part-1 solve-part-2]
         (map #(% path filled-map))
         (map calculate-password)
         pp/pprint)))
