(ns day20
  (:require
   [clojure.pprint :as pp]
   [clojure.string :as str]))

(defn- read-coordinates [s]
  (->> s
       slurp
       str/split-lines
       (map parse-long)))

(defn- mix-coords [coords idx-coord]
  (let [from-idx (.indexOf coords idx-coord)
        to-idx (mod (+ from-idx (second idx-coord)) (dec (count coords)))]
    (->> (cond
      (= from-idx to-idx) coords
      ; [p1 | [from-elem] | p2 | [to-elem] | p3] 
      (< from-idx to-idx) (let [p1 (subvec coords 0 from-idx)
                                [from-elem & p2] (subvec coords from-idx to-idx)
                                [to-elem & p3] (subvec coords to-idx)]
                            (vec (concat p1 p2 [to-elem from-elem] p3)))
      ; [p1 | [to-elem] | p2 | [from-elem] | p3] 
      (> from-idx to-idx) (let [p1 (subvec coords 0 to-idx)
                                [to-elem & p2] (subvec coords to-idx from-idx)
                                [from-elem & p3] (subvec coords from-idx)]
                            (vec (concat p1 [from-elem to-elem] p2 p3)))))))

(defn- solve [coords decrypt-key reps]
  (let [indexed-coords (->> coords
                            (map #(* decrypt-key %))
                            (map-indexed vector)
                            vec)]
    (->> indexed-coords 
         (repeat reps)
         (apply concat)
         (reduce mix-coords indexed-coords)
         (map second)
         cycle
         (drop-while #(not (zero? %)))
         (take-nth 1000)
         (take 4)
         (apply +))))

(defn -main [filename]
  (let [coordinates (read-coordinates filename)]
    (pp/pprint (solve coordinates 1 1))  
    (pp/pprint (solve coordinates 811589153 10))))
