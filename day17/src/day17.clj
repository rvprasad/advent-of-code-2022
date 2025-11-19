(ns day17
  (:require
   [clojure.pprint :as pp]
   [clojure.string :as str]))

(defn- read-move-pattern [s]
  (-> s
      slurp
      str/trim
      (str/split #"")))

(defn- get-rocks
  ([n]
   (get-rocks [:bar :plus :rev-l :pipe :block] 0 n))
  ([rocks start end]
   (let [rock {:x 2 :y 4}]
     (->> rocks
          (map #(assoc rock :rock-type %))
          cycle
          (map #(assoc %2 :id %1) (range start end))))))

(defn- convert-rock-to-locs [{:keys [rock-type x y]}]
  (let [x+1 (inc x)
        y+1 (inc y)
        x+2 (inc x+1)
        y+2 (inc y+1)]
    (case rock-type
      :bar (mapv #(vector % y) (range x (+ x 4)))
      :plus [[x+1 y] [x y+1] [x+1 y+1] [x+2 y+1] [x+1 y+2]]
      :rev-l [[x y] [x+1 y] [x+2 y] [x+2 y+1] [x+2 y+2]]
      :pipe (mapv #(vector x %) (range y (+ y 4)))
      :block [[x y] [x+1 y] [x y+1] [x+1 y+1]])))

(defn- unblocked? [rock y-2-xs]
  (let [x (:x rock)
        width (case (:rock-type rock)
                :bar 3
                (:plus :rev-l) 2
                :pipe 0
                :block 1)
        overlap? (fn [[x y]] (contains? (y-2-xs y #{}) x))]
    (and (<= 0 x (+ x width) 6)
         (not-any? overlap? (convert-rock-to-locs rock)))))

(defn- move-rock [rock move y-2-xs]
  (let [moved-rock (case move
                     ">" (update rock :x inc)
                     "<" (update rock :x dec)
                     "v" (update rock :y dec))]
    (if (unblocked? moved-rock y-2-xs) moved-rock rock)))

(defn- stack-height [stack] (apply max (keys stack)))

(defn- add-rock-to-stack [rock stack]
  (reduce (fn [acc [x y]] (update acc y (fnil #(conj % x) #{})))
          stack
          (convert-rock-to-locs rock)))

(defn- move-rock-onto-stack [rock stack moves]
  (loop [rock (update rock :y #(+ % (stack-height stack)))
         [move & moves'] moves]
    (let [rock' (move-rock rock move stack)]
      (if (not= rock' rock)
        (recur rock' moves')
        (if (= "v" move)
          [(add-rock-to-stack rock stack) moves']
          (recur rock moves'))))))

(defn- get-pattern [stack n]
  (let [h1 (inc (stack-height stack))
        h2 (max 0 (- h1 n))]
    (mapv #(sort (stack %)) (range h2 h1))))

(defn- jump-cycles [rocks stack seen pattern-str num-rocks]
  (when-let [[seen-rock-id seen-height] (seen pattern-str)]
    (let [r-id (:id (first rocks))
          num-rocks-in-cycle (- r-id seen-rock-id 1)
          reps (quot (- num-rocks r-id) num-rocks-in-cycle)
          rock-jump (+ r-id (* reps num-rocks-in-cycle))
          rocks' (get-rocks (map :rock-type (take 5 rocks)) rock-jump num-rocks)
          stack-jump (* reps (- (stack-height stack) seen-height))
          stack' (update-keys stack #(+ % stack-jump))]
      [rocks' stack'])))

(defn- move-rocks
  ([stack moves rocks num-rocks seen]
   (if (seq rocks)
     (let [[rock & rocks'] rocks
           [stack' moves'] (move-rock-onto-stack rock stack moves)
           pattern-str (str (get-pattern stack' 30))]
       (if-let [[rocks'' stack'']
                (jump-cycles rocks' stack' seen pattern-str num-rocks)]
         (first (reduce #(apply move-rock-onto-stack %2 %1)
                        [stack'' moves']
                        rocks''))
         (recur stack'
                moves'
                rocks'
                num-rocks
                (assoc seen pattern-str [(:id rock) 
                                         (stack-height stack')]))))
     stack)))

(defn- solve [move-pattern n]
  (let [stack {0 (set (range 0 7))}
        moves (->> move-pattern
                   cycle
                   (interpose "v"))]
    (stack-height (move-rocks stack moves (get-rocks n) n {}))))

(defn -main [filename]
  (let [move-pattern (read-move-pattern filename)]
    (pp/pprint (solve move-pattern 2022))
    (pp/pprint (solve move-pattern 1000000000000))))
