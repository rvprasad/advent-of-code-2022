(ns day10
  (:require
   [clojure.java.io :as io]
   [clojure.set]
   [clojure.string :as str]))

(defn- read-program [filename]
  (->>
   filename
   (slurp)
   (str/split-lines)))

(defn- execute-inst [inst curr-cycle]
  (let [next-cycle (+ curr-cycle 1)]
    (condp re-matches inst
      #"addx (-?\d+)" :>> (fn [[_ n]]
                            [(Integer. n) [next-cycle (+ next-cycle 1)]])
      #"noop" [0 [next-cycle]])))

(defn- execute-program [program process]
  (loop [[x prev-cycle acc] [1 0 []] p program]
    (if (empty? p)
      acc
      (let [[x-delta curr-cycles] (execute-inst (first p) prev-cycle)
            new-x (+ x x-delta)
            new-acc (concat acc (process x curr-cycles))
            new-cycle (apply max curr-cycles)]
        (recur [new-x new-cycle new-acc] (rest p))))))

(defn- get-hit-cycle [x cycles]
  (let [hits (clojure.set/intersection (set cycles) #{20 60 100 140 180 220})]
    (if-let [e (seq hits)] [(* x (first e))] [])))

(defn- sum-of-signal-strengths [program]
  (reduce + (execute-program program get-hit-cycle)))

(defn- get-pixel-positions [x cycles]
  (let [cycles (map #(rem % 40) (map #(- % 1) cycles))]
    (map #(if (some #{(- x 1) x (+ x 1)} [%]) \# \.) cycles)))

(defn- draw-pixels [program]
  (let [pixels (execute-program program get-pixel-positions)]
    (doseq [x (partition 40 pixels)]
      (println (str/join x)))))

(defn -main [filename]
  (let [program (read-program filename)]
    (println (sum-of-signal-strengths program))
    (draw-pixels program)))
