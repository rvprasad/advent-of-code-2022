(ns day5
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn read-stacks-and-procedure
  [filename]
  (let [content (slurp filename)
        [part1 part2] (str/split content #"\n\n")
        stacks (->>
                (->
                 part1
                 (str/replace #"[\[\]]" " ")
                 (str/split #"\n"))
                (map #(str/split % #""))
                (apply map list)
                (map reverse)
                (map (fn [x] (filter #(not= % " ") x)))
                (filter #(not (empty? %)))
                (reduce (fn [m [hd & tl]] (assoc m (Integer. hd) tl)) {}))
        procedure (->>
                   (->
                    part2
                    (str/replace #"(move|from|to) " "")
                    (str/split  #"\n"))
                   (map #(str/split % #" "))
                   (map (fn [x] (map #(Integer. %) x))))]
    [stacks procedure]))

(defn find-top-box-after-rearrangment
  [stacks procedure order]
  (if (empty? procedure)
    (str/join (map last (map #(get stacks (+ % 1)) (range (count stacks)))))
    (let [[[n from to] & tl] procedure
          from-stack (get stacks from)
          to-stack (get stacks to)
          crates (order (take-last n from-stack))
          new-stacks (assoc stacks
                            from (drop-last n from-stack)
                            to (concat to-stack crates))]
      (recur new-stacks tl order))))

(defn -main
  [filename]
  (let [[stacks procedure] (read-stacks-and-procedure filename)]
    (println (find-top-box-after-rearrangment stacks procedure reverse))
    (println (find-top-box-after-rearrangment stacks procedure identity))))
