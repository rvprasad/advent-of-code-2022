(ns day5
  (:require
   [clojure.string :as str]))

(defn- read-stacks-and-procedure [s]
  (let [content (slurp s)
        [part1 part2] (str/split content #"\n\n")
        stacks (->> (-> part1
                        (str/replace #"[\[\]]" " ")
                        str/split-lines)
                    (map #(str/split % #""))
                    (apply map list)
                    (map reverse)
                    (map (fn [x] (filter #(not= % " ") x)))
                    (filter seq)
                    (reduce (fn [m [hd & tl]] (assoc m (Integer. hd) tl)) {}))
        procedure (->> (-> part2
                           (str/replace #"(move|from|to) " "")
                           str/split-lines)
                       (map #(str/split % #" "))
                       (map (fn [x] (map #(Integer. %) x))))]
    [stacks procedure]))

(defn- rearrange-and-get-top-boxes [stack-map procedure order]
  (if (empty? procedure)
    (str/join (map last (map #(stack-map (inc %)) (range (count stack-map)))))
    (let [[[n from to] & tl] procedure
          from-stack (get stack-map from)
          [bottom-crates top-crates] (split-at (- (count from-stack) n)
                                               from-stack)
          new-to-stack (concat (stack-map to) (order top-crates))]
      (recur (assoc stack-map from bottom-crates to new-to-stack) tl order))))

(defn -main [filename]
  (let [[stacks procedure] (read-stacks-and-procedure filename)]
    (println (rearrange-and-get-top-boxes stacks procedure reverse))
    (println (rearrange-and-get-top-boxes stacks procedure identity))))
