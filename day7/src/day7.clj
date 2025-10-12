(ns day7
  (:require
   [clojure.string :as str]))

(defn- gather-size-path [[acc stack] s]
  (condp re-matches s
    #"\$ cd \.\." :>> (fn [_] [acc (pop stack)])
    #"\$ cd (.*)" :>> #(vector acc (conj stack (second %)))
    #"(\d+) (.*)" :>> #(vector (conj acc (conj stack (Integer. (second %))))
                               stack)
    [acc stack]))

(defn- get-folder-sizes [acc [size & path]]
  (loop [a acc p path]
    (if (empty? p)
      a
      (recur (assoc a p (+ size (get a p 0))) (pop p)))))

(defn -main [filename]
  (let [path-2-sizes (->> filename
                          slurp
                          str/split-lines
                          (reduce gather-size-path ['() '()])
                          first
                          (reduce get-folder-sizes {}))
        required-space (- (get path-2-sizes '("/")) 40000000)]
    (println (reduce + (filter #(< % 100000) (vals path-2-sizes))))
    (println (apply min (filter #(> % required-space) (vals path-2-sizes))))))
