(ns day7
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn- gather-size-path-seq [[acc stack] line]
  (condp re-matches line
    #"\$ cd \.\." :>> (fn [_] [acc (pop stack)])
    #"\$ cd (.*)" :>> (fn [x] [acc (conj stack (second x))])
    #"(\d+) (.*)" :>> (fn [[_ size file]]
                        [(conj acc (conj stack (Integer. size))) stack])
    [acc stack]))

(defn- read-size-path-seqs [filename]
  (with-open [file (io/reader filename)]
    (first (reduce gather-size-path-seq
                   ['() '()]
                   (vec (line-seq file))))))

(defn- get-folder-sizes [acc [size & path]]
  (loop [a acc p path]
    (if (empty? p)
      a
      (recur (assoc a p (+ size (get a p 0))) (pop p)))))

(defn -main [filename]
  (let [size-file-seqs (read-size-path-seqs filename)
        path-2-sizes (reduce get-folder-sizes {} size-file-seqs)
        required-space (- (get path-2-sizes '("/")) 40000000)]
    (println (reduce + (filter #(< % 100000) (vals path-2-sizes))))
    (println (apply min (filter #(> % required-space)
                                (vals path-2-sizes))))))
