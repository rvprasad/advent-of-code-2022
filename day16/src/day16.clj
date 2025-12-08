(ns day16
  (:require
   [clojure.pprint :as pp]
   [clojure.set :as set]
   [clojure.string :as str]))

(defn- read-report [s]
  (->> s
       slurp
       str/split-lines
       (map #(re-matches #"Valve ([A-Z]+) .*=(\d+); [a-z ]* ([A-Z, ]+)" %))
       (reduce (fn [acc [_ v r succs]]
                 (assoc acc v [(Integer/parseInt r)
                               (str/split succs #", ")]))
               {})))

(defn- process-state [valve-2-rate-n-succs {:keys [valve open] :as item}]
  (let [path-rate (->> (:open item)
                       (map #(first (get valve-2-rate-n-succs %)))
                       (reduce +))
        prototype (update item :path-score #(+ % path-rate))
        [rate succs] (get valve-2-rate-n-succs valve)]
    (concat (map #(assoc prototype :valve %) succs)
            (if (and (> rate 0) (not (open valve)))
              [(assoc prototype :open (conj open valve))]
              []))))

(defn- process-state-frontier [valve-2-rate-n-succs worklist]
  (->> worklist
       (map #(process-state valve-2-rate-n-succs %))
       (apply concat)
       (group-by #(select-keys % [:valve :open]))
       (map (fn [[m v]] 
              (assoc m :path-score (apply max (map :path-score v)))))))

(defn- generate-state [valve-2-rate-n-succs]
  (iterate (partial process-state-frontier valve-2-rate-n-succs)
           [{:valve "AA" :open #{} :path-score 0}]))

(defn- solve-part-1 [valve-2-rate-n-succs]
  (->> (generate-state valve-2-rate-n-succs)
       (drop 30)
       first
       (map :path-score)
       (apply max)))

(defn- solve-part-2 [valve-2-rate-n-succs]
  (let [opened-2-score (->> (generate-state valve-2-rate-n-succs)
                            (drop 26)
                            first
                            (map (fn [x] {(:open x) (:path-score x)}))
                            (reduce (partial merge-with max)))]
    (reduce max (for [[opened1 score1] opened-2-score
                      [opened2 score2] opened-2-score
                      :when (empty? (set/intersection opened1 opened2))]
                  (+ score1 score2)))))

(defn -main [filename]
  (let [valve-2-rate-n-succs (read-report filename)]
    (pp/pprint (solve-part-1 valve-2-rate-n-succs))
    (pp/pprint (solve-part-2 valve-2-rate-n-succs))))
