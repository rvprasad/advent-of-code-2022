(ns day11
  (:require
   [clojure.string :as str]))

(defrecord Monkey [items op div t-next f-next])

(defn- get-op-fn [line]
  (let [[op v] (rest (str/split (last (str/split line #"= ")) #" "))
        op (if (= "*" op) * +)]
    (if (= "old" v) #(op % %) #(op % (parse-long v)))))

(defn- convert [[id-line items-line op-line tst-line t-line f-line]]
  (let [id (str/replace id-line #"[Monkey :]" "")
        items (str/split (last (str/split items-line #": ")) #", ")
        op-fn (get-op-fn op-line)
        div (Integer. (last (str/split tst-line #" ")))
        t-next (last (str/split t-line #" "))
        f-next (last (str/split f-line #" "))]
    [id (->Monkey items op-fn div t-next f-next)]))

(defn- read-monkeys [s]
  (->> s
       slurp
       (#(str/split % #"\n\n"))
       (map str/split-lines)
       (map convert)
       (into {})))

(defn- inspect-items [limit worry {:keys [items op div t-next f-next]}]
  (let [new-items (map #(rem (long (/ (op (parse-long %)) worry)) limit) items)
        ids (map #(if (= 0 (rem % div)) t-next f-next) new-items)
        helper (fn [acc [item id]] (assoc acc id (conj (acc id []) item)))]
    (reduce helper {} (map vector (map str new-items) ids))))

(defn- perform-round [id-2-monkeys id-2-num-items [id & ids] inspect-fn]
  (let [monkey (id-2-monkeys id)
        id-2-new-items (inspect-fn monkey)
        update-monkey (fn [[id m]]
                        [id (assoc m :items (concat (:items m)
                                                    (id-2-new-items id '())))])
        new-id-2-monkeys (->> id-2-monkeys
                              (map update-monkey)
                              (into {})
                              (#(assoc % id (assoc monkey :items []))))
        new-id-2-num-items (assoc id-2-num-items id (+ (id-2-num-items id 0)
                                                       (count (:items monkey))))]
    (if (empty? ids)
      [new-id-2-monkeys new-id-2-num-items]
      (recur new-id-2-monkeys new-id-2-num-items ids inspect-fn))))

(defn- solve-puzzle [id-2-monkeys id-2-num-items inspect-fn round]
  (if (= round 0)
    (reduce * (take-last 2 (sort (vals id-2-num-items))))
    (let [ids (sort (keys id-2-monkeys))
          [f s] (perform-round id-2-monkeys id-2-num-items ids inspect-fn)]
      (recur f s inspect-fn (dec round)))))

(defn -main [filename]
  (let [id-2-monkeys (read-monkeys filename)
        limit (reduce * 1 (map #(:div %) (vals id-2-monkeys)))
        id-2-num-items (zipmap (keys id-2-monkeys) (repeat 0))
        part1-inspect (partial inspect-items limit 3)
        part2-inspect (partial inspect-items limit 1)]
    (println (solve-puzzle id-2-monkeys id-2-num-items part1-inspect 20))
    (println (solve-puzzle id-2-monkeys id-2-num-items part2-inspect 10000))))
