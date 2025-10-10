(ns day11
  (:require
   [clojure.string :as str]))

(defrecord Monkey [items op tst])

(defn- get-items [line]
  (mapv #(Integer. %) (str/split (last (str/split line #": ")) #", ")))

(defn- get-op-fn [line worry-factor scaling-factor]
  (let [[op v] (rest (str/split (last (str/split line #"= ")) #" "))
        op (if (= "*" op) * +)
        op-fn (if (= "old" v) #(op % %) #(op % (Integer. v)))]
    #(rem (bigint (/ (op-fn %) worry-factor)) scaling-factor)))

(defn- get-tst-fn [tst-line t-line f-line]
  (let [div (Integer. (last (str/split tst-line #" ")))
        t-val (last (str/split t-line #" "))
        f-val (last (str/split f-line #" "))]
    #(if (= 0 (rem % div)) t-val f-val)))

(defn- convert [worry-factor scaling-factor
                [id-line items-line op-line tst-line tr-line fl-line]]
  [(str/replace id-line #"[Monkey :]" "")
   (->Monkey (get-items items-line)
             (get-op-fn op-line worry-factor scaling-factor)
             (get-tst-fn tst-line tr-line fl-line))])

(defn- read-monkeys [filename worry-factor]
  (let [file-content (slurp filename)
        scaling-factor (->>
                        file-content
                        (re-seq #"by \d+")
                        (map #(Integer. (str/replace % #"by " "")))
                        (apply *))]
    (->>
     (str/split file-content #"\n\n")
     (map str/split-lines)
     (map (partial convert worry-factor scaling-factor))
     (into {}))))

(defn- inspect-items [monkey]
  (let [items (:items monkey)
        op (:op monkey)
        tst (:tst monkey)
        new-items (map op items)
        ids (map tst new-items)
        helper (fn [acc [item id]] (assoc acc id (conj (acc id []) item)))]
    (reduce helper {} (map vector new-items ids))))

(defn- get-new-id-2-num-items [id monkey id-2-num-items]
  (assoc id-2-num-items id (+ (id-2-num-items id 0) (count (:items monkey)))))

(defn- perform-round [id-2-monkeys id-2-num-items [id & ids]]
  (let [monkey (id-2-monkeys id)
        id-2-new-items (inspect-items monkey)
        update-monkey (fn [[id monkey]]
                        [id (assoc monkey
                                   :items
                                   (concat (:items monkey)
                                           (id-2-new-items id '())))])
        new-id-2-monkeys (->>
                          id-2-monkeys
                          (map update-monkey)
                          (into {})
                          (#(assoc % id (assoc monkey :items []))))
        new-id-2-num-items (get-new-id-2-num-items id monkey id-2-num-items)]
    (if (empty? ids)
      [new-id-2-monkeys new-id-2-num-items]
      (recur new-id-2-monkeys new-id-2-num-items ids))))

(defn- calculate-monkey-business
  ([id-2-monkeys round]
   (calculate-monkey-business id-2-monkeys
                              (zipmap (keys id-2-monkeys) (repeat 0))
                              round))

  ([id-2-monkeys id-2-num-items round]
   (if (= round 0)
     (reduce * (take-last 2 (sort (vals id-2-num-items))))
     (let [[f s] (perform-round id-2-monkeys
                                id-2-num-items
                                (sort (keys id-2-monkeys)))]
       (recur f s (- round 1))))))

(defn -main [filename]
  (println (calculate-monkey-business (read-monkeys filename 3) 20))
  (println (calculate-monkey-business (read-monkeys filename 1) 10000)))
