(ns day21
  (:require
   [clojure.pprint :as pp]
   [clojure.string :as str]))

(defn- read-op-graph [s]
  (->> s
       slurp
       str/split-lines
       (reduce
        (fn [graph l]
          (let [[hd & tmp] (str/split l #" ")
                trg (str/replace hd ":" "")]
            (if (= (count tmp) 1)
              (update graph trg (fn [[preds op]]
                                  [preds (parse-long (first tmp))]))
              (let [[op1 op op2] tmp]
                (update graph trg (fn [[_ _]] [[op1 op2] op]))))))
        {})))

(defn- solve-part-1
  ([graph] (solve-part-1 "root" graph))
  ([node graph] ((solve-part-1 node graph {}) node))
  ([node graph memory] (if (contains? memory node)
                         memory
                         (let [[[op1 op2] op-or-val] (graph node)]
                           (if (nil? op1)
                             (assoc memory node op-or-val)
                             (let [op1-val ((solve-part-1 op1 graph memory) op1)
                                   op2-val ((solve-part-1 op2 graph memory) op2)
                                   op (eval (read-string op-or-val))]
                               (assoc memory node (op op1-val op2-val))))))))

(defn- evaluate [[pred1 pred2] graph]
  [(try (solve-part-1 pred1 graph)
        (catch NullPointerException e nil))
   (try (solve-part-1 pred2 graph)
        (catch NullPointerException e nil))])

(defn- solve-part-2
  ([graph]
   (let [new-graph (dissoc graph "humn")
         [pred1 pred2 :as preds] (first (new-graph "root"))
         [val1 val2] (evaluate preds new-graph)]
     (if (nil? val1)
       (solve-part-2 val2 pred1 new-graph)
       (solve-part-2 val1 pred2 new-graph))))
  ([value node graph]
   (if (= node "humn")
     value
     (let [[[pred1 pred2 :as preds] op] (graph node)
           [l-val r-val] (evaluate preds graph)
           [pred-value pred-node] (if (nil? l-val) [r-val pred1] [l-val pred2])]
       (-> (case op
             "+" [- identity]
             "-" (if (nil? l-val) [+ identity] [- reverse])
             "*" [/ identity]
             "/" (if (nil? l-val) [* identity] [/ reverse]))
           ((fn [[op transform]] (apply op (transform [value pred-value]))))
           (solve-part-2 pred-node graph))))))

(defn -main [filename]
  (let [graph (read-op-graph filename)]
    (pp/pprint (solve-part-1 graph))
    (pp/pprint (solve-part-2 graph))))
