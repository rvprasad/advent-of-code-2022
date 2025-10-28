(ns day16
  (:require
   [clojure.pprint :as pp]
   [clojure.string :as str]))

(defn- read-report [s]
  (->> s
       slurp
       str/split-lines
       (map #(re-matches #"Valve ([A-Z]+) has flow rate=(\d+); tunnels? leads? to valves? ([A-Z, ]+)" %))
       (reduce (fn [acc [_ v r succs]]
                 (assoc acc v [(Integer/parseInt r) (str/split succs #", ")]))
               {})))

(defn -main [filename]
  (let [v-2-rate-and-succs (read-report filename)]
    (pp/pprint v-2-rate-and-succs)))

