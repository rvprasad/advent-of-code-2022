(ns day19
  (:require
   [clojure.pprint :as pp]
   [clojure.string :as str]))

(defn- read-blueprints [s]
  (->> s
       slurp
       str/split-lines
       (map #(map Integer/parseInt (re-seq #"\d+" %)))
       (map (fn [[id or-ore cl-ore ob-ore ob-clay ge-ore ge-obs]]
              {:id id
               :insts [{:robot :geo :ore ge-ore :obs ge-obs}
                       {:robot :obs :ore ob-ore :clay ob-clay}
                       {:robot :clay :ore cl-ore}
                       {:robot :ore :ore or-ore}]}))))

(def robot-types [:geo :obs :clay :ore])

(defn- build-robots [[hd & tl :as insts] state]
  (if (empty? insts)
    [state]
    (let [new-robots (build-robots tl state)
          new-state (-> state
                        (update :resources
                                #(merge-with - % (dissoc hd :robot)))
                        (update-in [:robots (:robot hd)] inc))]
      (if (some neg? (vals (:resources new-state)))
        new-robots
        (conj new-robots new-state)))))

(defn- process-state [{insts :insts} state]
  (->> (assoc state :robots {:ore 0 :clay 0 :obs 0 :geo 0})
       (build-robots insts)
       (map (fn [m] (update-vals m #(merge-with + % (:robots state)))))))

(defn- pick-relevant-states [states]
  (let [keyfn (fn [k s] (mapv #(- (get-in s [k %])) robot-types))]
    (->> states
         (group-by (partial keyfn :robots))
         (sort-by first)
         (map second)
         (take 100)
         (map #(take 5 (sort-by (partial keyfn :resources) %)))
         (apply concat))))

(defn- process-states [[bp states]]
  [bp (->> states
           (map #(process-state bp %))
           (apply concat)
           pick-relevant-states)])

(defn- process-blueprint [bp n]
  (let [start-state {:resources {:ore 0 :clay 0 :obs 0 :geo 0}
                     :robots {:ore 1 :clay 0 :obs 0 :geo 0}}]
    [bp (as-> [bp #{start-state}] v
          (iterate process-states v)
          (nth v n)
          (second v)
          (map #(get-in % [:resources :geo]) v)
          (apply max v))]))

(defn- solve-part-1 [blueprints n]
  (->> blueprints
       (map #(process-blueprint % n))
       (map #(* (:id (first %)) (second %)))
       (apply +)))

(defn- solve-part-2 [blueprints n]
  (->> blueprints
       (map #(process-blueprint % n))
       (map second)
       (apply *)))

(defn -main [filename]
  (let [blueprints (read-blueprints filename)]
    (pp/pprint (solve-part-1 blueprints 24))
    (pp/pprint (solve-part-2 (take 3 blueprints) 32))))
