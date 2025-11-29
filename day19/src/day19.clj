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

(defn- build-robots [[hd & tl :as insts] state]
  (if (empty? insts)
    [state]
    (concat (build-robots tl state)
            (let [rem-resources (merge-with #(- %1 %2)
                                            (:resources state)
                                            (dissoc hd :robot))]
              (if (some neg? (vals rem-resources))
                []
                [(-> state
                     (assoc :resources rem-resources)
                     (update-in [:robots (:robot hd)] inc))])))))

(defn- process-state [{insts :insts} state]
  (let [robots (:robots state)]
    (->> (assoc state :robots {:ore 0 :clay 0 :obs 0 :geo 0})
         (build-robots insts)
         (map (fn [m] (update-vals m #(merge-with + % robots))))
         set)))

(def robot-types [:geo :obs :clay :ore])

(defn- process-states [[bp states]]
  [bp (->> states
           (map #(process-state bp %))
           (apply concat)
           (sort-by (fn [s]
                      (vec (interleave
                            (map #(get-in s [:robots %]) robot-types)
                            (map #(get-in s [:resources %]) robot-types))))
                    (comp - compare))
           (take 300)
           set)])

(defn- calculate-max-geodes [blueprints n]
  (let [start {:resources {:ore 0 :clay 0 :obs 0 :geo 0}
               :robots {:ore 1 :clay 0 :obs 0 :geo 0}}
        f (fn [bp] (->> (iterate process-states [bp #{start}])
                        (drop n)
                        first))
        g (fn [[bp result]]
            [bp (->> result
                     (map #(get-in % [:resources :geo]))
                     (apply max))])]
    (map (comp g f) blueprints)))

(defn- solve-part-1 [blueprints n]
  (->> (calculate-max-geodes blueprints n)
       (map (fn [[{id :id} i]] (* id i)))
       (apply +)))

(defn- solve-part-2 [blueprints n]
  (->> (calculate-max-geodes blueprints n)
       (map second)
       (apply *)))

(defn -main [filename]
  (let [blueprints (read-blueprints filename)]
    (pp/pprint (solve-part-1 blueprints 24))
    (pp/pprint (solve-part-2 (take 3 blueprints) 32))))
