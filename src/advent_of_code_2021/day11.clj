(ns advent-of-code-2021.day11
  (:require [advent-of-code-2021.core :as core]))

(def ^:const flash-point 10)

(defn adjacent-positions
  [{:keys [rows columns]} [y x]]
  (for [dy [-1 0 1] dx [-1 0 1]
        :when (not= [dy dx] [0 0])
        :let [r (+ dy y) c (+ dx x)]
        :when (and (< -1 r rows) (< -1 c columns))]
    [r c]))

(defn all-positions-seq
  [{:keys [rows columns]}]
  (for [y (range rows) x (range columns)]
    [y x]))

(defn energy-increase
  [state positions]
  (reduce (fn [s [y x]]
            (update-in s [:energy y x] inc))
          state positions))

(defn at-flash-point?
  [{:keys [energy]} pos]
  (>= (get-in energy pos) flash-point))

(defn flashers-seq
  [state positions]
  (filter #(at-flash-point? state %) positions))

(defn flash-chain
  ([state]
   (flash-chain state #{} (flashers-seq state (all-positions-seq state))))
  ([state visited [pos & positions]]
   (cond
     (nil? pos) {:state state :visited visited}
     (visited pos) (recur state visited positions)
     (not (at-flash-point? state pos)) (recur state visited positions)
     :else (let [neighbours (adjacent-positions state pos)
                 next-state (energy-increase state neighbours)]
             (recur next-state (conj visited pos) (apply conj positions neighbours))))))

(defn reset-flashed
  [{:keys [state visited] :as meta-state}]
  (assoc meta-state :state (reduce (fn [s [y x]]
                                     (assoc-in s [:energy y x] 0))
                                   state visited)))

(defn flash-seq
  [state]
  (let [next-state (-> state
                       (energy-increase (all-positions-seq state))
                       flash-chain
                       reset-flashed)]
    (lazy-seq (cons next-state (flash-seq (:state next-state))))))

(defn parse-energy-levels
  [filename]
  (let [energy-data (->> (core/read-lines filename)
                         (mapv #(mapv core/str->int (core/str-split-chars %))))]
    {:energy energy-data
     :rows (count energy-data)
     :columns (count (get energy-data 0))}))

(defn part-1
  []
  (->> (parse-energy-levels "input/day11.txt")
       flash-seq
       (take 100)
       (map (comp count :visited))
       (reduce +)))

(defn part-2
  []
  (let [state (parse-energy-levels "input/day11.txt")
        total-positions (* (:rows state) (:columns state))]
    (->> (flash-seq state)
         (take-while #(not= (count (:visited %)) total-positions))
         count
         inc)))
