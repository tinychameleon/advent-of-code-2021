(ns advent-of-code-2021.day6
  (:require [advent-of-code-2021.core :as core]
            [clojure.string :as str]))

(def ^:const spawn-ready-age 0)
(def ^:const newly-spawned-age 8)
(def ^:const cycle-reset-age 6)

(defn parse-lanternfish-ages
  [filename]
  (->> (core/read-file filename)
       str/trim
       (core/str-split #",")
       (mapv core/str->int)
       core/value-counts))

(defn simulate-age
  [age]
  (if (= age 0) cycle-reset-age (dec age)))

(def safe+ (fnil + 0))

(defn update-population
  [population k v]
  (update population k safe+ v))

(defn simulate-population
  [population]
  (reduce (fn [pop [k v]]
            (update-population pop (simulate-age k) v))
          {}
          population))

(defn simulate-cycle
  [population]
  (let [new-fish (get population spawn-ready-age 0)]
    (update-population (simulate-population population) newly-spawned-age new-fish)))

(defn solver
  [days]
  (let [population (parse-lanternfish-ages "input/day6.txt")
        simulation (iterate simulate-cycle population)
        result (first (drop days simulation))]
    (reduce + (vals result))))

(def part-1 (partial solver 80))

(def part-2 (partial solver 256))
