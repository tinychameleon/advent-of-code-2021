(ns advent-of-code-2021.day7
  (:require [advent-of-code-2021.core :as core]
            [clojure.string :as str]))

(defn gaussian-sum
  [n]
  (/ (* (+ n 1) n) 2))

(defn mean
  [coll]
  (/ (reduce + coll) (count coll)))

(defn parse-crab-positions
  [filename]
  (->> (core/read-file filename)
       str/trim
       (core/str-split #",")
       (map core/str->int)))

(defn position-range
  [positions]
  (let [bound (core/ceiling (mean positions))]
    (range bound)))

(defn fuel-cost
  [positions transform target]
  (reduce (fn [fuel crab]
            (-> (- crab target) Math/abs transform (+ fuel)))
          0 positions))

(defn solver
  [transformer]
  (let [positions (parse-crab-positions "input/day7.txt")
        mapf (partial fuel-cost positions transformer)]
    (apply min (map mapf (position-range positions)))))

(def part-1 (partial solver identity))

(def part-2 (partial solver gaussian-sum))
