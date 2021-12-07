(ns advent-of-code-2021.day7
  (:require [advent-of-code-2021.core :as core]
            [clojure.string :as str]))

;; Optimization: reduce inputs to a map of value counts yields 40% speed up.
;; Original without value counts:
;;   https://github.com/tinychameleon/advent-of-code-2021/blob/36034357c87f8160bd2ae673dd4a0311ddf56f3e/src/advent_of_code_2021/day7.clj

(defn gaussian-sum
  [n]
  (/ (* (+ n 1) n) 2))

(defn mean
  [hash-map]
  (/ (reduce #(+ (apply * %2) %1) 0 hash-map) (reduce + (vals hash-map))))

(defn parse-crab-positions
  [filename]
  (->> (core/read-file filename)
       str/trim
       (core/str-split #",")
       (map core/str->int)
       core/value-counts))

(defn position-range
  [positions]
  (range (mean positions)))

(defn fuel-cost
  [positions transform target]
  (reduce (fn [fuel [crab amount]]
            (-> (- crab target) Math/abs transform (* amount) (+ fuel)))
          0 positions))

(defn solver
  [transformer]
  (let [positions (parse-crab-positions "input/day7.txt")
        mapf (partial fuel-cost positions transformer)]
    (apply min (map mapf (position-range positions)))))

(def part-1 (partial solver identity))

(def part-2 (partial solver gaussian-sum))
