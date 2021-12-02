(ns advent-of-code-2021.day1
  (:require [advent-of-code-2021.core :as core]))

(defn part-1
  []
  (count-increases (core/read-ints "input/day1.txt")))

(defn part-2
  []
  (->> (core/read-ints "input/day1.txt")
       (partition 3 1)
       (map #(reduce + %))
       count-increases))

(defn- count-increases
  [coll]
  (->> (partition 2 1 coll)
       (remove #(apply >= %))
       count))
