(ns advent-of-code-2021.day1
  (:require [advent-of-code-2021.core :as core]))

(defn count-depth-increases
  [depths]
  (let [count-if-greater (fn [total [start end]]
                           (if (> end start)
                             (inc total)
                             total))]
    (reduce count-if-greater 0 depths)))

(defn part-1
  []
  (let [depths (->> (core/read-lines "input/day1.txt")
                    (map core/str->int)
                    (core/adjacent-tuples))]
    (count-depth-increases depths)))

(defn part-2
  []
  (let [depths (->> (core/read-lines "input/day1.txt")
                    (map core/str->int)
                    (core/adjacent-tuples 3)
                    (map #(reduce + %))
                    (core/adjacent-tuples))]
    (count-depth-increases depths)))
