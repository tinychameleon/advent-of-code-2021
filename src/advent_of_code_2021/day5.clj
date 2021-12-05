(ns advent-of-code-2021.day5
  (:require [advent-of-code-2021.core :as core]))

(def coordinate-regex #"^(\d+),(\d+) -> (\d+),(\d+)$")

(defn parse-coordinate-line
  [line]
  (let [[_ & coords] (re-find coordinate-regex line)
        [x1 y1 x2 y2] (map core/str->int coords)]
    {:x1 x1 :y1 y1 :x2 x2 :y2 y2}))

(defn parse-thermal-vent-readings
  [filename]
  (map parse-coordinate-line (core/read-lines filename)))

(defn vertical-or-horizontal-vent?
  [{:keys [x1 y1 x2 y2]}]
  (or (= x1 x2) (= y1 y2)))

(defn coordinate-range
  [a b]
  (cond
    (> a b) (range a (dec b) -1)
    (< a b) (range a (inc b))
    :else (repeat a)))

(defn point-seq
  [{:keys [x1 y1 x2 y2]}]
  (let [xs (coordinate-range x1 x2)
        ys (coordinate-range y1 y2)]
    (core/zip xs ys)))

(defn nil-inc
  [n]
  (inc (or n 0)))

(defn record-vent-point
  [vent-map point]
  (update vent-map point nil-inc))

(defn overlay-vents-into-map
  [readings]
  (reduce #(reduce record-vent-point %1 (point-seq %2)) {} readings))

(def no-diagonal-lines (partial filter vertical-or-horizontal-vent?))

(defn solver
  [pred]
  (->> (parse-thermal-vent-readings "input/day5.txt")
       pred
       overlay-vents-into-map
       vals
       (filter #(> % 1))
       count))

(def part-1 (partial solver no-diagonal-lines))

(def part-2 (partial solver identity))
