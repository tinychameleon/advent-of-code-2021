(ns advent-of-code-2021.day17
  (:require [advent-of-code-2021.core :as core]))

(defn parse-target-area
  [filename]
  (let [[x1 x2 y2 y1] (map core/str->int (re-seq #"[\d-]+" (core/read-file filename)))]
    {:x1 x1 :y1 y1 :x2 x2 :y2 y2}))

(defn gaussian-sum
  [n]
  (/ (* (+ 1 n) n) 2))

(defn partial-gaussian-sum
  [n step]
  (- (gaussian-sum n) (gaussian-sum (- n step))))

(defn bounded-x-seq
  [lower upper step x]
  (let [sum (partial-gaussian-sum x step)]
    (cond
      (or (> step x) (> sum upper)) nil
      (< sum lower) (recur lower upper (inc step) x)
      :else (lazy-seq (cons {:step step :x x} (x-seq lower upper (inc step) x))))))

(defn bounded-x-steps
  [{:keys [x1 x2]}]
  (let [f (partial bounded-x-seq x1 x2 1)]
    (mapcat f (range (inc x2)))))

(defn cons-y-map
  [step y]
  {:y y :sum (partial-gaussian-sum y step)})

(defn bounded-y-seq
  [lower upper step]
  (filter #(<= lower (:sum %) upper)
          (map (partial cons-y-map step) (range lower (Math/abs lower)))))

(defn bounded-y-steps
  [{lower :y2 upper :y1}]
  (let [limit (* (Math/abs lower) 2)
        steps (range 1 (inc limit))
        f #(vector % (bounded-y-seq lower upper %))]
    (reduce conj {} (map f steps))))

(defn freefall-ys
  [x step ys]
  (when (= x step)
    (->> ys keys (filter #(> % step)) (mapcat #(ys %)))))

(defn ys-for
  [ys {:keys [x step]}]
  (concat (ys step) (freefall-ys x step ys)))

(defn coord-seq
  [xs ys]
  (for [xm xs, ym (ys-for ys xm), :let [y (:y ym)]]
    {:x (:x xm) :y y :height (gaussian-sum y)}))

(def bounded-axis-steps (juxt bounded-x-steps bounded-y-steps))

(def max-height (partial max-key :height))

(defn solver
  [f]
  (->> "input/day17.txt" parse-target-area bounded-axis-steps (apply coord-seq) f))

(def part-1 (partial solver #(->> % (reduce max-height) :height)))

(def part-2 (partial solver #(->> % (into #{}) count)))
