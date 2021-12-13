(ns advent-of-code-2021.day13
  (:require [advent-of-code-2021.core :as core]))

(defn str->point
  [s]
  (mapv core/str->int (clojure.string/split s #",")))

(defn parse-points
  [lines]
  (set (map str->point lines)))

(def ^:const fold-regex #"(x|y)=(\d+)")

(defn str->fold
  [s]
  (let [[_ axis val] (re-find fold-regex s)]
    [axis (core/str->int val)]))

(defn parse-folds
  [lines]
  (map str->fold lines))

(defn parse-instructions
  [filename]
  (let [lines (core/read-lines filename)
        [point-data _ fold-data] (partition-by #(= % "") lines)]
    {:points (parse-points point-data) :folds (parse-folds fold-data)}))

(defn mirror-function
  [[axis val]]
  (case axis
    "x" (fn [[x y]] [(+ val val (- x)) y])
    "y" (fn [[x y]] [x (+ val val (- y))])))

(defn axis-compare
  [[axis val]]
  (case axis
    "x" (fn [[x _]] (> x val))
    "y" (fn [[_ y]] (> y val))))

(defn mirror-points
  [points fold]
  (let [foldables (into #{} (filter (axis-compare fold) points))
        static (clojure.set/difference points foldables)
        mirror (mirror-function fold)]
    (reduce #(conj %1 (mirror %2)) static foldables)))

(defn apply-fold
  [{:keys [points folds] :as instructions}]
  (-> instructions
      (assoc :points (mirror-points points (first folds)))
      (assoc :folds (rest folds))))

(defn fold-all
  [{:keys [folds] :as instructions}]
  (if (empty? folds)
    instructions
    (recur (apply-fold instructions))))

(defn max-vector
  [[c r] [x y]]
  [(max c x) (max r y)])

(defn print-points
  [{:keys [points]}]
  (let [[cols rows] (map inc (reduce max-vector points))
        point->char #(if (points %) "█" " ")]
    (for [y (range rows)
          :let [row-points (map vector (range cols) (repeat y))]]
      (println (apply str (map point->char row-points))))))

(defn apply-to-instructions
  [f]
  (fn [] (-> (parse-instructions "input/day13.txt") f)))

(def part-1 (apply-to-instructions (comp count :points apply-fold)))

(def part-2 (apply-to-instructions (comp print-points fold-all)))
