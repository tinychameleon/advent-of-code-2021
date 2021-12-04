(ns advent-of-code-2021.day4
  (:require [advent-of-code-2021.core :as core]
            [clojure.string :as str]))

(def ^:const board-line-size 5)
(def ^:const board-grid-size 25)
(def ^:const board-cell-marked-val :m)
(def ^:const board-cell-cleared-val :x)

(def board-rows (partial partition board-line-size))

(defn board-cols
  [boards]
  (->> (board-rows (board-rows boards))
       (map core/zip)
       (reduce into [])))

(def all-cells-marked? (partial every? #(= % board-cell-marked-val)))

(defn is-winner?
  [[_ line]]
  (all-cells-marked? line))

(defn winning-lines
  [boards]
  (let [rows (core/with-index (board-rows boards))
        cols (core/with-index (board-cols boards))
        winners (map first (filter is-winner? (concat rows cols)))]
    (sort winners)))

(defn winning-board
  [index boards]
  (let [board-number (core/floor (/ index board-line-size))
        start-index (* board-number board-grid-size)]
    [start-index (into [] (take board-grid-size (drop start-index boards)))]))

(defn winning-boards?
  [boards]
  (when-let [winners (winning-lines boards)]
    (map #(winning-board % boards) winners)))

(defn clear-winning-board
  [boards [index _]]
  (let [indices (range index (+ index board-grid-size))]
    (reduce #(assoc %1 %2 board-cell-cleared-val) boards indices)))

(defn mark-drawn-number
  [boards n]
  (replace {n board-cell-marked-val} boards))

(defn next-winning-boards
  [[n & rest] boards]
  (when n
    (let [boards (mark-drawn-number boards n)]
      (if-let [winners (winning-boards? boards)]
        [n (map second winners) rest (reduce clear-winning-board boards winners)]
        (recur rest boards)))))

(defn winning-boards
  [numbers boards]
  (lazy-seq
   (when-let [[n winners rest boards] (next-winning-boards numbers boards)]
     (reduce #(cons [n %2] %1) (winning-boards rest boards) (reverse winners)))))

(defn board-score
  [n board]
  (letfn [(unmarked-sum [b] (reduce + (remove keyword? b)))]
    (* (unmarked-sum board) n)))

(defn parse-bingo-data
  [filename]
  (let [[drawn & boards-data] (core/read-lines filename)
        numbers (map core/str->int (str/split drawn #","))
        boards (->> (str/join "\n" boards-data)
                    str/trim
                    (core/str-split #"\s+")
                    (map core/str->int)
                    (into []))]
    [numbers boards]))

(defn solver
  [f]
  (->> (parse-bingo-data "input/day4.txt")
       (apply winning-boards)
       f
       (apply board-score)))

(def part-1 (partial solver first))
(def part-2 (partial solver last))
