(ns advent-of-code-2021.day3
  (:require [advent-of-code-2021.core :as core]
            [clojure.string :as str]))

;; This is the first problem where mapping my thought process into Clojure
;; idioms has hit some friction. Likely this is longer and more complex
;; than it needs to be.

(defn binary->vec
  [binary-string]
  (into [] (map core/str->int (core/str-split-chars binary-string))))

(defn read-report-into-vec
  [filename]
  (into [] (map binary->vec (core/read-lines filename))))

(def bit-length (comp count first))

(def bits->int (comp (partial core/str->int 2) str/join))

(defn extract-bits-by
  [f numbers]
  (->> (range (bit-length numbers))
       (map #(f numbers %))
       bits->int))

(defn bit-mode-by
  [f numbers pos]
  (->> (map #(get % pos) numbers)
       (group-by identity)
       (map (fn [[k v]] [k (count v)]))
       (sort-by second)
       f
       first))

(defn bit-compare
  [f default [[_ low] [_ high] :as values]]
  (if (= low high)
    [default]
    (f values)))

(defn most-common-bit-value
  [numbers pos]
  (bit-mode-by (partial bit-compare second 1) numbers pos))

(defn least-common-bit-value
  [numbers pos]
  (bit-mode-by (partial bit-compare first 0) numbers pos))

(def gamma-rate (partial extract-bits-by most-common-bit-value))

(def epsilon-rate (partial extract-bits-by least-common-bit-value))

(defn power-consumption
  [numbers]
  (* (gamma-rate numbers) (epsilon-rate numbers)))

(defn bit-filter-by
  [f numbers pos]
  (if (= (count numbers) 1)
    numbers
    (let [bit (f numbers pos)]
      (filter #(= (get % pos) bit) numbers))))

(defn extract-filter-by
  [f numbers]
  (->> (range (bit-length numbers))
       (reduce (partial bit-filter-by f) numbers)
       first
       bits->int))

(def oxygen-generator-rating (partial extract-filter-by most-common-bit-value))

(def co2-scrubber-rating (partial extract-filter-by least-common-bit-value))

(defn life-support-rating
  [numbers]
  (* (oxygen-generator-rating numbers) (co2-scrubber-rating numbers)))

(defn part-1
  []
  (-> "input/day3.txt" read-report-into-vec power-consumption))

(defn part-2
  []
  (-> "input/day3.txt" read-report-into-vec life-support-rating))
