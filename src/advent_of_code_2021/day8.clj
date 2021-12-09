(ns advent-of-code-2021.day8
  (:require [advent-of-code-2021.core :as core]
            [clojure.string :as str]))

(def string-sort (comp (partial apply str) sort))

(defn sort-digits
  [digits]
  (map string-sort digits))

(defn parse-display-line
  [line]
  (let [[in out] (str/split line #" \| ")]
    {:inputs (sort-digits (core/str-split-words in))
     :outputs (sort-digits (core/str-split-words out))}))

(defn parse-display-signals
  [filename]
  (->> (core/read-lines filename)
       (map parse-display-line)))

(defn unique-digit
  [digit]
  [(case (count digit)
     2 1
     3 7
     4 4
     7 8
     nil) (into #{} digit)])

(def first-nil? (comp nil? first))

(defn signal-state
  [signals]
  (let [digits (map unique-digit signals)
        known (into {} (remove first-nil? digits))
        unknown (map second (filter first-nil? digits))]
    [known unknown]))

(def signal-rules
  [{:number 3 :digit-length 5 :diff-by 1 :diff-eq 3}
   {:number 2 :digit-length 5 :diff-by 4 :diff-eq 3}
   {:number 5 :digit-length 5 :diff-by 4 :diff-eq 2}
   {:number 9 :digit-length 6 :diff-by 3 :diff-eq 1}
   {:number 6 :digit-length 6 :diff-by 5 :diff-eq 1}
   {:number 0 :digit-length 6 :diff-by 5 :diff-eq 2}])

(defn length-of
  [rule]
  #(= (count %) (:digit-length rule)))

(defn diff-by
  [known rule]
  #(->> (:diff-by rule) known (apply disj %) count (= (:diff-eq rule))))

(defn process-signals
  [rules signals]
  (let [[a b] (signal-state signals)]
    (loop [known a
           unknown b
           [rule & rest] rules]
      (if rule
        (let [pred (every-pred (length-of rule) (diff-by known rule))
              match (first (filter pred unknown))]
          (recur (assoc known (:number rule) match) (remove #(= % match) unknown) rest))
        (reduce-kv (fn [m val digit]
                     (assoc m (apply str digit) val))
                   {}
                   known)))))

(defn seq->int
  [s]
  (let [powers (map #(int (Math/pow 10 %)) (reverse (range (count s))))]
    (reduce + (map * s powers))))

(defn signal-output
  [signal-rules signal-data]
  (let [inputs (->> signal-data
                    :inputs
                    (map string-sort)
                    (into #{})
                    (process-signals signal-rules))
        outputs (map inputs (:outputs signal-data))]
    (seq->int outputs)))

(defn part-1
  []
  (->> (parse-display-signals "input/day8.txt")
       (map :outputs)
       flatten
       (map unique-digit)
       (remove first-nil?)
       count))

(defn part-2
  []
  (->> (parse-display-signals "input/day8.txt")
       (map #(signal-output signal-rules %))
       (reduce +)))
