(ns advent-of-code-2021.day10
  (:require [advent-of-code-2021.core :as core]))

(def chunk-pairs {\( \), \[ \], \{ \}, \< \>})

(def closing-points {\) 3, \] 57, \} 1197, \> 25137})

(def completion-points {\) 1, \] 2, \} 3, \> 4})

(defn chunks-seq
  ([line]
   (chunks-seq line "" '()))
  ([[ch & remaining] chunk closers]
   (cond
     (nil? ch) {:chunk chunk :closers closers}
     (chunk-pairs ch) (recur remaining (str chunk ch) (conj closers (chunk-pairs ch)))
     (= ch (first closers)) (recur remaining (str chunk ch) (rest closers))
     :else {:error :corrupt :expected (first closers) :found ch})))

(defn autocomplete-score
  [closers]
  (reduce #(+ (* 5 %1) (completion-points %2)) 0 closers))

(defn autocomplete-winner
  [scores]
  (-> scores sort (nth (int (/ (count scores) 2)))))

(defn parse-navigation-subsystem-for-type
  [pred]
  (->> (core/read-lines "input/day10.txt") (map chunks-seq) (filter pred)))

(defn part-1
  []
  (->> (parse-navigation-subsystem-for-type :error)
       (map (comp closing-points :found))
       (reduce +)))

(defn part-2
  []
  (->> (parse-navigation-subsystem-for-type :chunk)
       (map (comp autocomplete-score :closers))
       autocomplete-winner))
