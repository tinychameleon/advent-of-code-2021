(ns advent-of-code-2021.day14
  (:require [advent-of-code-2021.core :as core]))

(def ^:const mapping-regex #"(\w+) -> (\w+)")

(defn str->mapping
  [s]
  (let [[_ pair ins] (re-find mapping-regex s)]
    [(apply vector pair) (first ins)]))

(defn parse-polymer-formula
  [filename]
  (let [lines (core/read-lines filename)
        [template _ mappings] (partition-by #(= % "") lines)]
    {:template (first template) :mappings (into {} (map str->mapping mappings))}))

(defn polymer-state
  [polymer]
  {:parts (frequencies (map #(into [] %) (partition 2 1 polymer)))
   :counts (frequencies polymer)})

(def safe+ (fnil + 0))

(defn polymer-convert
  [mappings state pair count]
  (let [ins (mappings pair)
        [a b] pair]
    (-> state
        (update-in [:parts [a ins]] safe+ count)
        (update-in [:parts [ins b]] safe+ count)
        (update-in [:counts ins] safe+ count))))

(defn polymer-extend
  [mappings state]
  (reduce-kv (partial polymer-convert mappings) (dissoc state :parts) (:parts state)))

(defn polymer-span
  [{:keys [counts]}]
  (let [occurs (vals counts)]
    (- (apply max occurs) (apply min occurs))))

(defn solver
  [iterations]
  (let [{:keys [template mappings]} (parse-polymer-formula "input/day14.txt")
        sequencer (partial polymer-extend mappings)]
    (->> (polymer-state template)
         (iterate sequencer)
         (drop iterations)
         first
         polymer-span)))

(def part-1 (partial solver 10))

(def part-2 (partial solver 40))
