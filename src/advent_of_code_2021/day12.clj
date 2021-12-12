(ns advent-of-code-2021.day12
  (:require [advent-of-code-2021.core :as core]
            [clojure.string :as str]))

(defn bidirectional-update
  [m [a b]]
  (update (update m a conj b) b conj a))

(defn prune-start
  [nodes]
  (remove #(= % "start") nodes))

(defn construct-graph
  [filename]
  (->> (core/read-lines filename)
       (map #(str/split % #"-"))
       (reduce bidirectional-update {})
       (reduce-kv #(assoc %1 %2 (prune-start %3)) {})))

(defn small?
  [node]
  (= (str/lower-case node) node))

(defn in-visited?
  [node visited]
  (visited node))

(defn visit-when-small
  [node visited]
  (if (small? node)
    (assoc visited node true)
    visited))

(defn in-visited-or-twice?
  [node visited]
  (cond
    (not (small? node)) false
    (nil? (visited node)) false
    :else (some #(= % 2) (vals visited))))

(def inc0 (fnil inc 0))

(defn visit-with-count
  [node visited]
  (if (small? node)
    (update visited node inc0)
    visited))

(defn find-paths
  ([pred updater graph]
   (find-paths pred updater graph (list "start") {} []))
  ([pred updater graph [node & _ :as path] visited paths]
   (cond
     (nil? node) paths
     (= node "end") (conj paths path)
     (pred node visited) paths
     :else (let [edges (graph node)
                 new-visited (updater node visited)]
             (reduce (fn [paths next-node]
                       (find-paths pred updater graph (conj path next-node)
                                   new-visited paths))
                     paths edges)))))

(defn solver
  [pred updater]
  (->> (construct-graph "input/day12.txt")
       (find-paths pred updater)
       count))

(def part-1 (partial solver in-visited? visit-when-small))

(def part-2 (partial solver in-visited-or-twice? visit-with-count))
