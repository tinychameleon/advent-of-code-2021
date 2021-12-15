(ns advent-of-code-2021.day15
  (:require [advent-of-code-2021.core :as core]))

(def char->int (comp core/str->int str))

(defn parse-risk-level-graph
  [filename]
  (let [graph (mapv #(mapv char->int %) (core/read-lines filename))]
    {:graph graph :rows (count graph) :columns (count (get graph 0))}))

(defn vertices
  [{:keys [rows columns]}]
  (for [y (range rows) x (range columns)]
    [y x]))

(defn neighbours
  [{:keys [rows columns]} [y x]]
  (for [[dy dx] [[-1 0] [1 0] [0 -1] [0 1]]
        :let [r (+ y dy) c (+ x dx)]
        :when (and (< -1 r rows) (< -1 c columns))]
    [r c]))

(defn update-vertex
  [graph [vertex-cost vertex] state v]
  (let [recorded-cost (get-in state [:costs v] Integer/MAX_VALUE)
        cost (+ (get-in graph v) vertex-cost)]
    (if (< cost recorded-cost)
      (-> state
          (update :work disj [recorded-cost v])
          (update :work conj [cost v])
          (assoc-in [:costs v] cost)
          (assoc-in [:paths v] vertex))
      state)))

(defn update-costs
  [graph entry state vertices]
  (let [f (partial update-vertex (graph :graph) entry)]
    (reduce f state vertices)))

(defn dijkstra
  ([graph target start]
   (let [state {:costs {start 0} :paths {start nil} :work (sorted-set [0 start])}]
     (dijkstra graph target #{} state)))
  ([graph target visited state]
   (let [[cost vertex :as entry] (first (state :work))
         state (update state :work disj entry)]
     (cond
       (nil? cost) state
       (= vertex target) state
       :else (let [next-vertices (remove visited (neighbours graph vertex))
                   state (update-costs graph entry state next-vertices)]
               (recur graph target (conj visited vertex) state))))))

(defn wrap+
  [n x]
  (let [r (+ n x)]
    (if (> r 9) (- r 9) r)))

(defn wrap-fns
  [n]
  (map #(partial wrap+ %) (range n)))

(defn extend-graph
  [n graph]
  (let [fns (wrap-fns n)
        columns (map (fn [row] (flatten (map #(mapv % row) fns))) (graph :graph))
        rows (reduce #(apply conj %1 %2)
                     []
                     (map (fn [f] (map #(mapv f %) columns)) fns))]
    (-> graph
        (assoc :graph rows)
        (assoc :rows (count rows))
        (assoc :columns (count (get rows 0))))))

(defn search
  [graph-mutator]
  (let [g (graph-mutator (parse-risk-level-graph "input/day15.txt"))
        target [(dec (g :rows)) (dec (g :columns))]]
    (-> g (dijkstra target [0 0]) :costs (get target))))

(def part-1 (partial search identity))

(def part-2 (partial search (partial extend-graph 5)))
