(ns advent-of-code-2021.day9
  (:require [advent-of-code-2021.core :as core]))

(defn parse-height-map
  [filename]
  (let [hm (->> (core/read-lines filename)
                (mapv #(->> % core/str-split-chars (mapv core/str->int))))]
    {:hmap hm :rows (count hm) :columns (count (get hm 0))}))

(defn height
  [height-map coord]
  (get-in (:hmap height-map) coord))

(defn adjacent-locations
  [height-map [row column]]
  (let [deltas [[1 0] [-1 0] [0 1] [0 -1]]
        locations (map (fn [[dx dy]]
                         [(+ row dx) (+ column dy)])
                       deltas)]
    (filter (fn [[y x]]
              (and (< -1 y (:rows height-map))
                   (< -1 x (:columns height-map))))
            locations)))

(defn low-point?
  [height-map coord]
  (let [v (height height-map coord)]
    (->> (adjacent-locations height-map coord)
         (map #(height height-map %))
         (every? #(< v %)))))

(defn location-seq
  [height-map]
  (for [y (range (:rows height-map)) x (range (:columns height-map))]
    [y x]))

(defn low-point-seq
  [height-map]
  (filter #(low-point? height-map %) (location-seq height-map)))

(defn risk-level
  [height-map coord]
  (+ (height height-map coord) 1))

(def ^:const peak-height 9)

(defn basin-size
  ([height-map coord]
   (basin-size height-map 0 #{} [coord]))
  ([hm size visited [coord & work-list]]
   (if coord
     (if (visited coord)
       (recur hm size visited work-list)
       (let [work-list (->> (adjacent-locations hm coord)
                            (remove #(or (visited %)
                                         (= (height hm %) peak-height)
                                         (< (height hm %) (height hm coord))))
                            (apply conj work-list))]
         (recur hm (inc size) (conj visited coord) work-list)))
     size)))

(defn solver
  [transform reducer]
  (let [hm (parse-height-map "input/day9.txt")]
    (->> (low-point-seq hm) (map #(transform hm %)) reducer)))

(defn part-1 
  []
  (solver risk-level (partial reduce +)))

(defn part-2
  []
  (solver basin-size #(->> % (sort-by -) (take 3) (reduce *))))
