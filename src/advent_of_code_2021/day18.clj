(ns advent-of-code-2021.day18
  (:require [advent-of-code-2021.core :as core]))

(def ^:const value-regex #"^(\d+)(.+)$")
(def ^:const explosive-depth 4)
(def ^:const split-value 10)

(defn cons-node
  [left right]
  {:kind :node :left left :right right})

(defn cons-value
  [v]
  {:kind :value :value v})

(defn parse-value
  [s]
  (let [[_ n s] (re-find value-regex s)]
    [(cons-value (core/str->int n)) s]))

(defn parse-tree
  [s]
  (case (first s)
    \[ (let [[l s] (parse-tree (subs s 1))
             [r s] (parse-tree s)]
         [(cons-node l r) s])
    \] (recur (subs s 1))
    \, (recur (subs s 1))
    (parse-value s)))

(def cons-tree (comp first parse-tree))

(defn parse-trees
  [filename]
  (->> filename core/read-lines (map cons-tree)))

(defn explode-find
  ([node]
   (explode-find node []))
  ([node path]
   (cond
     (nil? node) nil
     (= (:kind node) :value) nil
     (= (count path) explosive-depth) [node path]
     :else (or (explode-find (:left node) (conj path :left))
               (explode-find (:right node) (conj path :right))))))

(defn split-find
  ([node]
   (split-find node []))
  ([node path]
   (cond
     (nil? node) nil
     (>= (get node :value -1) split-value) [node path]
     :else (or (split-find (:left node) (conj path :left))
               (split-find (:right node) (conj path :right))))))

(def value-node? (comp (partial = :value) :kind))

(def opposite-dir {:left :right :right :left})

(defn path-to-value
  [m path dir]
  (if (value-node? (get-in m path))
    (conj path :value)
    (recur m (conj path dir) dir)))

(defn get-value
  [node dir]
  (get-in node [dir :value]))

(defn update-parent
  [parent node dir]
  (let [opp (opposite-dir dir)
        opp-node (parent opp)
        value (get-value node opp)
        sum (if (value-node? opp-node)
              (cons-value (+ (get-value parent opp) value))
              (update-in opp-node (path-to-value opp-node [dir] dir) + value))]
    {dir (cons-value 0) opp sum}))

(defn update-direction
  [tree node path side]
  (let [debris-path (->> path reverse (drop-while #(= % side)) reverse (into []))]
    (if (empty? debris-path)
      tree
      (let [path (conj (pop debris-path) side)]
        (update-in tree
                   (if (value-node? (get-in tree path))
                     (conj path :value)
                     (path-to-value tree path (opposite-dir side)))
                   +
                   (get-value node side))))))

(defn explode-node
  [tree [node path]]
  (let [side (peek path)]
    (-> tree
        (update-in (pop path) update-parent node side)
        (update-direction node path side))))

(defn split-node
  [tree [node path]]
  (let [l (cons-value (core/floor (/ (:value node) 2)))
        r (cons-value (core/ceiling (/ (:value node) 2)))]
    (assoc-in tree path (cons-node l r))))

(defn tree-reduce
  [tree]
  (if-let [node-path (explode-find tree)]
    (recur (explode-node tree node-path))
    (if-let [node-path (split-find tree)]
      (recur (split-node tree node-path))
      tree)))

(defn tree-add
  [t1 t2]
  (tree-reduce (cons-node t1 t2)))

(def tree-sum (partial reduce tree-add))

(defn tree-magnitude
  [tree]
  (if (value-node? tree)
    (:value tree)
    (+ (* (tree-magnitude (:left tree)) 3)
       (* (tree-magnitude (:right tree)) 2))))

(def tree-sum-magnitude (comp tree-magnitude tree-sum))

(defn permutations
  [values]
  (for [a values b values :when (not= a b)]
    [a b]))

(defn solver
  [f]
  (->> "input/day18.txt" parse-trees f))

(def part-1 (partial solver tree-sum-magnitude))

(def part-2 (partial solver #(->> % permutations (map tree-sum-magnitude) (apply max))))
