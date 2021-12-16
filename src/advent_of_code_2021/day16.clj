(ns advent-of-code-2021.day16
  (:require [advent-of-code-2021.core :as core]
            [clojure.string :as str]))

(defn hex->bits
  [s]
  (let [bitstr (->> s str (core/str->int 16) Integer/toBinaryString (format "%4s"))]
    (str/replace bitstr " " "0")))

(defn packet-data->binary
  [filename]
  (->> filename core/read-file str/trim (map hex->bits) str/join))

(defn bits
  [n s]
  (let [n (min (count s) n)]
    [(subs s 0 n) (subs s n)]))

(defn int-bits
  [n s]
  (let [[b s] (bits n s)]
    [(Long/parseLong b 2) s]))

(defn literal-value
  [s]
  (loop [val "" s s]
    (let [[cont s] (int-bits 1 s)
          [b s] (bits 4 s)
          v (str val b)]
      (if (= cont 1)
        (recur v s)
        [{:value (Long/parseLong v 2)} s]))))

(defn packet-length-operator
  [s]
  (loop [packets [] [n s] (int-bits 11 s)]
    (if (= n 0)
      [packets s]
      (let [[p s] (parse-packet s)]
        (recur (conj packets p) [(dec n) s])))))

(defn bit-length-operator
  [s]
  (let [[op-bits s] (apply bits (int-bits 15 s))]
    (loop [packets [] b op-bits]
      (if (empty? b)
        [packets s]
        (let [[p b] (parse-packet b)]
          (recur (conj packets p) b))))))

(def operation-fns {0 +
                    1 *
                    2 min
                    3 max
                    5 #(if (> %1 %2) 1 0)
                    6 #(if (< %1 %2) 1 0)
                    7 #(if (= %1 %2) 1 0)})

(def operator-fns {0 bit-length-operator 1 packet-length-operator})

(defn operator
  [type-id s]
  (let [[length-type-id s] (int-bits 1 s)
        [value s] ((operator-fns length-type-id) s)]
    [{:op (operation-fns type-id) :children value} s]))

(defn parse-packet
  [s]
  (let [[version s] (int-bits 3 s)
        [type-id s] (int-bits 3 s)
        [value s] (if (= 4 type-id) (literal-value s) (operator type-id s))]
    [(conj {:version version :type-id type-id} value) s]))

(defn tree-apply
  [leaf-value reducer root]
  (if (:value root)
    (leaf-value root)
    (reducer root (map #(tree-apply leaf-value reducer %) (get root :children)))))

(def version-sum (partial tree-apply :version #(reduce + (:version %1) %2)))

(def evaluate (partial tree-apply :value #(apply (:op %1) %2)))

(defn solver
  [f]
  (->> "input/day16.txt" packet-data->binary parse-packet first f))

(def part-1 (partial solver version-sum))

(def part-2 (partial solver evaluate))
