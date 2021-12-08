(ns advent-of-code-2021.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-file
  "Read the filename into memory."
  [filename]
  (slurp (io/resource filename)))

(defn read-lines
  "Read all the lines in filename."
  [filename]
  (str/split-lines (read-file filename)))

(defn str-split-chars
  "Split a string into string representations of each character."
  [s]
  (str/split s #""))

(defn str-split-words
  "Split a string into words based on space characters."
  [s]
  (str/split s #"\s+"))

(defn str-split
  "Split a string with the string as the last parameter."
  [re s]
  (str/split s re))

(defn str->int
  "Convert a string to an integer."
  ([s]
   (str->int 10 s))
  ([base s]
   (Integer/parseInt s base)))

(defn read-ints
  "Read lines of integers from filename."
  [filename]
  (map str->int (read-lines filename)))

(defn floor
  "Round any non-integer down to the nearest integer."
  [n]
  (-> n Math/floor int))

(defn ceiling
  "Round any non-integer up to the nearest integer."
  [n]
  (-> n Math/ceil int))

(defn zip
  "Zip colls together."
  [& colls]
  (apply map vector colls))

(defn with-index
  "Create a new seq of [index val] pairs from coll."
  ([coll]
   (with-index coll 0))
  ([coll start]
   (zip (drop start (range)) coll)))

(defn value-counts
  [coll]
  (->> (group-by identity coll)
       (reduce (fn [m [k v]] (assoc m k (count v))) {})))

(defn- adjacent-tuples
  "Return a seq of adjacent tuples of size n from coll.
  Made private from learning about clojure.core/partition."
  ([coll]
   (adjacent-tuples 2 coll))
  ([n coll]
   (let [slice #(drop % (drop-last (- n % 1) coll))
         seq-slices (map slice (range n))]
     (apply map vector seq-slices))))
