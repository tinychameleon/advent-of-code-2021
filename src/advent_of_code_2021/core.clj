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

(defn str-split
  "Split a string into string representations of each character."
  [s]
  (str/split s #""))

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

(defn- adjacent-tuples
  "Return a seq of adjacent tuples of size n from coll.
  Made private from learning about clojure.core/partition."
  ([coll]
   (adjacent-tuples 2 coll))
  ([n coll]
   (let [slice #(drop % (drop-last (- n % 1) coll))
         seq-slices (map slice (range n))]
     (apply map vector seq-slices))))
