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
  (str/split (read-file filename) #"\n"))

(defn str->int
  "Convert a string to an integer."
  [s]
  (Integer/parseInt s))

(defn adjacent-tuples
  "Return a seq of adjacent tuples of size n from coll."
  ([coll]
   (adjacent-tuples 2 coll))
  ([n coll]
   (let [slice #(drop % (drop-last (- n % 1) coll))
         seq-slices (map slice (range n))]
     (apply map vector seq-slices))))
