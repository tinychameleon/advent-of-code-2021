(ns advent-of-code-2021.day2
  (:require [advent-of-code-2021.core :as core]
            [clojure.string :as str]))

(defn part-1
  []
  (solver apply-2d-command {:horizontal 0 :depth 0}))

(defn part-2
  []
  (solver apply-3d-command {:horizontal 0 :depth 0 :aim 0}))

(defn- solver
  [f val]
  (let [commands (read-commands "input/day2.txt")
        {:keys [horizontal depth]} (reduce f val commands)]
    (* horizontal depth)))

(defn- apply-2d-command
  [state [command delta]]
  (case command
    "forward" (update state :horizontal + delta)
    "down" (update state :depth + delta)
    "up" (update state :depth - delta)))

(defn- apply-3d-command
  [state [command delta]]
  (case command
    "forward" (-> state
                  (update :horizontal + delta)
                  (update :depth + (* (:aim state) delta)))
    "down" (update state :aim + delta)
    "up" (update state :aim - delta)))

(defn- parse-line
  [line]
  (let [[command delta] (str/split line #" ")]
    [command (core/str->int delta)]))

(defn- read-commands
  [filename]
  (map parse-line (core/read-lines filename)))
