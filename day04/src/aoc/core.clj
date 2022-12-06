(ns aoc.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:gen-class))

(defn get-resource [filename] (slurp (io/file (io/resource filename)))) 

(defn line-to-pair-ranges [line]
  (let [[l r] (str/split line #",")
        [l0 l1] (str/split l #"-")
        [r0 r1] (str/split r #"-")]
    (vector (vector (Integer/parseInt l0) (Integer/parseInt l1))
            (vector (Integer/parseInt r0) (Integer/parseInt r1)))))

(defn input-to-pair-ranges [input]
  (->> (str/split-lines input)
       (map line-to-pair-ranges)))

(defn in-range? [i r0 r1]
  (and (>= i r0) (<= i r1)))

(defn pair-overlaps-fully? [[[l0 l1] [r0 r1]]]
  (or (and (in-range? l0 r0 r1)
           (in-range? l1 r0 r1))
      (and (in-range? r0 l0 l1)
           (in-range? r1 l0 l1))))

(comment
  (input-to-pair-ranges (get-resource "example"))
  
  (pair-overlaps-fully? '[[2 4] [6 8]])
  
  )

(defn part1 [input]
  (->> (get-resource input)
       input-to-pair-ranges
       (filter pair-overlaps-fully?)
       count))

(defn pair-overlaps-partially? [[[l0 l1] [r0 r1]]]
  (or (in-range? l0 r0 r1)
      (in-range? l1 r0 r1)
      (in-range? r0 l0 l1)
      (in-range? r1 l0 l1)))

(defn part2 [input]
  (->> (get-resource input)
       input-to-pair-ranges
       (filter pair-overlaps-partially?)
       count))

(defn -main
  "Solves AoC day X"
  [& args]
  (println (str "\npart1:\n" (part1 "input")))
  (println (str "\npart2:\n" (part2 "input"))))