(ns aoc.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:gen-class))

(defn get-resource [filename] (slurp (io/file (io/resource filename))))

(defn find-start-sequence
  ([[buffer input]] #(find-start-sequence 4 (reverse buffer) input))
  ([position r-buffer input]
   (if (= 4 (count (set r-buffer)))
     position
     #(find-start-sequence
       (inc position)
       (cons (first input) (take 3 r-buffer))
       (rest input)))))

(defn part1 [input]
  (->> (split-at 4 input)
       (trampoline find-start-sequence)))

(defn find-start-sequence2
  ([[buffer input]] #(find-start-sequence2 14 (reverse buffer) input))
  ([position r-buffer input]
   (if (= 14 (count (set r-buffer)))
     position
     #(find-start-sequence2
       (inc position)
       (cons (first input) (take 13 r-buffer))
       (rest input)))))

(defn part2 [input]
  (->> (split-at 14 input)
       (trampoline find-start-sequence2)))

(defn -main
  "Solves AoC day X"
  [& args]
  (println (str "\npart1:\n" (part1 (get-resource "input"))))
  (println (str "\npart2:\n" (part2 (get-resource "input")))))