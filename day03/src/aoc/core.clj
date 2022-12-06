(ns aoc.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set])
  (:gen-class))

(defn get-resource [filename] (slurp (io/file (io/resource filename))))


(defn input-to-rucksacks [input]
  (->> input
       str/split-lines
       (map #(split-at (/ (count %) 2) %))))

(defn rucksack-to-compartments-shared-item [rucksack]
  (first (set/intersection (set (rucksack 0)) (set (rucksack 1)))))

(def type-to-priority-map
  (zipmap (map char (concat (range 97 123) ; a-z
                            (range 65 91))) ; A-Z
          (range 1 53))) ; 1-52

(defn type-to-priority [type]
  (type-to-priority-map type))

(defn part1 [input]
  (->> (get-resource input)
       input-to-rucksacks
       (map rucksack-to-compartments-shared-item)
       (map type-to-priority)
       (apply +)))

(defn get-group-badge [group]
  (first (apply set/intersection (map set group))))

(defn part2 [input]
  (->> (get-resource input)
       str/split-lines
       (partition 3)
       (map get-group-badge)
       (map type-to-priority)
       (apply +)
       ))

(defn -main
  "Solves AoC day X"
  [& args]
  (println (str "\npart1:\n" (part1 "input")))
  (println (str "\npart2:\n" (part2 "input"))))