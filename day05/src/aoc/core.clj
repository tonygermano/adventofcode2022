(ns aoc.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  
  (:gen-class))

(defn get-resource [filename] (slurp (io/file (io/resource filename))))

(defn unrotate-stack-input [rotated-stacks]
  (let  [interleaved (apply interleave rotated-stacks)
         stacks (partition (count rotated-stacks) interleaved)]
    (map #(drop-while (partial = \space) %) stacks)))

(defn input-to-stacks [input]
  (->> (str/split-lines input)
       pop ;ignore last line of stack numbers
       (map #(partition 1 4 '() (rest %)))
       (map flatten)
       unrotate-stack-input
       vec))

(defn input-to-instructions [input]
  (->> (str/split-lines input)
       (map #(->> (re-matches #"move (\d+) from (\d+) to (\d+)" %)
                  rest
                  (map (fn [i] (Integer/parseInt i)))))))

(defn input-to-stacks-and-instructions [[stacks-input instructions-input]]
  (vector (input-to-stacks stacks-input)
          (input-to-instructions instructions-input)))

(defn perform-operation [stacks [qty from to]]
  (-> (update-in stacks [(dec to)] #(concat (reverse (take qty (stacks (dec from)))) %))
      (#(update-in % [(dec from)] (fn [s] (drop qty s))))))

(defn follow-instructions [[stacks instructions]]
  (if (empty? instructions)
    stacks
    #(follow-instructions
      (vector (perform-operation stacks (first instructions))
              (rest instructions)))))

(defn part1 [input]
  (->> (get-resource input)
       (#(str/split % #"\n\n"))
       input-to-stacks-and-instructions
       (trampoline follow-instructions)
       (map first)
       (apply str)))

(comment
  (part1 "example")
  )

(defn part2 [input]
  (->> input
       (comment
         (get-resource input))))

(defn -main
  "Solves AoC day X"
  [& args]
  (println (str "\npart1:\n" (part1 "input")))
  (println (str "\npart2:\n" (part2 "input"))))