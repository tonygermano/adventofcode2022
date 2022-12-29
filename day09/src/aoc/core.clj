(ns aoc.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:gen-class))

(defn get-resource [filename] (slurp (io/file (io/resource filename))))

(defn parse-line [input]
  (->> input
       (re-matches #"([UDLR]) (\d+)")
       ((fn [m] {:direction (keyword (get m 1))
                 :distance (Integer/parseInt (get m 2))}))))

(defn parse-input [input]
  (->> input
       str/split-lines
       (map parse-line)))

(def directions {:U [0 1]
                 :D [0 -1]
                 :R [1 0]
                 :L [-1 0]})

(def start-state {:head [0 0]
            :tail [0 0]
            :tail-visited #{[0 0]}})

(defn next-tail-position [head tail]
  (let [delta (mapv - head tail)
        touching? (every? #(< (abs %) 2) delta)
        movement-direction (mapv #(Integer/signum %) delta)]
    (if touching?
      tail
      (mapv + tail movement-direction))))

(defn do-steps [state]
  (if (empty? (:steps state))
    (:tail-visited state)
    (let [head (map + (:head state) (first (:steps state)))
          tail (next-tail-position head (:tail state))
          tail-visited (conj (:tail-visited state) tail)
          steps (rest (:steps state))]
      (recur {:head head
              :tail tail
              :tail-visited tail-visited
              :steps steps}))))

(defn part1 [input]
  (->> input
       parse-input
       (mapcat #(repeat (:distance %) ((:direction %) directions)))
       (assoc start-state :steps)
       do-steps
       count))

(def part2-start-state {:rope (repeat 10 [0 0]) 
                        :tail-visited #{[0 0]}})

(defn move-rope [rope step]
  (loop [lead-knot (mapv + (first rope) step)
         new-rope [lead-knot]
         remaining-rope (rest rope)]
    (if (empty? remaining-rope)
      new-rope
      (let [next-knot (next-tail-position lead-knot (first remaining-rope))]
       (recur next-knot (conj new-rope next-knot) (rest remaining-rope))))))

(defn do-steps2 [state]
  (if (empty? (:steps state))
    (:tail-visited state)
    (let [new-rope (move-rope (:rope state)(first (:steps state)))]
      (recur {:steps (rest (:steps state))
              :rope new-rope
              :tail-visited (conj (:tail-visited state) (first (reverse new-rope)))}))))

(defn part2 [input]
  (->> input
       parse-input
       (mapcat #(repeat (:distance %) ((:direction %) directions)))
       (assoc part2-start-state :steps)
       do-steps2
       count))

(defn -main
  "Solves AoC day X"
  [& args]
  (println (str "\npart1:\n" (part1 (get-resource "input"))))
  (println (str "\npart2:\n" (part2 (get-resource "input")))))