(ns aoc.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:gen-class))

(defn get-resource [filename] (slurp (io/file (io/resource filename))))

(defn char->int [c] (- (int c) 48))

(defn parse-input [input]
  (->> input
      (str/split-lines)
      (mapv #(mapv char->int %))))

(defn update-visibility-map-row
  "Modifies an existing visibility map row when looking from the start of
   the sequence"
  [height-row visibility-row]
  (loop [index 0
         previous-max-height -1
         updated-visibility-row visibility-row]
    (if (= index (count height-row))
      updated-visibility-row
      (let [current-height (get height-row index)
            visible? (> current-height previous-max-height)]
        (if visible?
          (recur (inc index) current-height (assoc updated-visibility-row index 1))
          (recur (inc index) previous-max-height updated-visibility-row))))))

(defn update-visibility-map
  "Rearrange the matrices to view from one direction at a time"
  ([height-map] (let [zero-visibility-row (mapv (constantly 0) (get height-map 0))]
                  (update-visibility-map height-map (mapv (constantly zero-visibility-row) height-map))))
  ([height-map visibility-map] (let [reverse-rows (fn [m] (mapv #(vec (reverse %)) m))
                                     transpose-rows (fn [m] (apply mapv vector m))
                                     l2r-v (mapv update-visibility-map-row height-map visibility-map)
                                     r2l-h (reverse-rows height-map)
                                     r2l-v (mapv update-visibility-map-row r2l-h (reverse-rows l2r-v))
                                     t2b-h (transpose-rows r2l-h)
                                     t2b-v (mapv update-visibility-map-row t2b-h (transpose-rows r2l-v))
                                     b2t-h (reverse-rows t2b-h)
                                     b2t-v (mapv update-visibility-map-row b2t-h (reverse-rows t2b-v))]
                                 (-> b2t-v reverse-rows transpose-rows reverse-rows))))

(defn part1 [input]
  (->> input
       get-resource
       parse-input
       update-visibility-map
       flatten
       (reduce +)
       ))

(defn part2 [input]
  (->> input
       (comment
         (get-resource input)
         )
       ))

(defn -main
  "Solves AoC day X"
  [& args]
  (println (str "\npart1:\n" (part1 "input")))
  (println (str "\npart2:\n" (part2 "input"))))