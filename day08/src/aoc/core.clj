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

(defn reverse-row [v] (vec (reverse v)))
(defn reverse-rows [m] (mapv reverse-row m))
(defn transpose-rows [m] (apply mapv vector m))

(defn update-visibility-map
  "Rearrange the matrices to view from one direction at a time"
  ([height-map] (let [zero-visibility-row (mapv (constantly 0) (get height-map 0))]
                  (update-visibility-map height-map (mapv (constantly zero-visibility-row) height-map))))
  ([height-map visibility-map] (let [l2r-v (mapv update-visibility-map-row height-map visibility-map)
                                     r2l-h (reverse-rows height-map)
                                     r2l-v (mapv update-visibility-map-row r2l-h (reverse-rows l2r-v))
                                     t2b-h (transpose-rows r2l-h)
                                     t2b-v (mapv update-visibility-map-row t2b-h (transpose-rows r2l-v))
                                     b2t-h (reverse-rows t2b-h)
                                     b2t-v (mapv update-visibility-map-row b2t-h (reverse-rows t2b-v))]
                                 (-> b2t-v reverse-rows transpose-rows reverse-rows))))

(def directions {:N [-1 0] :S [1 0] :E [0 1] :W [0 -1]})

(defn get-size [m] [(count m) (count (get m 0))])

(defn add-moat [m]
  "surrounds the entire map with a border of nil"
  (let [[_ cols] (get-size m)
        nil-row (vec (repeat (+ 2 cols) nil))
        middle-rows (mapv #(vec (flatten [nil % nil])) m)]
    ((comp vec #(apply concat %) conj) [] [nil-row] middle-rows [nil-row])))

(defn get-viewing-distance-in-direction
  [moated-map start-row start-col delta]
  (let [start-height (get-in moated-map [start-row start-col])]
    (loop [coordinates (mapv + delta [start-row start-col])
           distance 1]
      (let [height (get-in moated-map coordinates)]
        (cond
          (nil? height) (dec distance)
          (<= start-height height) distance
          :else (recur (mapv + delta coordinates) (inc distance)))))))

(defn get-distance-map [height-map]
  (let [m (add-moat height-map)
        size (get-size height-map)]
    (loop [row 1
           col 1
           result height-map]
      (cond
        (> row (get size 0)) result
        (> col (get size 1)) (recur (inc row) 1 result)
        :else (recur row (inc col)
                     (assoc-in
                      result [(dec row) (dec col)]
                      (update-vals
                       directions
                       (partial get-viewing-distance-in-direction m row col))))))))

(defn calculate-score
  "ex. {:N 0, :S 3, :E 0, :W 1}
   Multiply all values to find the score"
  [distances]
  (apply * (vals distances)))

(defn part1 [input]
  (->> input
       get-resource
       parse-input
       update-visibility-map
       flatten
       (reduce +)))

(defn part2 [input]
  (->> input
       get-resource
       parse-input
       get-distance-map
       flatten
       (map calculate-score)
       (#(apply max %))))

(defn -main
  "Solves AoC day X"
  [& args]
  (println (str "\npart1:\n" (part1 "input")))
  (println (str "\npart2:\n" (part2 "input"))))