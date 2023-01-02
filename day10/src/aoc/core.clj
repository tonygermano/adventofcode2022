(ns aoc.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:gen-class))

(defn get-resource [filename] (slurp (io/file (io/resource filename))))

(defrecord Result [completed? cycles-consumed updated-op register])

(defprotocol Op
  "Operation" 
  (complete [this register])
  (remaining-cycles [this])
  (consume-cycles [this cycles]))

(defn get-result [op cycles register]
  (let [completed? (>= cycles (remaining-cycles op))
        cycles-to-consume (min cycles (remaining-cycles op))
        updated-op (consume-cycles op cycles-to-consume)
        updated-register (if completed?
                   (complete op register)
                   register)]
    (->Result completed? cycles-to-consume updated-op updated-register)))

(def DefaultOp
  {:remaining-cycles (fn [this] (:remaining-cycles this))
   :consume-cycles (fn [this cycles] 
                     (assoc this :remaining-cycles (- (remaining-cycles this) cycles)))})

(defrecord Addx [x remaining-cycles])
(extend Addx
  Op
  (assoc DefaultOp
         :complete (fn [this register] (+ (:x this) register))))

(defrecord Noop [remaining-cycles])
(extend Noop
  Op
  (assoc DefaultOp
         :complete (fn [_ register] register)))

(defn line->Op [line]
  (let [args (str/split line #" ")]
    (condp = (keyword (first args))
      :addx (->Addx (Integer/parseInt (get args 1)) 2)
      :noop (->Noop 1))))

(defn parse-input [input]
  (->> input
       str/split-lines
       (map line->Op)))

(def part1-start-state {:interesting-cycle-durations (cons 20 (repeat 5 40))
                        :register 1
                        :current-cycle 0
                        :interesting-values '()})

(defn execute [state]
  (if (empty? (:interesting-cycle-durations state))
    (:interesting-values state)
    (let [remaining-cycles (first (:interesting-cycle-durations state))
          result (get-result (first (:instructions state))
                             remaining-cycles
                             (:register state))
          updated-instructions (if (:completed? result)
                                 (rest (:instructions state))
                                 (cons (:updated-op result) (rest (:instructions state))))
          interesting? (= remaining-cycles (:cycles-consumed result))
          updated-current-cycle (+ (:current-cycle state) (:cycles-consumed result))
          updated-durations (if interesting?
                              (rest (:interesting-cycle-durations state))
                              (cons (- remaining-cycles (:cycles-consumed result))
                                    (rest (:interesting-cycle-durations state))))]
      (recur {:interesting-cycle-durations updated-durations
              :register (:register result)
              :current-cycle updated-current-cycle
              :interesting-values (if interesting?
                                    (cons [updated-current-cycle (:register state)] (:interesting-values state))
                                    (:interesting-values state))
              :instructions updated-instructions}))))

(defn part1 [input]
  (->> input
       parse-input
       (assoc part1-start-state :instructions)
       execute
       (map #(reduce * %))
       (reduce +)))

(def part2-start-state {:interesting-cycle-durations (repeat (* 40 6) 1)
                        :register 1
                        :current-cycle 0
                        :interesting-values '()})

(defn signal->pixel [[cycle register]]
  (let [pixel-on "#"
        pixel-off "."
        sprite-start register
        sprite-end (+ 2 register)
        row-position (-> cycle dec (mod 40) inc)]
    (if (and (<= sprite-start row-position)
             (<= row-position sprite-end))
      pixel-on
      pixel-off)))

(defn part2 [input]
  (->> input
       parse-input
       (assoc part2-start-state :instructions)
       execute
       reverse
       (map signal->pixel)
       (partition 40)
       (map #(str/join "" %))))

(defn -main
  "Solves AoC day X"
  [& args]
  (println (str "\npart1:\n" (part1 (get-resource "input"))))
  (println (str "\npart2:\n" (str/join "\n" (part2 (get-resource "input"))))))