(ns aoc.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:gen-class))

(defn get-resource [filename] (slurp (io/file (io/resource filename))))

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
   (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

(defrecord ThrownItem [from-monkey to-monkey item-worry-level])

(defprotocol MonkeyActions
  (inspect-item [monkey item-worry-level] "returns new worry level")
  (test-item [monkey item-worry-level] "returns a ThrownItem")
  (tire-of-item [_ item-worry-level] "monkey gets bored and returns new worry level")
  (peek-item [monkey] "returns a ThrownItem representing the next item or nil if no items"))

(defrecord Monkey [id items operation arg2 test-divisor monkey-if-true monkey-if-false]
  MonkeyActions
  (inspect-item [monkey item-worry-level]
    (let [op (condp = (:operation monkey) "*" *, "+" +)
          arg2 (if (= "old" (:arg2 monkey)) item-worry-level (Integer/parseInt (:arg2 monkey)))]
      (op item-worry-level arg2)))
  (test-item [monkey item-worry-level]
    (let [to-monkey (if (= 0 (mod item-worry-level (:test-divisor monkey)))
                      (:monkey-if-true monkey)
                      (:monkey-if-false monkey))]
      (->ThrownItem (:id monkey) to-monkey item-worry-level)))
  (tire-of-item [_ item-worry-level]
    (int (/ item-worry-level 3)))
  (peek-item [monkey]
    (when-let [item-worry-level (peek (:items monkey))]
      (->> item-worry-level
           (inspect-item monkey)
           (tire-of-item monkey)
           (test-item monkey)))))

(defn string->Monkey [s]
  (let [lines (str/split-lines s)
        pairs (mapv #(str/split % #": ?") lines)
        id (-> pairs (get-in [0 0]) (str/split #" ") (get 1) Integer/parseInt)
        items (-> pairs (get-in [1 1]) (str/split #", ") (#(map read-string %)) queue)
        [_ op-string arg2] (-> pairs (get-in [2 1]) (#(re-matches #"new = old (.) (.*)" %))) 
        test-divisor (-> pairs (get-in [3 1]) (str/split #" ") (get 2) Integer/parseInt)
        monkey-if-true (-> pairs (get-in [4 1]) (str/split #" ") (get 3) Integer/parseInt)
        monkey-if-false (-> pairs (get-in [5 1]) (str/split #" ") (get 3) Integer/parseInt)]
    (->Monkey id items op-string arg2 test-divisor monkey-if-true monkey-if-false)))

(defn parse-input [input]
  (let [monkey-strings (str/split input #"\n\n")]
    (mapv string->Monkey monkey-strings)))

(defn create-part1-initial-state [monkeys]
  (let [ids (->> monkeys (map :id) sort)]
    {:monkeys monkeys
     :ids ids
     :inspections (vec (repeat (count ids) 0))}))

(defn process-monkey [id state]
  (let [from-monkey (get-in state [:monkeys id])
        thrown-item (peek-item from-monkey)]
    (if (nil? thrown-item)
      state
      (let [state-popped-item
            (assoc-in state [:monkeys id :items]
                      (pop (get-in state [:monkeys id :items])))
            state-received-item
            (assoc-in state-popped-item [:monkeys (:to-monkey thrown-item) :items]
                      (conj (get-in state-popped-item [:monkeys (:to-monkey thrown-item) :items])
                            (:item-worry-level thrown-item)))
            state-count-item
            (assoc-in state-received-item [:inspections id]
                      (inc (get-in state-received-item [:inspections id])))]
        (recur id state-count-item)))))

(defn process-round [state]
  (loop [remaining-monkeys (:ids state)
         state state]
    (if (empty? remaining-monkeys)
      state
      (recur (rest remaining-monkeys) (process-monkey (first remaining-monkeys) state)))))

(defn process-rounds [rounds state]
  (if (< rounds 1)
    state
    (recur (dec rounds) (process-round state))))

(defn part1 [input]
  (->> input
       parse-input
       create-part1-initial-state
       (process-rounds 20)
       :inspections
       sort
       reverse
       (take 2)
       (reduce *)))

(defn part2 [input]
  (->> input
       
       ))

(defn -main
  "Solves AoC day X"
  [& args]
  (println (str "\npart1:\n" (part1 (get-resource "input"))))
  (println (str "\npart2:\n" (part2 (get-resource "input")))))