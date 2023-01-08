(ns aoc.core-test
  (:require [clojure.test :refer :all]
            [aoc.core :refer :all]))

(deftest get-resource-test
  (testing "Get Default Resource"
    (is (= "AoC\n" (get-resource "default")))))

(deftest string->Monkey-test
  (let [monkeys (->> (get-resource "example")
                     (parse-input ->Monkey))]
    (testing "example0"
      (is (= (get monkeys 0) (->Monkey 0 (queue '(79 98)) "*" "19" 23 2 3))))
    (testing "example1"
      (is (= (get monkeys 1) (->Monkey 1 (queue '(54 65 75 74)) "+" "6" 19 2 0))))
    (testing "example2"
      (is (= (get monkeys 2) (->Monkey 2 (queue '(79 60 97)) "*" "old" 13 1 3))))
    (testing "example3"
      (is (= (get monkeys 3) (->Monkey 3 (queue '(74)) "+" "3" 17 0 1))))))

(deftest part1-test
  (testing "example"
    (is (= 10605 (part1 (get-resource "example"))))))

(deftest part2-test
  (testing "example"
    (is (= 2713310158 (part2 (get-resource "example"))))))
