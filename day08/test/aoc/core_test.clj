(ns aoc.core-test
  (:require [clojure.test :refer :all]
            [aoc.core :refer :all]))

(deftest get-resource-test
  (testing "Get Default Resource"
    (is (= "AoC\n" (get-resource "default")))))

(deftest parse-input-test
  (testing "example"
    (is (= [[3 0 3 7 3]
            [2 5 5 1 2]
            [6 5 3 3 2]
            [3 3 5 4 9]
            [3 5 3 9 0]]
           (parse-input (get-resource "example"))))))

(deftest update-visibility-map-row-test
  (testing "example"
    (are [x y] (= (update-visibility-map-row x (vec (repeat 5 0))) y)
      [3 0 3 7 3] [1 0 0 1 0]
      [2 5 5 1 2] [1 1 0 0 0] 
      [6 5 3 3 2] [1 0 0 0 0]
      [3 3 5 4 9] [1 0 1 0 1]
      [3 5 3 9 0] [1 1 0 1 0])))

(deftest update-visibility-map-test
  (testing "example"
    (is (= [[1 1 1 1 1]
            [1 1 1 0 1]
            [1 1 0 1 1]
            [1 0 1 0 1]
            [1 1 1 1 1]]
           (update-visibility-map
            [[3 0 3 7 3]
             [2 5 5 1 2]
             [6 5 3 3 2]
             [3 3 5 4 9]
             [3 5 3 9 0]])))))

(deftest part1-test
  (testing "example"
    (is (= 21 (part1 "example")))))

(deftest part2-test
  (testing "example"
    (is (= 8 (part2 "example")))))
