(ns aoc.core-test
  (:require [clojure.test :refer :all]
            [aoc.core :refer :all]))

(deftest get-resource-test
  (testing "Get Default Resource"
    (is (= "AoC\n" (get-resource "default")))))

(deftest line-to-pair-ranges-test
  (testing "2-4,6-8"
    (is (= '[[2 4] [6 8]] (line-to-pair-ranges "2-4,6-8")))))

(deftest in-range?-test
  (testing "1 in [2 3]"
    (is (false? (in-range? 1 2 3))))
  (testing "2 in [2 3]"
    (is (true? (in-range? 2 2 3))))
  (testing "3 in [2 3]"
    (is (true? (in-range? 3 2 3))))
  (testing "4 in [2 3]"
    (is (false? (in-range? 4 2 3)))))

(deftest pair-overlaps-fully?-test
  (testing "2-4,6-8"
    (is (false? (pair-overlaps-fully? '[[2 4] [6 8]]))))
  (testing "2-3,4-5"
    (is (false? (pair-overlaps-fully? '[[2 3] [4 5]]))))
  (testing "5-7,7-9"
    (is (false? (pair-overlaps-fully? '[[5 7] [7 9]]))))
  (testing "2-8,3-7"
    (is (true? (pair-overlaps-fully? '[[2 8] [3 7]]))))
  (testing "6-6,4-6"
    (is (true? (pair-overlaps-fully? '[[6 6] [4 6]]))))
  (testing "2-6,4-8"
    (is (false? (pair-overlaps-fully? '[[2 6] [4 8]])))))

(deftest part1-test
  (testing "example"
    (is (= 2 (part1 "example")))))

(deftest part2-test
  (testing "example"
    (is (= 4 (part2 "example")))))
