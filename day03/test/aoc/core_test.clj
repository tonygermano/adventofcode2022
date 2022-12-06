(ns aoc.core-test
  (:require [clojure.test :refer :all]
            [aoc.core :refer :all]))

(deftest get-resource-test
  (testing "Get Default Resource"
    (is (= "AoC\n" (get-resource "default")))))

(deftest input-to-rucksacks-test
  (testing "abcd"
    (is (= '([(\a \b) (\c \d)]) (input-to-rucksacks "abcd")))))

(deftest rucksack-to-compartments-shared-item-test
  (testing "[aBc baT]"
    (is (= \a (rucksack-to-compartments-shared-item '[(\a \B \c) (\b \a \T)])))))

(deftest type-to-priority-test
  (testing "a"
    (is (= 1 (type-to-priority \a))))
  (testing "z"
    (is (= 26 (type-to-priority \z))))
  (testing "A"
    (is (= 27 (type-to-priority \A))))
  (testing "Z"
    (is (= 52 (type-to-priority \Z)))))

(deftest part1-test
  (testing "example"
    (is (= 157 (part1 "example")))))

(deftest part2-test
  (testing "example"
    (is (= 70 (part2 "example")))))
