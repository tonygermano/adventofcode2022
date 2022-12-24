(ns aoc.core-test
  (:require [clojure.test :refer :all]
            [aoc.core :refer :all]))

(deftest get-resource-test
  (testing "Get Default Resource"
    (is (= "AoC\n" (get-resource "default")))))

(deftest part1-test
  (testing "example"
    (is (= "answer" (part1 (get-resource "example"))))))

(deftest part2-test
  (testing "example"
    (is (= "answer" (part2 (get-resource "example"))))))
