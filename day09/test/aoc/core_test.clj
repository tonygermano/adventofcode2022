(ns aoc.core-test
  (:require [clojure.test :refer :all]
            [aoc.core :refer :all]))

(deftest get-resource-test
  (testing "Get Default Resource"
    (is (= "AoC\n" (get-resource "default")))))

(deftest next-tail-position-test
  (testing "example"
    (are [dir head tail] (= dir (next-tail-position head tail))
      [0 0] [0 0] [0 0]
      [0 0] [1 0] [0 0]
      [1 0] [2 0] [0 0]
      [4 1] [4 2] [3 0]
      )))

(deftest part1-test
  (testing "example"
    (is (= 13 (part1 (get-resource "example"))))))

(deftest part2-test
  (testing "example"
    (is (= "answer" (part2 (get-resource "example"))))))
