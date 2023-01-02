(ns aoc.core-test
  (:require [clojure.test :refer :all]
            [aoc.core :refer :all]))

(deftest get-resource-test
  (testing "Get Default Resource"
    (is (= "AoC\n" (get-resource "default")))))

(deftest part1-test
  (testing "example"
    (is (= 13140 (part1 (get-resource "example"))))))

(deftest signal->pixel-test
  (testing "on"
    (are [x] (= (signal->pixel x) "#")
      [1 1] [2 1] [3 1]
      [24 24] [25 24] [26 24]
      [40 40]
      [41 1] [42 1] [43 1]))
  (testing "off"
    (are [x] (= (signal->pixel x) ".")
      [4 1]
      [23 24] [27 24]
      [39 40]
      [44 1])))

(def part2-example-rendering
  '("##..##..##..##..##..##..##..##..##..##.."
    "###...###...###...###...###...###...###."
    "####....####....####....####....####...."
    "#####.....#####.....#####.....#####....."
    "######......######......######......####"
    "#######.......#######.......#######....."))

(deftest part2-test
  (testing "example"
    (is (= part2-example-rendering (part2 (get-resource "example"))))))
