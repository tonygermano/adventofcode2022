(ns aoc.core-test
  (:require [clojure.test :refer :all]
            [aoc.core :refer :all]))

(deftest get-resource-test
  (testing "Get Default Resource"
    (is (= "AoC\n" (get-resource "default")))))

(deftest part1-test
  (testing "example 1"
    (is (= 7 (part1 "mjqjpqmgbljsphdztnvjfqwrcgsmlb"))))
  (testing "example 2"
    (is (= 5 (part1 "bvwbjplbgvbhsrlpgdmjqwftvncz"))))
  (testing "example 3"
    (is (= 6 (part1 "nppdvjthqldpwncqszvftbrmjlhg"))))
  (testing "example 4"
    (is (= 10 (part1 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"))))
  (testing "example 5"
    (is (= 11 (part1 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")))))

(deftest part2-test
  (testing "example 1"
    (is (= 19 (part2 "mjqjpqmgbljsphdztnvjfqwrcgsmlb"))))
  (testing "example 2"
    (is (= 23 (part2 "bvwbjplbgvbhsrlpgdmjqwftvncz"))))
  (testing "example 3"
    (is (= 23 (part2 "nppdvjthqldpwncqszvftbrmjlhg"))))
  (testing "example 4"
    (is (= 29 (part2 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"))))
  (testing "example 5"
    (is (= 26 (part2 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")))))
