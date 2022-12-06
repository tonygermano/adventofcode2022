(ns aoc.core-test
  (:require [clojure.test :refer :all]
            [aoc.core :refer :all]))

(deftest get-resource-test
  (testing "Get Default Resource"
    (is (= "AoC\n" (get-resource "default")))))

(deftest unrotate-stack-input-test
  (testing "example"
    (is (= '((\N \Z) (\D \C \M) (\P))
           (unrotate-stack-input
            '((\space \D \space) (\N \C \space) (\Z \M \P)))))))

(deftest input-to-stacks-test
  (testing "example"
    (is (= '[(\N \Z) (\D \C \M) (\P)]
           (input-to-stacks
            "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 ")))))

(deftest input-to-instructions-test
  (testing "example"
    (is (= '((1 2 1) (3 1 3) (2 2 1) (1 1 2))
           (input-to-instructions "move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
")))))

(deftest perform-operation-test
  (testing "example"
    (is (= '[(\D \N \Z) (\C \M) (\P)]
           (perform-operation '[(\N \Z) (\D \C \M) (\P)] '(1 2 1))))))


(deftest part1-test
  (testing "example"
    (is (= "CMZ" (part1 "example")))))

(deftest part2-test
  (testing "example"
    (is (= "answer" (part2 "example")))))
