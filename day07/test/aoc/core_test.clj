(ns aoc.core-test
  (:require [clojure.test :refer :all]
            [aoc.core :refer :all]
            [clojure.zip :as z]))

(deftest get-resource-test
  (testing "Get Default Resource"
    (is (= "AoC\n" (get-resource "default")))))

(deftest input->command-test
  (testing "cd command"
    (let [{command :command
           output :output} (input->command "cd /")]
      (is (= :cd (:type command)))
      (is (= "/" (first (:args command))))
      (is (empty? output))))
  (testing "ls command"
    (let [{command :command
           files :output} (input->command "ls
dir a
14848514 b.txt
8504156 c.dat
dir d")]
      (are [x y] (= x y)
        :ls (:type command)
        :dir (:type (first files))
        "a" (:name (first files))
        :file (:type (second files))
        "b.txt" (:name (second files))
        14848514 (:size (second files))
        4 (count files)))))

(deftest parse-command-test
  (testing "cd command"
    (let [command (parse-command "cd /")]
      (is (= :cd (:type command)))
      (is (= "/" (first (:args command))))))
  (testing "ls command"
    (let [command (parse-command "ls")]
      (is (= :ls (:type command))))))

(deftest parse-ls-output-test
  (testing "ls command"
    (testing "dir"
      (let [file (parse-ls-output "dir a")]
        (is (= :dir (:type file)))
        (is (= "a" (:name file)))))
    (testing "file"
      (let [file (parse-ls-output "14848514 b.txt")]
        (is (= :file (:type file)))
        (is (= "b.txt" (:name file)))
        (is (= 14848514 (:size file)))))))

(deftest input->commands-test
  (testing "example"
    (is (= '({:command {:type :cd, :args ("/")}, :output nil}
             {:command {:type :ls, :args nil},
              :output
              ({:type :dir, :name "a"}
               {:type :file, :name "b.txt", :size 14848514}
               {:type :file, :name "c.dat", :size 8504156}
               {:type :dir, :name "d"})}
             {:command {:type :cd, :args ("a")}, :output nil}
             {:command {:type :ls, :args nil},
              :output
              ({:type :dir, :name "e"}
               {:type :file, :name "f", :size 29116}
               {:type :file, :name "g", :size 2557}
               {:type :file, :name "h.lst", :size 62596})}
             {:command {:type :cd, :args ("e")}, :output nil}
             {:command {:type :ls, :args nil}, :output ({:type :file, :name "i", :size 584})}
             {:command {:type :cd, :args ("..")}, :output nil}
             {:command {:type :cd, :args ("..")}, :output nil}
             {:command {:type :cd, :args ("d")}, :output nil}
             {:command {:type :ls, :args nil},
              :output
              ({:type :file, :name "j", :size 4060174}
               {:type :file, :name "d.log", :size 8033020}
               {:type :file, :name "d.ext", :size 5626152}
               {:type :file, :name "k", :size 7214296})})
(input->commands (get-resource "example"))))))

(deftest make-dir-test
  (let [file1 {:type :file
               :name "a.txt"
               :size 1234}
        file2 {:type :file
               :name "b.txt"
               :size 2345}
        file3 {:type :file
               :name "c.txt"
               :size 54321}
        node {:type :dir
              :name "a"
              :children (list file1 file2)}]
    (is (= {:type :dir
            :name "a"
            :children (list file1 file3)}
           (make-dir node (list file1 file3))))
    (is (= "c.txt" (-> (make-dir node (list file1 file3)) :children second :name)))))

(comment
  "- / (dir)
  - a (dir)
    - e (dir)
      - i (file, size=584)
    - f (file, size=29116)
    - g (file, size=2557)
    - h.lst (file, size=62596)
  - b.txt (file, size=14848514)
  - c.dat (file, size=8504156)
  - d (dir)
    - j (file, size=4060174)
    - d.log (file, size=8033020)
    - d.ext (file, size=5626152)
    - k (file, size=7214296)"
  )
(deftest commands->filesystem-test
  (testing "example"
    (let [filesystem (-> (get-resource "example") input->commands commands->filesystem)
          zp (zip-fs filesystem)]
      (is (= "/" (-> zp z/node :name)))
      (is (= "d" (-> zp z/down z/right z/right z/right z/node :name)))
      (is (= 29116 (-> zp z/down z/down z/right z/node :size))))))

(deftest compute-dir-sizes-test
  (testing "example"
    (let [filesystem (-> (get-resource "example") input->commands commands->filesystem)]
      (is (= 48381165 (-> filesystem compute-dir-sizes :size))))))

(deftest part1-test
  (testing "example"
    (is (= 95437 (part1 "example")))))

(deftest part2-test
  (testing "example"
    (is (= "answer" (part2 "example")))))
