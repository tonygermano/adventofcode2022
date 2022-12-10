(ns aoc.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.zip :as z])
  (:gen-class))

(defn get-resource [filename] (slurp (io/file (io/resource filename))))

(defn parse-command [line]
  (let [[cmd & args] (str/split line #" +")]
    {:type (condp = cmd
             "cd" :cd
             "ls" :ls)
     :args args}))

(defn parse-ls-output [line]
  (let [words (str/split line #" ")]
    (cond
      (= (first words) "dir") {:type :dir
                               :name (second words)}
      :else {:type :file
             :name (second words)
             :size (Integer/parseInt (first words))})))

(defn input->command [input]
  (let [lines (str/split-lines input)
        command (parse-command (first lines))]
    {:command command
     :output (condp = (:type command)
               :cd nil
               :ls (map parse-ls-output (rest lines)))}))

(defn input->commands [input]
  (->> (str/split input #"\$ ")
       rest ; skip empty string at beginning
       (map input->command)))

(defn make-dir [other children]
  (assoc other :children children))

(defn zip-navigate-to-subdir [pwd subdirname]
  (let [matches-dir? (fn [file] (and (= :dir (:type (z/node file)))
                                    (= subdirname (:name (z/node file)))))]
    (cond
      (= "/" subdirname) (loop [loc pwd]
                           (if (z/up loc)
                             (recur (z/up loc))
                             loc))
      (= ".." subdirname) (z/up pwd)
      :else (loop [next-file (z/down pwd)]
              (if (matches-dir? next-file)
                next-file
                (recur (z/next next-file)))))))

(defn zip-add-to-dir [pwd filelist]
  (if (empty? filelist)
    pwd
    (recur (z/append-child pwd (first filelist)) (rest filelist))))

(defn zip-fs [filesystem]
  (z/zipper #(= :dir (:type %)) :children make-dir filesystem))

(def new-fs '{:type :dir, :name "/"})

(defn commands->filesystem [commands]
  (loop [pwd (zip-fs new-fs)
         remaining-commands commands]
    (if (empty? remaining-commands)
      (z/root pwd)
      (condp = (-> remaining-commands first :command :type)
        :cd (recur (zip-navigate-to-subdir pwd (-> remaining-commands first :command :args first)) (rest remaining-commands))
        :ls (recur (zip-add-to-dir pwd (-> remaining-commands first :output)) (rest remaining-commands))))))

(defn compute-dir-sizes [filesystem] 
  (let [root (zip-fs filesystem)
        start (loop [loc root]
                (if (-> loc z/next z/end?)
                  loc
                  (recur (z/next loc))))]
    (loop [loc start]
      (if (= :dir (-> loc z/node :type))
        (let [size (apply + (map :size (z/children loc)))
              edited (z/edit loc #(assoc-in % [:size] size))]
          (if (nil? (z/prev edited))
            (z/root edited)
            (recur (z/prev edited))))
        (recur (z/prev loc))))))

(defn find-dirs-not-more-than-size [filesystem limit]
  (loop [loc (zip-fs filesystem)
         result ()]
    (if (z/end? loc)
      result
      (let [{type :type , size :size} (z/node loc)]
        (if (and (= :dir type) (<= size limit))
          (recur (z/next loc) (cons size result))
          (recur (z/next loc) result))))))

(defn part1 [input]
  (->> (get-resource input)
       input->commands
       commands->filesystem
       compute-dir-sizes
       (#(find-dirs-not-more-than-size % 100000))
       (reduce +)))

(defn part2 [input]
  (->> input
       (comment
         (get-resource input)
         )
       ))

(defn -main
  "Solves AoC day X"
  [& args]
  (println (str "\npart1:\n" (part1 "input")))
  (println (str "\npart2:\n" (part2 "input"))))