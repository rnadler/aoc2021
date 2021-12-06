(ns aoc2021.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-file
  "Read a resource file"
  [file]
  (slurp (io/resource file)))

(defn file-to-ints
  "Convert a file to an int array"
  [file]
  (map #(Integer/parseInt % )(str/split-lines (read-file file))))

;; Day 1

(defn diffs
  "Get vector diffs; v[i] - v[i-1]"
  [vec]
  (filter #(some? %)
          (map-indexed (fn [i, x]
                         (if (> i 0) (- x (nth vec (- i 1))) nil))
                       vec)))
(defn pos-diffs
  "Get count of positive diffs"
  [values]
  (let [pos-diffs (filter #(> % 0) (diffs values))]
    (count pos-diffs)))

(defn day1-part1
  "day1 part1"
  []
  (pos-diffs (file-to-ints "day1-input.txt")))

(defn sum3
  "3 value sum"
  [vec i]
  (let [max (- (count vec) 3) ]
    (if (> i max) nil (+ (nth vec i) (nth vec (+ i 1)) (nth vec (+ i 2))))))

(defn sums-part2
  "Get vector sums; v[i] - v[i+1] + v[i+2]"
  [vec]
  (filter #(some? %)
          (map-indexed (fn [i, _] (sum3 vec i)) vec)))

(defn day1-part2
  "day1 part2"
  []
  (pos-diffs (sums-part2 (file-to-ints "day1-input.txt"))))

;; Day 2

(defn nav-commands []
  (map (fn [[x y]] [x (Integer/parseInt y)]) (map #(str/split % #" ") (str/split-lines (read-file "day2-input.txt")))))

(defn destination [input]
  (reduce (fn [res [cmd val]]
            (case cmd
              "forward" (update res :hor (partial + val))
              "down" (update res :depth (partial + val))
              "up" (update res :depth #(- % val))))
          {:hor 0 :depth 0}
          input))

(defn day2-part1 []
  (let [dest (destination (nav-commands))]
    (* (:hor dest) (:depth dest))))
