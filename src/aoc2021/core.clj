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
  (map #(Integer/parseInt % ) (str/split-lines (read-file file))))

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

(defn day2-result [dest]
  (* (:hor dest) (:depth dest)))

(defn day2-part1 []
  (day2-result (destination (nav-commands))))

(defn destination2 [input]
  (reduce (fn [res [cmd val]]
            (case cmd
              "forward" (-> (update res :hor (partial + val))
                            (update :depth (partial + (* (:aim res) val))))
              "down" (update res :aim (partial + val))
              "up" (update res :aim #(- % val))))
          {:hor 0 :depth 0 :aim 0}
          input))

(defn day2-part2 []
  (day2-result (destination2 (nav-commands))))

;; Day 3

(defn diagnostics
  "day3 diagnostic data"
  []
  (map #(into [] (map (fn [v] (Integer/parseInt v))) (str/split % #"")) (str/split-lines (read-file "day3-input.txt"))))

(defn diag-data
  "Get diag data - count 0's and 1's"
  [input]
  (map #(frequencies %)
       (reduce (fn [res, v]
                 (conj res (map #(get % v) input)))
               []
               (range (count (first input))))))

(defn common-count
  [input z o]
  (into [] (map #(if (> (get % 0) (get %1 1)) z o) (diag-data input))))

(defn bin-to-int
  "Binary array (0's and 1's) to an integer value"
  [arr]
  (-> arr
      str/join
      (Integer/parseInt 2)))

(defn get-rate
  "Get gamma rate"
  [input z o]
  (bin-to-int (common-count input z o)))

(defn epsilon-rate
  [input]
  (get-rate input 1 0))

(defn gamma-rate
  [input]
  (get-rate input 0 1))

(defn day3-part1
  []
  (let [input (diagnostics)]
    (* (gamma-rate input) (epsilon-rate input))))

 (defn first-match
  [input z o]
   (let [counts (common-count input z o)]
     (first (first (filter (fn [a] (= (count a) 1))
                    (into [] (reduce (fn [res, v]
                                       (println v (get counts v) (count (last res)) (last res))
                                       (conj res (filter #(= (get % v) (get counts v)) (last res))))
                                     [input]
                                     (range (count counts)))))))))

;; (bin-to-int (first-match (diagnostics) 0 1)) ;; 3131
;;
;; (first-match (diagnostics) 1 0) ;; nil
;;6 0 7 ([0 0 1 1 1 1 *1 0 0 1 1 0] [0 0 1 1 1 1 1 *0 0 1 0 0] [0 0 1 1 1 1 *1 1 1 1 0 1] [0 0 1 1 1 1 0 0 1 0 0 1] [0 0 1 1 1 1 0 0 1 1 0 1] [0 0 1 1 1 1 1 1 0 0 0 0] [0 0 1 1 1 1 1 0 1 0 1 1])
;;7 0 2 ([0 0 1 1 1 1 0 *0 1 0 0 1] [0 0 1 1 1 1 0 *0 1 1 0 1])
;;8 0 2 ([0 0 1 1 1 1 0 0 *1 0 0 1] [0 0 1 1 1 1 0 0 *1 1 0 1])
;;9 0 0 ()


(defn og-rate
  "oxygen generator rating"
  [input]
  (bin-to-int (first-match input 0 1)))

(defn co2sc-rate
  "CO2 scrubber rating"
  [input]
  (bin-to-int (first-match input 1 0)))

(defn day3-part2
  []
  (let [input (diagnostics)]
    (* (og-rate input) (co2sc-rate input))))
