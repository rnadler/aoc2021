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
  (into [] (map #(let [z-val (get % 0)
                       o-val (get % 1)
                       z-v (if z-val z-val 0)
                       o-v (if o-val o-val 0)]
                   (if (> z-v o-v) z o))
                (diag-data input))))

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
  "Reduce values based on the bit criteria"
   [input z o]
   (first (first (filter (fn [a] (= (count a) 1))
                         (into [] (reduce (fn [res, v]
                                            (let [data (last res)
                                                  counts (common-count data z o)
                                                  step (filter #(= (get % v) (get counts v)) data)]
                                              (conj res step)))
                                          [input]
                                          (range (count (first input)))))))))

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

;; Day 4

(defn bingo-data
  "day4 bingo data"
  []
  (-> (read-file "day4-input.txt")
      str/split-lines))

(defn str-to-arr
  "Convert a string of numbers into an array of integers.
  Use the regex as the delimiter."
  [regex data]
  (->> (str/split (str/trim data) regex)
       (map #(Integer/parseInt %))
       (into [])))

(defn boards
  "Bingo boards"
  [data]
  (->> data
       rest ;; ignore the draw line
       (partition 6)
       (map #(rest %)) ;; remove the blank line. 6 --> 5 per board
       ;; Board map: :m - multiplier (0 or 1) :v value
       (map #(into [] (map (fn [v] (into [] (map (fn [val] {:m 1 :v val}) (str-to-arr #"\s+" v))) ) %)))
       (into [])))

(defn draws
  "Number draws"
  [data]
  (str-to-arr #"," (first data)))

;; (count (draws (bingo-data))) ;; 100
;; (boards (bingo-data))

(defn update-boards
  "Mark boards with given number"
  [boards num]

  )

(defn winner-board
  "Find a winning board"
  [boards]
  )
