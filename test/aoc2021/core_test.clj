(ns aoc2021.core-test
  (:require [clojure.test :refer :all]
            [aoc2021.core :as sut]))

(deftest test-day1-part1
  (testing "day1-part1"
    (is (= (sut/day1-part1) 1475))))

(deftest test-day1-part2
  (testing "day1-part2"
    (is (= (sut/day1-part2) 1516))))

