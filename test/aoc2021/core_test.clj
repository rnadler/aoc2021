(ns aoc2021.core-test
  (:require [clojure.test :refer :all]
            [aoc2021.core :as sut]))

(deftest test-day1-part1
  (testing "day1-part1"
    (is (= (sut/day1-part1) 1475))))

(deftest test-day1-part2
  (testing "day1-part2"
    (is (= (sut/day1-part2) 1516))))

(deftest test-day2-part1
  (testing "day2-part1"
    (is (= (sut/day2-part1) 2187380))))

(deftest test-day2-part2
  (testing "day2-part2"
    (is (= (sut/day2-part2) 2086357770))))
