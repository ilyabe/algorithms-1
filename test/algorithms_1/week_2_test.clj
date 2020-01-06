(ns algorithms-1.week-2-test
  (:require [clojure.test :refer :all]
            [algorithms-1.week-2 :as w2]))

(def bigger [54044 14108 79294 29649 25260 60660 2995 53777 49689 9083])
(def hw2 (read-string (slurp "test/resources/hw2.clj")))

(deftest sort-and-count-inv
  (testing "Sanity check: 0 inversions for a sorted array"
    (is (= 0 (second (w2/sort-and-count-inv [0 1 2 3 4 5])))))
  (testing "Sanity check: n(n-1)/2 inversions for a reverse sorted array"
    (is (= 28 (second (w2/sort-and-count-inv [8 7 6 5 4 3 2 1])))))
  (testing "Bigger numbers work too"
    (is (= 28 (second (w2/sort-and-count-inv bigger)))))
  (testing "Challenge set with 10k numbers"
    (is (= 2407905288 (second (w2/sort-and-count-inv hw2))))))
