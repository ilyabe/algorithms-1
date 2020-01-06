(ns algorithms-1.week-2-test
  (:require [clojure.test :refer :all]
            [algorithms-1.week-2 :as w2]))

(deftest sort-and-count-inv
  (testing "Sanity check: 0 inversions for a sorted array"
    (is (= 0 (second (w2/sort-and-count-inv [0 1 2 3 4 5])))))
  (testing "Sanity check: n(n-1)/2 inversions for a reverse sorted array"
    (is (= 28 (second (w2/sort-and-count-inv [8 7 6 5 4 3 2 1]))))))