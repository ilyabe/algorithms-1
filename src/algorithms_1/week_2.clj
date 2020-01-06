(ns algorithms-1.week-2)

;;
;; Count inversions
;;

;; Bring in MergeSort and adapt to track counts, not just sort
(defn merge-and-count-split-inv [a b]
  ;(prn "a" a "b" b)
  (loop [[x & xs :as as] a
         [y & ys :as bs] b
         sorted [[] 0]]
    ;(prn "sorted" sorted "x" x "y" y)
    (cond
      (nil? x) (update sorted 0 concat bs)
      (nil? y) (update sorted 0 concat as)
      (< x y) (recur xs bs (-> sorted
                               (update 0 conj x)))
      (< y x) (recur as ys (-> sorted
                               (update 0 conj y)
                               (update 1 #(+ % (count as))))))))

(defn sort-and-count-inv [coll]
  (let [cnt (count coll)
        mid (/ cnt 2)]
    (cond
      (zero? cnt) [coll 0]
      (= 1 cnt) [coll 0]
      :else (let [[c left-inv] (sort-and-count-inv (take mid coll))
                  [d right-inv] (sort-and-count-inv (drop mid coll))
                  [b split-inv] (merge-and-count-split-inv c d)]
              ;(prn "left-inv" left-inv "right-inv" right-inv "split-inv" split-inv)
              [b (+ left-inv right-inv split-inv)]))))

(comment
  (sort-and-count-inv [8 7 6 5])
  (sort-and-count-inv [8 7 6 5 4 3 2 1])
  (sort-and-count-inv (read-string (slurp "test/resources/inversions_data_set.clj")))
  (last (sort-and-count-inv (read-string (slurp "test/resources/hw2.clj")))))
