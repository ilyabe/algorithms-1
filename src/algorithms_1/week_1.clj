(ns algorithms-1.week-1)

;; MergeSort

(defn merge [a b]
  (loop [[x & xs :as as] a
         [y & ys :as bs] b
         sorted []]
    (cond
      (nil? x) (concat sorted bs)
      (nil? y) (concat sorted as)
      (< x y) (recur xs bs (conj sorted x))
      (< y x) (recur as ys (conj sorted y)))))

(defn merge-sort [coll]
  (let [cnt (count coll)
        mid (/ cnt 2)]
    (cond
      (zero? cnt) coll
      (= 1 cnt) coll
      :else (let [c (merge-sort (take mid coll))
                  d (merge-sort (drop mid coll))]
              (merge c d)))))

;; Karatsuba Multiplication

(defn halves [x y n]
  (let [n2 (/ n 2)]
    (->> [(quot x (Math/pow 10 n2))
          (rem x (Math/pow 10 n2))
          (quot y (Math/pow 10 n2))
          (rem y (Math/pow 10 n2))]
         (map bigint))))

(defn compute-n [x y]
  (let [cx (count (str x))
        cy (count (str y))]
    (if (and (even? cx) (even? cy))
      cx
      (inc (max cx cy)))))

(comment
  (let [x 1234 y 5678 n (compute-n x y)] (halves x y n))
  (let [x 46 y 134 n (compute-n x y)] (halves x y n)))

(defn karatsuba [x y]
  (if (or (< x 10) (< y 10))
    (* x y)
    (let [n (compute-n x y)
          [a b c d] (halves x y n)
          p (+ a b)
          q (+ c d)
          ac (karatsuba a c)
          bd (karatsuba b d)
          pq (karatsuba p q)
          adbc (- pq ac bd)]
      (+ (* ac (bigint (Math/pow 10 n)))
         (* adbc (bigint (Math/pow 10 (/ n 2))))
         bd))))

(comment
  (karatsuba 46 134) ;; 6164
  (karatsuba 10 50)
  (karatsuba 5 6)
  (karatsuba 1112 2224) ;; 2473088
  (karatsuba 1234N 5678N) ;; 7006652
  (karatsuba 8385623558792674733786478294439724659735456427989434445674878447N
             2826463673794433976332997844395283945995575924857989533744856647N)
  (karatsuba 1234 5678)
  (take 2 [1 2 3 4 5])
  (drop 2 [1 2 3 4 5])
  (merge-sort [8 7 6 3])
  (merge-sort [8 7 6 3 4])
  (merge-sort (take 10000 (map rand-int (repeat 10000)))))

;; Second Place

;; Divide
;; Base: coll size 2
;;;; return: [max, [others]]

;; Conquer
;; if (first a) > (first b)
;;;; [a (concat (second a) [(first b)] (second b))]
;;;; [b (concat (second b) [(first a)] (second a))]

;; Divide
;; [12 24 6 4 56 32 98 100]
;; [12 24 6 4]  [56 32 98 100]
;; [12 24] [6 4]  [56 32] [98 100]
;; Conquer
;; [24 [12]] [6 [4]]  [56 [32]] [100 [98]]
;; [24 [12 6]] [100 [98 56]]
;; [100 [98 56 24]]
;; This results in ≤ n + log2n - 2 (8 + 3 - 2 = 9)
;; e.g. 9 comparisions ≤ n + log2n - 2 (8 + 3 - 2 = 9)
;;;; 9 ≤ 8 + 3 - 2
;;;; 9 ≤ 9

(defn max 
  "A linear max without using Clojure's built-in max"
  [coll]
  (loop [x 0
         [y & ys] coll]
    (cond
      (nil? y) x
      (> x y) (recur x ys)
      (< x y) (recur y ys))))

(defn knockout [[wina losea] [winb loseb]]
  (if (> wina winb)
    [wina (concat losea [winb])]
    [winb (concat loseb [wina])]))

(defn winner [[a b :as coll]]
  (let [cnt (count coll)
        mid (/ cnt 2)]
    (cond
      (= 2 cnt) (if (> a b)
                  [a [b]]
                  [b [a]])
      :else (let [c (winner (take mid coll))
                  d (winner (drop mid coll))]
              (knockout c d)))))

(defn second-place [coll]
  (max (second (winner coll))))

(comment
  (winner [12 24 6 4 56 32 98 100])
  (second-place [12 24 6 4 56 32 98 100])) ;; => 98
