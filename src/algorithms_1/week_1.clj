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

(defn halves [x y n2]
  (let [->num (fn [xs] (bigint (apply str xs)))
        [a b] (map ->num (split-at n2 (str x)))
        [c d] (map ->num (split-at n2 (str y)))]
    [a b c d]))

(defn pad [x y]
  "Pad x and y with 0s so that n is always a power of 2."
  (let [xs (str x)
        ys (str y)
        cxs (count xs)
        cys (count ys)
        mc (max cxs cys)
        mcev (if (even? mc) mc (inc mc))
        fmt (str "%0" mcev "d")
        zs (fn [num] (format fmt (BigInteger. (str num))))]
    (cond
      (= 1 cxs cys) [xs ys]
      (= 2 cxs cys) [xs ys]
      (and (= cxs cys) (even? cxs)) [xs ys]
      ;; which is bigger?
      ;; round to nearest power of 2
      ;; format with that many zeroes
      :else [(zs x) (zs y)])))

(comment
  [(pad 1 2)
   (pad 12 34)
   (pad 1 23)
   (pad 12 3)
   (pad 123 45)
   (pad 123 456)
   (pad 123 4567)
   (pad 1234 567)])

(defn karatsuba [x y]
  (if (or (< x 10) (< y 10))
    (* x y)
    (let [[px py] (pad x y)
          n (count px)
          n2 (/ n 2)
          [a b c d] (halves px py n2)
          p (+ a b)
          q (+ c d)
          ac (karatsuba a c)
          bd (karatsuba b d)
          pq (karatsuba p q)
          adbc (- pq ac bd)]
      (+ (* ac (bigint (Math/pow 10 n)))
         (* adbc (bigint (Math/pow 10 n2)))
         bd))))

(comment
  (karatsuba 46 134)
  (karatsuba 51146 51134) ;; 6164
  (* 51146 51134)
  (karatsuba 10 50) *e
  (karatsuba 34 78) ;; => 2652
  (karatsuba 56 78) ;; => 4368
  (karatsuba 5 6)
  (karatsuba 1112 2224) ;; 2473088
  (karatsuba 1234 5678) ;; 7006652
  (karatsuba 2500 4500)
  (= 8539734222673567065463550869546574495034888535765114961879601127067743044893204848617875072216249073013374895871952806582723184
     (karatsuba 3141592653589793238462643383279502884197169399375105820974944592
                2718281828459045235360287471352662497757247093699959574966967627))
  (= 8539734222673567065463550869546574495034888535765114961879601127067743044893204848617875072216249073013374895871952806582723184
     (* 3141592653589793238462643383279502884197169399375105820974944592N
        2718281828459045235360287471352662497757247093699959574966967627N))
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

(defn lmax 
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
  (lmax (second (winner coll))))

(comment
  (winner [12 24 6 4 56 32 98 100])
  (second-place [12 24 6 4 56 32 98 100])) ;; => 98
