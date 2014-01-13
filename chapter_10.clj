(use 'clojure.algo.monads)

;; dataflow style
(->
  (+ 1 2)
  (* 3)
  (+ 4))


;; let style
(let
  [step1-value (+ 1 2)
  step2-value (* step1-value 3)
  step3-value (+ step2-value 4)]
  step3-value)

(defn tolerant-inc-0
  [n]
    (if (nil? n)
      (inc 0)
      (inc n)))

(tolerant-inc-0
  (tolerant-inc-0
    (tolerant-inc-0 nil)))

(->
  (tolerant-inc-0 nil)
  (tolerant-inc-0)
  (tolerant-inc-0))

(defn tolerant-inc-1
  [n]
    (inc (if (nil? n) 0 n)))

(->
  (tolerant-inc-1 nil)
  (tolerant-inc-1)
  (tolerant-inc-1))

(def nil-patch
  (fn
    [function replacement]
      (fn
        [original]
          (function
            (if (nil? original)
              replacement
              original)))))

(defn nil-factory
  [& args]
    nil)

(->
  1
  (nil-factory)
  ((nil-patch inc 0))
  (* 3)
  dec)

(->
  (+ 1 2)
  ((fn [step1-value]
    (->
      (* step1-value 3)
      ((fn [step2-value]
         (+ step2-value step1-value)))))))


;; exercise 1

;;(let
;;  [a (concat '(a b c) '(d e f))
;;   b (count a)]
;;  (odd? b))

(->
  (concat '(a b c) '(d e f))
  ((fn
    [a]
      (->
        (count a)
        ((fn
          [b]
            (odd? b)))))))


;; exercise 2

;;(odd?
;;  (count
;;    (concat '(a b c) '(d e f))))

(->
  '(a b c)
  ((fn
    [value1]
      (->
        (concat value1 '(d e f))
        ((fn
          [value2]
          (->
            (count value2)
            ((fn
              [value3]
                (odd? value3))))))))))


;; exercise 3

(->
  3
  (+ 2)
  inc)

(->
  3
  ((fn
    [value1]
      (->
        (+ value1 2)
        ((fn
          [value2]
            (inc value2)))))))

(def decider
     (fn [value continuation]
       (if (oopsie? value)
         value
         (continuation value))))

(defn factorial
  [n]
  (cond
    (< n 0)
      (oops! "Factorial can never be less than zero." :number n)
    (< n 2)
      1
    :else
      (* n (factorial (dec n)))))

(def maybe-monad
     (monad [m-result identity
             m-bind   decider]))

(with-monad maybe-monad
  (domonad [a nil
            b (+ 1 a)] ; would blow up
     b))

;; Error utilities
(use 'clojure.algo.monads)

(def decider
     (fn [value continuation]
       (if (oopsie? value)
         value
         (continuation value))))

(def error-monad
     (monad [m-result identity
             m-bind   decider]))

(def factorial
     (fn [n]
       (cond (< n 0)
             (oops! "Factorial can never be less than zero." :number n)

             (< n 2)
             1

             :else
             (* n (factorial (dec n))))))

(with-monad error-monad
  (domonad [a -1
            b (factorial a)
            c (factorial (- a))]
     (* a b c)))

(def +?
  (with-monad maybe-m (m-lift 2 +)))

(prn (+? 1 1))

(def pairwise-plus
  (with-monad
    sequence-m
    (m-lift 2 +)))

(pairwise-plus [1 2 3] [4 5 6])

(def combined-monad (maybe-t sequence-m))

(with-monad
  combined-monad
  (domonad
    [ a [1 nil 3]
      b [-1 -2]]
      (* a b)))

;;; Exercise 1

(defn multiples
  [n]
    (range (* n 2) 101 n))

(multiples 11)

;;; Exercise 2

;; The Sequence monad version
(def nonprimes
  (with-monad sequence-m
  (domonad
    [ i (range 2 11)
      nonprime (multiples i)]
      nonprime)))


;; The `for` version
(def nonprimes
  (for [i (range 2 11)
        nonprime (multiples i)]
    nonprime))

(nonprimes 11)

;;; Exercise 3

(def primes
       (remove (set nonprimes) (range 2 101)))
(prn "Behold! Primes:")
(prn primes)
