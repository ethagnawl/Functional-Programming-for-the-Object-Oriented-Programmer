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
