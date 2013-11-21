
(+ 1 2)

['(+ 1 2) '(- 1 1)]

(map * [0 1 2 3] [100 200 300 400])

(def add-squares (fn [& squares] (apply + (map * squares squares))))

(add-squares 1 2 5)

(defn assert [expected actual]
                (pprint ["expected" expected])
                (pprint ["actual" expected])
                (= expected actual))

(assert 30 (add-squares 1 2 5))

(defn fac [n] (apply * (range 1 (inc n))))

(assert 120 (fac 5))

(doc partition)

(defn prefix-of? [prefix collection]
      (=
        prefix
        (take (count prefix) collection)))

(assert true (prefix-of? [1 2] [1 2 3]))
(assert true (prefix-of? '(1 2) [1 2 3]))

(defn tails [lst]
  (map drop
    (range (inc (count lst)))
    (repeat (inc (count lst)) lst)))


(tails '(1 2 3 4))

(assert
  '((1 2 3 4) (2 3 4) (3 4) (4) ())
  (tails '(1 2 3 4)))
