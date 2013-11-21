;;(prn (hash-map :a 1 :b 2))

(apply hash-map [:a 1 :b 2])

(:a {:a 1})

(assoc {:a 1} :b 2 :c 3)

(merge {:a 1} (assoc {:b 2} :c 3))


(dissoc (merge {:a 1} (assoc {:b 2} :c 3)) :a)

(assoc {:a 1} :a 2222)

(def Point
  (fn [x y]
    {:x x
     :y y
     :__class_symbol__ 'Point}))

;;(:__class_symbol__ i)
(def class-of :__class_symbol__)


(def i (Point 1 1))
(:x i)

(def x (fn [self] (:x self)))
(def y (fn [self] (:y self)))

(x i)
(class-of i)

(def shift
  (fn [this xinc yinc]
    (Point
      (+ (x this) xinc)
      (+ (y this) yinc))))

(shift (Point 1 200) 11 200)
