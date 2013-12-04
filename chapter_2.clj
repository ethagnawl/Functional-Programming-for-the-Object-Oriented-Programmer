;;(prn (hash-map :a 1 :b 2))

(apply hash-map [:a 1 :b 2])

(:a {:a 1})

(assoc {:a 1} :b 2 :c 3)

(merge {:a 1} (assoc {:b 2} :c 3))


(dissoc (merge {:a 1} (assoc {:b 2} :c 3)) :a)

(assoc {:a 1} :a 2222)


(def Point
     (fn [x y]
       {:x x,
        :y y
        :__class_symbol__ 'Point
        :__methods__ {
           :x :x
           :y :y
           :class :__class_symbol__
           :shift (fn [this xinc yinc]
                    (make Point (+ (send-to this :x) xinc)
                    (+ (send-to this :y) yinc)))
           :add (fn [this other]
                  (send-to this :shift (send-to other :x)
                                       (send-to other :y)))}}))

(:shift (:__methods__ (make Point 1 2)))
((:shift (:__methods__ point)) point -2 -3)
(def point (make Point 1 2))

(def send-to
       (fn [object message & args]
                (apply (message (:__methods__ object)) object args)))

(send-to (make Point 10 20) :shift -2 -3)

(prn (send-to (make Point 1 1) :x))
(prn (send-to (make Point 1 1) :y))

(prn (send-to (make Point 1 1) :add (make Point 10 10)))

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

;(def add
;  (fn [this other]
;    (Point  (+ (x this) (x other))
;            (+ (y this) (y other)))))
;
;(prn (add (Point 1 2) (Point 3 4)))

(def add
  (fn [this other]
    (shift this (x other) (y other))))

(prn (add (Point 1 2) (Point 3 4)))

(defn make
  [type & args]
    (apply type args))

(prn (make Point 1 2))

(def add
     (fn [this other]
       (Point (+ (:x this) (:x other))
              (+ (:y this) (:y other)))))

(def add
     (fn [this other]
       (shifted this (:x other) (:y other))))

(def Triangle
  (fn [point1 point2 point3]
    (if (not (= 3 (count (set [point1 point2 point3]))))
      (throw (new Error (str "Not a triangle:" point1 point2 point3)))
      {:point1 point1, :point2 point2, :point3 point3})))

(make Triangle
      (make Point 1 2)
      (make Point 1 3)
      (make Point 3 1))

(def point {:x 1, :y 2, :__class_symbol__ 'Point})

(def Point
     (fn [x y]
       {:x x,
        :y y
        :__class_symbol__ 'Point}))

(def x :x)
(def y :y)
(def class-of :__class_symbol__)

(def shift
     (fn [this xinc yinc]
       (Point (+ (x this) xinc)
              (+ (y this) yinc))))

(def Triangle
     (fn [point1 point2 point3]
       {:point1 point1, :point2 point2, :point3 point3
        :__class_symbol__ 'Triangle}))


(def right-triangle (Triangle (Point 0 0)
                              (Point 0 1)
                              (Point 1 0)))

(def equal-right-triangle (Triangle (Point 0 0)
                                    (Point 0 1)
                                    (Point 1 0)))

(def different-triangle (Triangle (Point 0 0)
                                  (Point 0 10)
                                  (Point 10 0)))

(def equal-triangles? =)

;; identical
(equal-triangles? right-triangle right-triangle)

;; not identical, but contents are equal
(equal-triangles? right-triangle equal-right-triangle)

;; not equal
(equal-triangles? right-triangle different-triangle)


;; multiple
(equal-triangles? right-triangle right-triangle equal-right-triangle)

(def valid-triangle?
  (fn [& points]
    (and (= 3 (count points))
    (= (distinct points) points))))

(valid-triangle?
  (make Triangle
    (make Point 1 2)
    (make Point 2 2)
    (make Point 2 3)))
