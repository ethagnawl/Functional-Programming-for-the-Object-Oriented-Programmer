(defn method-from-message
  [message class]
  (message (:__instance_methods__ class)))

(defn class-from-instance
  [instance]
  (eval (:__class_symbol__ instance)))

(def apply-message-to
  (fn [class instance message args]
    (apply (message (method-cache class)) instance args)))

(defn make
  [class & args]
  (let [seeded {:__class_symbol__ (:__own_symbol__ class)}]
    (apply-message-to class seeded :add-instance-values args)))

(defn send-to
  [instance message & args]
    (apply-message-to
      (class-from-instance instance) instance message args))

(prn (send-to (make Point 7 11) :shift -1 -2))

(defn class-instance-methods
  [class-symbol]
    (:__instance_methods__ (eval class-symbol)))

;;(defn recursive-function
;;  [something]
;;    (if (**ending-case?** something)
;;      (**ending-value** something)
;;      (**combiner**
;;        something
;;        (recursive-function **smaller-structure-from**))))
;;
;;(defn recursive-function
;;  [something so-far]
;;    (if (**ending-case?** something)
;;      so-far
;;      (recursive-function
;;        (**smaller-structure-from** something)
;;        (**combiner** something so-far))))


(defn lineage-1
  [class-symbol so-far]
    (if (nil? class-symbol)
      so-far
      (recur
        (class-symbol-above class-symbol)
        (cons class-symbol so-far))))

(defn lineage
  [class-symbol]
    (lineage-1 class-symbol []))

(defn class-symbol-above
  [class-symbol]
    (:__superclass__symbol__ (eval class-symbol)))

(def method-cache
  (fn
    [class]
      (let [class-symbol (:__own_symbol__ class)
            method-maps (map class-instance-methods (lineage class-symbol))]
        (apply merge method-maps))))

(def Anything
  {
     :__own_symbol__ 'Anything
    :__instance_methods__ {
                           :add-instance-values identity
                           :class-name :__class_symbol__
                           :class (fn
                                    [this]
                                      (class-from-instance this))}})

(def Point
  {
    :__own_symbol__ 'Point
    :__superclass__ 'Anything
    :__instance_methods__ {
      :add-instance-values (fn [this x y]
                             (assoc this :x x :y y))
      :class-name :__class_symbol__
      :class (fn [this] (class-from-instance this))
      :shift (fn [this xinc yinc]
               (make Point (+ (:x this) xinc)
                           (+ (:y this) yinc)))
      :add (fn [this other]
             (send-to this :shift (:x other)
                                  (:y other)))}})

;;(send-to (make Point 1 2) :class-name)
(def my-point (make Point 1 2))
(prn my-point)


;;; Exercise 3


(def my-point (make Point 1 2))

(def Point
{
  :__own_symbol__ 'Point
  :__instance_methods__
  {
    :add-instance-values (fn [this x y]
                           (assoc this :x x :y y))
    :origin (fn [this] (make Point 0 0))
    :class-name :__class_symbol__
    :class (fn [this] (class-from-instance this))
    :shift (fn [this xinc yinc]
             (make Point (+ (:x this) xinc)
                         (+ (:y this) yinc)))
    :add (fn [this other]
           (send-to this :shift (:x other)
                                (:y other)))
   }
 })

(send-to my-point :origin)

;; Redefining a class changes the behavior of existing instances
;; because having an instance's :__class_symbol__ be a symbol that's
;; later `eval`ed adds a level of indirection. If the value were a
;; class map itself, changing the binding or association of `Point`
;; would have no effect on existing instances, just new ones.
;;
;; You could see that with this code:
;;    user=> (def Point "the original definition of Point")
;;    user=> (def a-point {:__class_NOT_symbol__ Point})
;;    user=> (def Point "the new definition of Point")
;;    user=> a-point
;;    {:__class_NOT_symbol__ "the original definition of Point"}
;;
;; If that's not clear, apply the substitution rule to the `def` lines.
;; (Note that `def` is another special symbol. It does not evaluate its
;; first argument, just the second.)
;;
;; "All problems in computer science can be solved by another level of
;; indirection." -- David Wheeler
;;

;;; Exercise 4


(defn apply-message-to
  [class instance message args]
  (let [method (or
                 (method-from-message message class)
                 message)]
    (apply method instance args)))

(def Holder
  {
    :__own_symbol__ 'Holder
   :__instance_methods__ {
                            :test (fn [& args] "test")
                            :add-instance-values (fn
                                                   [this held]
                                                   (assoc this :held held))
                         }
  })


(prn (send-to (make Holder "stuff") :held)) ;; => "stuff"
(prn (send-to (make Holder "stuff") :test)) ;; => "test"
(prn (send-to (make Holder "stuff") :wat)) ;; => nil

(defn factorial-1
  [n]
    (if (or (= n 0) (= n 1))
      1
      (* n (factorial (dec n)))))

;;(factorial-1 10)

(defn factorial-2
  [n so-far]
    (prn "---")
    (prn n)
    (prn so-far)
    (prn "---")
    (if (or
      (= n 0)
      (= n 1))
        so-far
        (factorial-2 (dec n) (* n so-far))))


(def factorial
  (fn [n] (factorial-2 n 1)))

(factorial-2 10 1)


(defn recursive-function
  [op something so-far]
    (if (empty? something)
      so-far
      (recursive-function op (rest something)
        (op (first something) so-far))))

;;(recursive-function (fn [x y] (+ x y)) [1 2 3 4] 1)
;;(recursive-function (fn [x y] (* x y)) [1 2 3 4] 1)
(recursive-function
                    #(assoc %2 %1 (count %2))
                    [:a :b :c]
                    {})
