;; exercise 1

(map (partial + 1) [1 2 3])


;; exercise 2

;;(filter odd? [1 2 3 4]) => (1 3)
;;(remove odd? [1 2 3 4]) => (2 4)

(def separate (juxt filter remove))
(separate odd? [1 2 3 4]) ;; => [(1 3) (2 4)]


;; exercise 3

(def myfun
  (let [x 3]
    (fn [] x)))

;;x => unable to resolve
(myfun) ;; => 3


;; exercise 4

(def myotherfun
  ((fn [x]
    (fn [] x))
  3))

(myotherfun)


;; exercise 5

(def my-atom (atom 0))
(swap! my-atom inc)
(swap! my-atom (fn [current-value] 33))
@my-atom


;; exercise 6

(defn always
  [value]
    (fn [& _] value))

((always 8) 1 2 `a :foo)


;; exercise 7

(def check-sum
  (fn [sequence]
  ;;(apply + (map * (1 2 3 4 5) sequence))))
    (apply + (map * (range 1 (inc (count sequence))) sequence))

(check-sum [4 8 9 3 2])


;; exercise 8

(def reversed-digits
     (fn [string]
       (map (fn [digit-char]
              (-> digit-char str Integer.))
            (reverse string))))

(def isbn?
     (fn [candidate]
       (zero? (rem (check-sum (reversed-digits candidate)) 11))))

(isbn? "0131774115") ;; => true
(isbn? "1934356190") ;; => true
(isbn? "0977716614") ;; => false


;; exercise 9

(def upc-check-sum
     (fn [sequence]
       (apply + (map (fn [position digit]
                       (* digit (if (odd? position) 1 3)))
                     (range 1 (inc (count sequence)))
                     sequence))))


(def upc?
  (fn [candidate]
    (zero? (rem (upc-check-sum (reversed-digits candidate)) 10))))

(upc? "074182265830") ;; => true
(upc? "731124100023") ;; => true
(upc? "722252601404") ;; => false


;; exercise 10
(def number-checker
     (fn [digit-function divisor]
       (fn [candidate]
         (let [digits (reversed-digits candidate)
               check-sum (apply +
                                (map digit-function
                                     (range 1 (inc (count digits)))
                                     digits))]
           (zero? (rem check-sum divisor))))))

(def isbn? (number-checker * 11))
(def upc? (number-checker
           (fn [position digit] (* digit (if (odd? position) 1 3)))
           10))
