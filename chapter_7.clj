(prn (set [1 1 1 2 3 4]))

(prn (contains? #{1 2 3 4} 1))

(prn (#{1 2 3 4} 1)) ;; => 1
(prn (#{1 2 3 4} 5)) ;; => nil

(use 'clojure.set)
(prn (union #{1 2} #{3 4})) ;; => #{1 2 3 4}
(prn (intersection #{1 2} #{1 3 4})) ;; => #{1}
(prn (difference #{1 2} #{2 3})) ;; => #{1}
(prn (filter odd? #{1 2 3})) ;; => {seq} (1 3)
(prn (set? (filter odd? #{1 2 3}))) ;; => false
(prn (select odd? #{1 2 3})) ;; => {1 3}


(defn answer-annotations
  [courses registrants-courses]
    (let [checking-set (set registrants-courses)]
      (map (fn [course]
        (assoc course
          :spaces-left (- (:limit course) (:registered course))
          :already-in? (contains? checking-set (:course-name course))))
      courses)))

;;(answer-annotations [
;;                      {
;;                        :course-name "zigging"
;;                        :limit 4
;;                        :registered 3
;;                      }
;;                      {
;;                        :course-name "zagging"
;;                        :limit 1
;;                        :registered 1
;;                      }] ["zagging"])

(defn domain-annotations
  [courses]
    (map
      (fn [course]
        (assoc course
          :empty? (zero?  (:registered course))
          :full? (zero?  (:spaces-left course))))
      courses))

(prn (domain-annotations [{:registered 1 :spaces-left 1}]))

(def note-unavailability
     (fn [courses instructor-count]
       (let [out-of-instructors?
             (= instructor-count
                (count (filter (fn [course] (not (:empty? course)))
                               courses)))]
         (map (fn [course]
                (assoc course
                       :unavailable? (or (:full? course)
                                         (and out-of-instructors?
                                              (:empty? course)))))
              courses))))

(defn annotate
  [courses registrants-courses instructor-count]
    (-> courses
      (answer-annotations registrants-courses)
      domain-annotations
      (note-unavailability instructor-count)))

(annotate [{ :course-name "zagging" :limit 1 :registered 1 }]
          ["zagging"]
          1) ;; => ({:unavailable? true, :full? true, :empty? false, :already-in? true, :spaces-left 0, :registered 1, :limit 1, :course-name "zagging"})

(-> [1] first inc list) ;; == (list (inc (first [1]))) => (2)
(-> [1] first inc (* 3) list) ;; == (list (* 3 (inc (first [1])))) => (6)
(-> 3 ((fn [n] (* 2 n))) inc)
(-> (+ 1 2) (* 3) (+ 4)) ;;(+ (* (+ 1 2) 3) 4)
