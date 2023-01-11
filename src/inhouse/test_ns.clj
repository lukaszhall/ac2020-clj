(ns inhouse.test-ns)


(def my-state (atom {}))

(defn my-inc
  [n]
  (+ n 2))









;; Rich Comment
(comment

  (my-inc 2)
  #_=> 3


  (map inc [1 2 3])

  (->> [1 2 3 4]
       (map inc)
       (filter even?))

  )





















(+ 1
   (+ 1 2 (/ 2
             1)))


'(1 2 3)
[1 2 3]

#{1 2 3}

(get {"a" 1
      2   "c"}
     "a")

({:a 1
  :b 2})

; ------

(def a [1 2 3])

(conj a 4)

(def b {:a 1
        :b 2})

(assoc b :b 3)



(defn my-function
  [x]
  (let [clean-x (dissoc x :a)
        new-b   3]
    (assoc clean-x :b new-b)))

(assoc (dissoc b :a) :b 3)


(def person {:name     "Mark Volkmann"
             :address  {:street "644 Glen Summit"
                        :city   "St. Charles"
                        :state  "Missouri"
                        :zip    63304}
             :employer {:name    "Object Computing, Inc."
                        :address {:street "12140 Woodcrest Dr."
                                  :city   "Creve Coeur"
                                  :state  "Missouri"
                                  :zip    63141}}})


(get (get (get person :employer) :address) :zip)


(-> (+ 1 2)
    (- 6)
    (/ 2))

(-> person :employer :address :zip)