(ns inhouse.bus-stops)


;  There is a bus moving in the city, and it takes and drops some people in each bus stop.
;
;  You are provided with a list (or vector) of integer pairs. Elements of each pair represent the number of people
;     getting onto bus (The first item) and number of people getting off of the bus (The second item) in a bus stop.
;
;  Your task is to return number of people who are still on the bus after the last bus station (after the last array).
;     Even though it is the last bus stop, the bus is not empty and some people are still in the bus.

;  The second value in the first integer pair is 0, since the bus is empty in the first bus stop.

(def ^:dynamic *max-passenger-travel-stops* 5)
(def ^:dynamic *max-passengers-per-stop* 5)

;; Passenger
(defn passenger-ride
  [on-stop off-stop]
  [on-stop
   off-stop])

(defn shift-passenger-ride
  "increases a passengers on/off stop # by shift amount"
  [shift-count [on-stop off-stop]]
  [(+ shift-count on-stop)
   (+ shift-count off-stop)])

(defn shift-passenger-rides
  "increase passenger stops"
  [shift-count passengers-stop-s]
  (mapv (partial shift-passenger-ride shift-count) passengers-stop-s))



(defn gen-passenger-ride
  "Generate a passenger that gets on at the first stop and gets off the bus at max-stops"
  []
  (passenger-ride 0
                  (inc (rand-int *max-passenger-travel-stops*))))


(defn gen-passenger-rides
  "Generate a vector of passengers for a given bus stop"
  []
  (into [] (repeatedly (inc (rand-int *max-passengers-per-stop*))
                       gen-passenger-ride)))

(defn passenger-rides-stream
  "Generate passengers for an infi"
  []
  (lazy-seq
    (cons (gen-passenger-rides)
          (map (partial shift-passenger-rides 1)
               (passenger-rides-stream)))))


(defn busstop-on+off-stream*
  [bus-stop active-riders [next-stop-passengers & remaining-stop-passengers]]
  (let [added-passengers   (concat active-riders
                                   next-stop-passengers)
        removed-passengers (filter (fn [[_on-stop off-stop]]
                                     (< bus-stop off-stop))
                                   added-passengers)
        _                  (println (str "bus stop# " bus-stop " " (print-str removed-passengers)))
        ]
    (lazy-seq (cons [(count next-stop-passengers)
                     (- (count added-passengers)
                        (count removed-passengers))]
                    (busstop-on+off-stream* (inc bus-stop)
                                            removed-passengers
                                            remaining-stop-passengers)))))

(defn busstop-on+off-stream
  []
  (busstop-on+off-stream* 0 '() (passenger-rides-stream)))



;; Solve the problem
(defn riders-on-bus
  [busstop-on+off-s]
  (reduce (fn [accum [on off]] (+ accum
                                  on
                                  (- off)))
          0
          busstop-on+off-s))




(comment

  (passenger-ride 4 5)
  #_=> [4 5]

  (->> (passenger-ride 4 5)
       (shift-passenger-ride 2))
  #_=> [6 7]

  (binding [*max-passenger-travel-stops* 100]
    (->> gen-passenger-ride
         (repeatedly)
         (take 6)))
  #_=> ([0 4] [0 5] [0 4] [0 1] [0 4] [0 1])

  (gen-passenger-rides)
  #_=> [[0 4] [0 1] [0 3] [0 1] [0 3]]

  (->> (gen-passenger-rides)
       (mapv #(shift-stops 1 %)))
  #_> [[1 5] [1 2] [1 5] [1 6] [1 4]]

  (->> (gen-passenger-rides)
       (shift-passenger-rides 2))
  #_=> [[2 4] [2 4] [2 6] [2 4] [2 5]]


  (take 7 (passenger-rides-stream))
  #_=> '([[0 1] [0 1] [0 4] [0 4] [0 4]]
         [[1 2] [1 2] [1 5] [1 4] [1 4]]
         [[2 5] [2 7] [2 5]]
         [[3 5] [3 8] [3 8] [3 7]]
         [[4 7] [4 7] [4 5]]
         [[5 7] [5 8] [5 10]]
         [[6 11] [6 8] [6 11]])


  (->> (busstop-on+off-stream* 0 '([20 29]) (passenger-rides-stream))
       (take 5))

  (->> (busstop-on+off-stream)
       (take 20))
  #_=> '([1 0] [4 0] [3 0] [1 2] [1 4] [3 0] [4 4] [5 1] [5 4] [4 2] [5 5] [5 5])


  (->> (busstop-on+off-stream)
       (take 12)
       riders-on-bus)
  #_=> 12

  )