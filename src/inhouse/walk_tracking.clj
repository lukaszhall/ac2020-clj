(ns inhouse.walk-tracking)


;You live in a city where all roads are laid out in a perfect grid. You have the unfortunate habit of arriving too early or
;too late to your appointments, so you decide to create a Walk Tracking App.
;
;You want create the application that tracks where you have walked and will then give you cardinal directions back to your
;starting point. (eg. ['n','s','w','e'] )
;
;Write a function that will take the currently walked path (e.g., ['n' 'e' 'e' ...]) and return the path that will lead you
;back to your origin.

;; ----====  Data Generation ====----
(defn gen-path
  [path-length]
  (mapv (fn [_] (rand-nth [:e :w :n :s]))
        (range path-length)))



(defmulti walk-back (fn [strategy _path] strategy))

;; ----====  Solution : Invert Path ====----

(def dir->inverted-dir {:n :s
                        :s :n
                        :e :w
                        :w :e})

(defmethod walk-back :retrace
  [_ path]
  {:walk-path   path
   :return-path (mapv dir->inverted-dir
                      path)})



;; ----====  Solution : Vector Space ====----

(def dir->vector {:n [0 1]
                  :s [0 -1]
                  :e [1 0]
                  :w [-1 0]})

(def vector->dir (clojure.set/map-invert dir->vector))


(defn zero-vector
  [basis-vector]
  (-> (count basis-vector)
      (repeat 0)
      (vec)))


(defn +_v
  ([vec1]
   vec1)

  ([[x1 y1] [x2 y2]]
   [(+ x1 x2) (+ y1 y2)])

  ([vec1 vec2 & remaining]
   (+_v vec1
        (apply +_v (cons vec2 remaining)))))


(defn axis-path-from-location
  [vec-idx vec1]
  (let [negated-value (* (get vec1 vec-idx)
                         -1)
        units         (abs negated-value)
        unit-vector   (assoc (zero-vector vec1)
                        vec-idx
                        (/ negated-value units))
        unit-vector-s (take units (repeat unit-vector))]
    (into [] unit-vector-s)))


(defn origin-path-from-location
  [vec1]
  (->> (for [idx (range (count vec1))]
         (axis-path-from-location idx vec1))
       (apply concat)
       (into [])))

(defmethod walk-back :vectorized
  [_ path]
  (let [path-vectors    (mapv path-direction-vectors
                              path)
        location        (apply +_v path-vectors)
        origin-vec-path (origin-path-from-location location)
        dir-path        (mapv vector->dir origin-vec-path)]

    {:walk-path        path
     :return-path      dir-path
     :walk-path-vecs   path-vectors
     :return-path-vecs origin-vec-path}))


(comment

  ;; Test data
  (gen-path 7)
  #_=> [:e :n :n :n :w :s :s]

  ;; Solution : Invert Path (retrace steps)
  (->> (gen-path 30)
       (walk-back :retrace))
  #_=> {:walk-path   [:s :w :w :n :e :e :n :e :w :e :s :s :n :w :e :e :s :n :n :w :e :w :n :n :n :e :n :e :e :w],
        :return-path [:n :e :e :s :w :w :s :w :e :w :n :n :s :e :w :w :n :s :s :e :w :e :s :s :s :w :s :w :w :e]}


  ;; Solution : Vector Space

  (zero-vector [4 3 1 3])
  #_=> [0 0 0 0]

  ;; Vector addition
  (+_v [1 0]
       [3 -4])
  #_=> [4 -4]

  (+_v [1 0] [8 2] [9 4] [9 4])
  #_=> [27 10]


  ;; Single axis return
  (axis-path-from-location 0 [4 7])
  #_=> => [[-1 0] [-1 0] [-1 0] [-1 0]]

  (axis-path-from-location 1 [4 7])
  #_=> [[0 -1] [0 -1] [0 -1] [0 -1] [0 -1] [0 -1] [0 -1]]

  (-> (apply +_v (axis-path-from-location 1 [4 7]))
      (+_v [4 7]))
  #_=> [4 0]


  ;; ALL axis return
  (origin-path-from-location [-1 2 -3])
  #_=> [[1 0 0] [0 -1 0] [0 -1 0] [0 0 1] [0 0 1] [0 0 1]]



  ;; Shortest return path
  (->> (gen-path 10)
       (walk-back :vectorized))
  #_=> {:walk-path        [:n :n :s :e :w :s :n :s :s :e],
        :return-path      [:w :n],
        :walk-path-vecs   [[0 1] [0 1] [0 -1] [1 0] [-1 0] [0 -1] [0 1] [0 -1] [0 -1] [1 0]],
        :return-path-vecs [[-1 0] [0 1]]}

  )

