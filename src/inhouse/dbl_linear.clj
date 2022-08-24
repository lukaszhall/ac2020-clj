(ns inhouse.dbl-linear)

; Consider a sequence u where u is defined as follows:
; 1. The number u(0) = 1 is the first one in u.
; 2. For each x in u, then y = 2 * x + 1 and z = 3 * x + 1 must be in u too.
; 3. There are no other numbers in u.
;
;  Ex: u = [1, 3, 4, 7, 9, 10, 13, 15, 19, 21, 22, 27, ...]
;
; 1 gives 3 and 4, then 3 gives 7 and 10, 4 gives 9 and 13, then 7 gives 15 and 22 and so on...
;
;  Task:
;  Given parameter n the function dbl-linear returns the element u(n) of the ordered (with <) sequence u (so, there are no duplicates).
;  Example:
;  (dbl-linear 10) => 22


;; As lambda calculus

;; pos  u0   u1     u2     u3     u4     u5     u6     u7     u8     u9
;; u = [1,   3    , 4    , 7    , 9    , 10   , 13   , 15   , 19   , 21     22,     27, ]
;; u = [1,   y(u0), z(u0), y(u1), y(u2), z(u1), z(u2), y(u3), z(u3),       ,        z(u[3])


(defn fn-y
  [x]
  (+ 1 (* 2 x)))

(defn fn-z
  [x]
  (+ 1 (* 3 x)))

(defn dbl-linear-algo
  [n]
  (loop [u   [1]
         idx 0]
    (if (= n idx)
      (nth (sort (distinct u)) n)
      (let [y          (fn-y (get u idx))
            z          (fn-z (get u idx))
            expanded-u (conj u y z)]
        (recur expanded-u
               (inc idx))))))



;;  --==  functional ==--

(defn fn-y+z
  [u]
  [(fn-y u)
   (fn-z u)])

(defn lazy-mapcat
  [f coll]
  (lazy-seq
    (if (not-empty coll)
      (concat
        (f (first coll))
        (lazy-mapcat f (rest coll))))))

(defn elements
  []
  (lazy-seq
    (concat [1]
            (lazy-mapcat fn-y+z (elements)))))


(defn dbl-linear-functional
  [n]

  (let [u (->> (elements)
               (take (* 3 (inc n)))
               distinct
               sort)]
    (nth u n)))




(comment

  ;;  --=========  algorithmic =========--

  ;; fn-y
  (fn-y 2)
  #_=> 5

  (->> (range 5)
       (mapv (juxt identity fn-y)))
  #_=> [[0 1] [1 3] [2 5] [3 7] [4 9]]

  ;; fn-z
  (fn-z 2)
  #_=> 7

  (->> (range 5)
       (mapv (juxt identity fn-z)))
  #_=> [[0 1] [1 4] [2 7] [3 10] [4 13]]



  ;; Solution
  (dbl-linear-algo 10)
  #_=> 22

  (mapv dbl-linear-algo (range 0 20))
  #_=> [1 3 4 7 9 10 13 15 19 21 22 27 28 31 39 40 43 45 46 55]

  (mapv dbl-linear-algo (range 90 110))
  #_=> [381 382 387 388 391 405 405 406 409 418 471 475 477 478 478 487 489 490 495 496]



  ;;  --=========  functional =========--

  ;; fn-y+z
  (fn-y+z 2)
  #_=> [5 7]

  (->> (range 5)
       (mapv (juxt identity fn-y+z)))
  #_=> [[0 [1 1]] [1 [3 4]] [2 [5 7]] [3 [7 10]] [4 [9 13]]]




  ;; elements
  (take 30 (elements))
  #_=> '(1 3 4 7 10 9 13 15 22 21 31 19 28 27 40 31 46 45 67 43 64 63 94 39 58 57 85 55 82 81)


  ;; Solution
  (dbl-linear-functional 10)
  #_=> 22

  (mapv dbl-linear-functional (range 0 20))
  #_=> [1 3 4 7 9 10 13 15 19 21 22 27 28 31 39 40 43 45 46 55]

  (mapv dbl-linear-functional (range 90 110))
  #_=> [379 381 382 387 388 391 405 406 409 418 447 463 471 475 477 478 487 489 490 495]



  ;; examples
  (->> (map fn-y+z (range 10))
       flatten)

  (->> (conj [1 2 3] (fn-y+z 4))
       flatten)

  (concat [1 2 3] (fn-y+z 4))

  (apply conj [1 2 3] (fn-y+z 4))





  )