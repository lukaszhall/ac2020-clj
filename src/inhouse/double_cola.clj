(ns inhouse.double-cola

  )


;; Algorithmic

(defn who-is-next
  "Find nth consumer of double-cola using algorithmic approach"
  [queue n]
  (loop [queue-idx  0
         multiplier 1
         depth      0]
    (let [next-depth (+ depth multiplier)]
      (cond
        ;; n falls in next set
        (>= next-depth n)
        (get queue queue-idx)

        ;; end of queue, next iteration will be double
        (>= queue-idx (-> queue count dec))
        (recur 0
               (* 2 multiplier)
               next-depth)

        ;; move on to next person
        :else
        (recur (inc queue-idx)
               multiplier
               next-depth)))))


;; Lambda Calculus
(defn name->element
  [name]
  [1 name])

(defn double-element
  [[size name]]
  [(* 2 size) name])

(defn elements
  [names]
  (lazy-seq
    (concat (map name->element names)
            (map double-element (elements names)))))

(defn nth-element
  [queue n]
  (loop [n n
         [[size name] & remaining] (-> queue elements)]
    (if (<= n size)
      name
      (recur (- n size)
             remaining))))




(comment

  ;; Sample test data from problem
  (who-is-next ["Sheldon", "Leonard", "Penny", "Rajesh", "Howard"], 8)
  (who-is-next ["Sheldon", "Leonard", "Penny", "Rajesh", "Howard"], 52) => "Penny"
  (who-is-next ["Sheldon", "Leonard", "Penny", "Rajesh", "Howard"], 7230702951) => "Leonard"


  ;; Baseline queue
  (def sample-queue ["Sheldon", "Leonard", "Penny", "Rajesh", "Howard"])


  ;; Algorithmic solution
  (mapv (partial who-is-next sample-queue)
        [8 52 7230702951])
  #_=> ["Leonard" "Penny" "Leonard"]


  ;; Lambda Calculus
  (name->element "Sheldon")
  #_=> [1 "Sheldon"]

  (double-element [1 "Sheldon"])
  #_=> [2 "Sheldon"]


  (take 10 (elements sample-queue))
  #_=> '([1 "Sheldon"]
         [1 "Leonard"]
         [1 "Penny"]
         [1 "Rajesh"]
         [1 "Howard"]
         [2 "Sheldon"]
         [2 "Leonard"]
         [2 "Penny"]
         [2 "Rajesh"]
         [2 "Howard"])

  (mapv (partial nth-element sample-queue)
        [8 52 7230702951])
  #_=> => ["Leonard" "Penny" "Leonard"]




  )



