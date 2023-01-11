(ns inhouse.d6)


(defn roll-d6
  []
  (inc (int (rand 6))))

(defn game-sum
  [n]
  (let [rolls (for [x (range n)]
                (roll-d6))
        sum   (reduce + rolls)]
    sum))

(defn simulation
  "Run a game of n dice rolls, m times. i.e., m simulations"
  [m n]
  (let [simulations (for [x (range m)]
                      (game-sum n))]
    (sort simulations)))

(defn simulation-analysis
  [simulation-sums]
  (frequencies simulation-sums))

(defn number-as-string
  [n]
  (apply str
         (for [x (range n)]
           "0")))

(number-as-string 4)

(comment

  (game-sum 100)

  (->> (simulation-analysis (simulation 10000 #_10 10))
       (map (fn [[x y]] {x (number-as-string y)}))
       (into (sorted-map)))

  (map (fn [[x y]] {x (Long/toString y 2)})
       {26 1, 27 2, 28 2, 29 1, 39 3, 44 1})

  ()

  )