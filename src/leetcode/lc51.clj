(ns leetcode.lc51
  )

;https://leetcode.com/problems/n-queens/

;The n-queens puzzle is the problem of placing n queens on an n x n chessboard such that no two queens attack each other.
;Given an integer n, return all distinct solutions to the n-queens puzzle. You may return the answer in any order.
;Each solution contains a distinct board configuration of the n-queens' placement, where 'Q' and '.' both indicate a queen and an empty space, respectively.
;
;  Example 1:
;    Input: n = 4
;    Output: [[".Q..","...Q","Q...","..Q."],["..Q.","Q...","...Q",".Q.."]]
;    Explanation: There exist two distinct solutions to the 4-queens puzzle as shown above
;
; Example 2:
;   Input: n = 1
;   Output: [["Q"]]
;
; Constraints:
;   1 <= n <= 9


(defn spy [x] (println x) x)
(defn spy-pp [x] (clojure.pprint/pprint x) x)

(defn- xy-diag
  "compute xy diagonal from x and y coords"
  [[x y]]
  (+ y x))

(defn- yx-diag
  [[x y]]
  (- x y))


(defn new-board
  "Create a board of nxn size with sets of all free :rows :cols :xy-diags and :yx-diags
  and queen locations"
  [n]
  {:size       n
   :rows       (apply sorted-set (for [x (range n)] x))
   :cols       (apply sorted-set (for [x (range n)] x))
   :xy-diag    (apply sorted-set (for [x (range (inc (xy-diag [(dec n) (dec n)])))] x))
   :yx-diag    (apply sorted-set (for [x (range (yx-diag [0 (dec n)])
                                                (inc (yx-diag [(dec n) 0])))] x))
   :queen-locs #{}})


(defn disj-from-set-in-m
  "remove v from set keyed by k in m.
  (disj-from-set-in-m {:a-key #{1 2 3}} :a-key 2) => {:a-key #{1 3}}"
  [m k v]
  (assoc m k (disj (k m) v)))


(defn conj-to-set-in-m
  "add v to set keyed by k in m. If not a set, or key doesn't exist, add.
  (conj-to-set-in-m {:a-key #{1 2 3}} :a-key 4) => {:a-key #{1 2 3 4}}"
  [m k v]
  (if (or (not (contains? m k))
          (not (set? (k m))))
    (assoc m k #{v})
    (assoc m k (conj (k m) v))))


(defn drawn-board
  "Return a visual string representation of a board"
  [board]
  (into [] (for [y (range (:size board))]
             (into [] (for [x (range (:size board))]
                        (cond
                          (contains? (:queen-locs board) [x y]) (symbol "Q")
                          :else (symbol ".")))))))


(defn place-queen
  "Place a queen on the board at [x y], remove any rows/columns/diagonals attacked by the queen"
  [board [x y]]
  (let [constrain-line    disj-from-set-in-m
        constrained-board (-> board
                              (constrain-line :rows y)
                              (constrain-line :cols x)
                              (constrain-line :xy-diag (xy-diag [x y]))
                              (constrain-line :yx-diag (yx-diag [x y])))
        queened-board     (conj-to-set-in-m constrained-board
                                            :queen-locs
                                            [x y])]
    queened-board))



(defn safe-space?
  "true iff placing a queen is non-attacking on the board. i.e., does the queen
  intersect with any used rows/cols/diags"
  [board [x y]]
  (let [line-free? (fn [b k v]
                     (some? ((-> b k) v)))
        free-row-m {:row-free     (line-free? board :rows y)
                    :col-free     (line-free? board :cols x)
                    :xy-diag-free (line-free? board :xy-diag (xy-diag [x y]))
                    :yx-diag-free (line-free? board :yx-diag (yx-diag [x y]))}
        _ (clojure.pprint/pprint free-row-m)
        no-hit?    (every? identity (vals free-row-m))]
    no-hit?))




(defn dfs-nqueens
  "dfs search with constraint prop "
  [n board]
  (for [x (range n)]
    (for [y (range n)]
      [x y]
      ))


  #_(loop [solns]

      ;; enumerate
      ;; place queen
      ;; constraint prop

      )

  )



(comment

  ;; xy-diags
  (into [] (for [y (range 4)]
             (into [] (for [x (range 4)]
                        (xy-diag [x y])))))
  #_=> [[0 1 2 3]
        [1 2 3 4]
        [2 3 4 5]
        [3 4 5 6]]


  ;; yx-diags
  (into [] (for [y (range 4)]
             (into [] (for [x (range 4)]
                        (yx-diag [x y])))))
  #_=> [[0 1 2 3]
        [-1 0 1 2]
        [-2 -1 0 1]
        [-3 -2 -1 0]]


  (safe-space? [1 1] (new-board 2))



  ;; Board
  (new-board 4)
  #_=> {:size       4,
        :rows       #{0 1 2 3}, :cols #{0 1 2 3}, :xy-diag #{0 1 2 3 4 5 6}, :yx-diag #{-3 -2 -1 0 1 2 3},
        :queen-locs #{}}

  (drawn-board {:size 8 :queen-locs #{[1 2] [5 4]}})
  #_=> [[. . . . . . . .]
        [. . . . . . . .]
        [. Q . . . . . .]
        [. . . . . . . .]
        [. . . . . Q . .]
        [. . . . . . . .]
        [. . . . . . . .]
        [. . . . . . . .]]


  (disj-from-set-in-m {:a-key #{1 2 3} :b 2 :c #{1 2}} :a-key 2)
  #_=> {:a-key #{1 3}, :b 2, :c #{1 2}}

  (conj-to-set-in-m {:a-key #{1 2 3} :b 2 :c #{1 2}} :a-key 4)
  #_=> {:a-key #{1 4 3 2}, :b 2, :c #{1 2}}

  [(conj-to-set-in-m {:a 1} :b 3)
   (conj-to-set-in-m {:a 2} :a 3)]
  #_=> [{:a 1, :b #{3}} {:a #{3}}]

  (-> (new-board 8)
      (place-queen [1 2])
      (place-queen [4 4])
      (place-queen [6 5])
      ((juxt identity drawn-board)))
  #_=> [{:size       8,
         :rows       #{0 1 3 6 7},
         :cols       #{0 2 3 5 7},
         :xy-diag    #{0 1 2 4 5 6 7 9 10 12 13 14},
         :yx-diag    #{-7 -6 -5 -4 -3 -2 2 3 4 5 6 7},
         :queen-locs #{[6 5] [4 4] [1 2]}}
        #_[[. . . . . . . .]
           [. . . . . . . .]
           [. Q . . . . . .]
           [. . . . . . . .]
           [. . . . Q . . .]
           [. . . . . . Q .]
           [. . . . . . . .]
           [. . . . . . . .]]]


  (-> (new-board 8)
      (place-queen [1 2])
      (place-queen [4 4])
      (place-queen [6 5])
      ((juxt identity
             drawn-board
             #(safe-space? % [6 6])
             #(safe-space? % [2 0]))))

  ((fn [b k v]
     (any? ((-> b k) v)))
   {:size       8,
    :rows       #{0 1 3 6 7},
    :cols       #{0 2 3 5 7},
    :xy-diag    #{0 1 2 4 5 6 7 9 10 12 13 14},
    :yx-diag    #{-7 -6 -5 -4 -3 -2 2 3 4 5 6 7},
    :queen-locs #{[6 5] [4 4] [1 2]}}
   :rows
   5
   )

  (apply and [true false])

  (some?
    ((-> {:size       8,
          :rows       #{0 1 3 6 7},
          :cols       #{0 2 3 5 7},
          :xy-diag    #{0 1 2 4 5 6 7 9 10 12 13 14},
          :yx-diag    #{-7 -6 -5 -4 -3 -2 2 3 4 5 6 7},
          :queen-locs #{[6 5] [4 4] [1 2]}} :rows)
     6))

  )