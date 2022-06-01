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


(defn spy
  ([x] (println x) x)
  ([msg x] (println (str msg x)) x))

(defn spy-pp
  ([x] (clojure.pprint/pprint x) x)
  ([msg x] (println msg) (clojure.pprint/pprint x) x))

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
  ([board]
   (into [] (for [y (range (:size board))]
              (into [] (for [x (range (:size board))]
                         (cond
                           (contains? (:queen-locs board) [x y]) (symbol "Q")
                           :else (symbol ".")))))))
  ([board other-locs]
   (drawn-board board other-locs (symbol "*")))
  ([board other-locs other-locs-sym]
   (let [locs-set (into #{} other-locs)]
     (into [] (for [y (range (:size board))]
                (into [] (for [x (range (:size board))]
                           (cond
                             (contains? (:queen-locs board) [x y]) (symbol "Q")
                             (contains? locs-set [x y]) (symbol "*")
                             :else (symbol "."))))))

     ))
  )



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
        ;_          (clojure.pprint/pprint free-row-m)
        ;_          (println [x y])
        no-hit?    (every? identity (vals free-row-m))]
    no-hit?))


(defn safe-spaces
  "Returns a vector of all board coordinates where a queen may be placed"
  [board]
  (into []
        (for [row (range (:size board))
              col (range (:size board))
              :when (safe-space? board [col row])]
          [col row])))


(defn trial-boards
  "Given a board with 0+ queens, returns a seq of boards with a valid newly placed queen"
  [board]
  (let [trial-spaces (safe-spaces board)
        boards       (map (partial place-queen board) trial-spaces)]
    boards))

(defn dfs-nqueens
  "dfs search with constraint prop "
  [board]
  (if (= (count (:queen-locs board))
         (:size board))
    board
    (let [test-boards     (trial-boards board)
          searched-boards (map dfs-nqueens test-boards)]
      searched-boards)))


(def dfs-nqueens
  (memoize
    (fn [board]
      (if (= (count (:queen-locs board))
             (:size board))
        board
        (let [test-boards     (trial-boards board)
              searched-boards (map dfs-nqueens test-boards)]
          searched-boards)))))


(defn dfs-nqueens-clean
  "cleaned results for dfs-nqueens search"
  [board]
  (->> (dfs-nqueens board)
       flatten
       distinct))

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
        :rows       #{0 1 2 3},
        :cols       #{0 1 2 3},
        :xy-diag    #{0 1 2 3 4 5 6},
        :yx-diag    #{-3 -2 -1 0 1 2 3},
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

  ;; disj-from-set-in-m
  (disj-from-set-in-m {:a-key #{1 2 3} :b 2 :c #{1 2}} :a-key 2)
  #_=> {:a-key #{1 3}, :b 2, :c #{1 2}}

  ;; conj-to-set-in-m
  (conj-to-set-in-m {:a-key #{1 2 3} :b 2 :c #{1 2}} :a-key 4)
  #_=> {:a-key #{1 4 3 2}, :b 2, :c #{1 2}}

  [(conj-to-set-in-m {:a 1} :b 3)
   (conj-to-set-in-m {:a 2} :a 3)]
  #_=> [{:a 1, :b #{3}} {:a #{3}}]

  ;; drawn-board, basic for queens
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


  ;; drawn-board, extra chars
  (drawn-board (new-board 4) [[1 1] [3 0]])
  #_=> [[. . . *]
        [. * . .]
        [. . . .]
        [. . . .]]


  ;; safe-space
  (-> (new-board 8)
      (place-queen [1 2])
      (place-queen [4 4])
      (place-queen [6 5])
      ((juxt identity
             drawn-board
             #(safe-space? % [6 6])
             #(safe-space? % [2 0]))))
  #_=> [{:size       8,
         :rows       #{0 1 3 6 7},
         :cols       #{0 2 3 5 7},
         :xy-diag    #{0 1 2 4 5 6 7 9 10 12 13 14},
         :yx-diag    #{-7 -6 -5 -4 -3 -2 2 3 4 5 6 7},
         :queen-locs #{[6 5] [4 4] [1 2]}}
        [[. . . . . . . .]
         [. . . . . . . .]
         [. Q . . . . . .]
         [. . . . . . . .]
         [. . . . Q . . .]
         [. . . . . . Q .]
         [. . . . . . . .]
         [. . . . . . . .]]
        false
        true]


  ;; safe-spaces
  (-> (new-board 8)
      (place-queen [1 2])
      (place-queen [4 4])
      (place-queen [6 5])
      (safe-spaces)
      ((juxt count
             identity)))
  #_=> => [12 [[2 0] [5 0] [7 0] [3 1] [5 1] [7 3] [0 6] [3 6] [0 7] [2 7] [3 7] [5 7]]]

  (-> (new-board 8)
      (safe-spaces)
      count)
  #_=> 64


  ;; trial boards
  (trial-boards (new-board 4))
  #_=> '({:size 4, :rows #{1 2 3}, :cols #{1 2 3}, :xy-diag #{1 2 3 4 5 6}, :yx-diag #{-3 -2 -1 1 2 3}, :queen-locs #{[0 0]}}
         {:size 4, :rows #{1 2 3}, :cols #{0 2 3}, :xy-diag #{0 2 3 4 5 6}, :yx-diag #{-3 -2 -1 0 2 3}, :queen-locs #{[1 0]}}
         {:size 4, :rows #{1 2 3}, :cols #{0 1 3}, :xy-diag #{0 1 3 4 5 6}, :yx-diag #{-3 -2 -1 0 1 3}, :queen-locs #{[2 0]}}
         {:size 4, :rows #{1 2 3}, :cols #{0 1 2}, :xy-diag #{0 1 2 4 5 6}, :yx-diag #{-3 -2 -1 0 1 2}, :queen-locs #{[3 0]}}
         {:size 4, :rows #{0 2 3}, :cols #{1 2 3}, :xy-diag #{0 2 3 4 5 6}, :yx-diag #{-3 -2 0 1 2 3}, :queen-locs #{[0 1]}}
         {:size 4, :rows #{0 2 3}, :cols #{0 2 3}, :xy-diag #{0 1 3 4 5 6}, :yx-diag #{-3 -2 -1 1 2 3}, :queen-locs #{[1 1]}}
         {:size 4, :rows #{0 2 3}, :cols #{0 1 3}, :xy-diag #{0 1 2 4 5 6}, :yx-diag #{-3 -2 -1 0 2 3}, :queen-locs #{[2 1]}}
         {:size 4, :rows #{0 2 3}, :cols #{0 1 2}, :xy-diag #{0 1 2 3 5 6}, :yx-diag #{-3 -2 -1 0 1 3}, :queen-locs #{[3 1]}}
         {:size 4, :rows #{0 1 3}, :cols #{1 2 3}, :xy-diag #{0 1 3 4 5 6}, :yx-diag #{-3 -1 0 1 2 3}, :queen-locs #{[0 2]}}
         {:size 4, :rows #{0 1 3}, :cols #{0 2 3}, :xy-diag #{0 1 2 4 5 6}, :yx-diag #{-3 -2 0 1 2 3}, :queen-locs #{[1 2]}}
         {:size 4, :rows #{0 1 3}, :cols #{0 1 3}, :xy-diag #{0 1 2 3 5 6}, :yx-diag #{-3 -2 -1 1 2 3}, :queen-locs #{[2 2]}}
         {:size 4, :rows #{0 1 3}, :cols #{0 1 2}, :xy-diag #{0 1 2 3 4 6}, :yx-diag #{-3 -2 -1 0 2 3}, :queen-locs #{[3 2]}}
         {:size 4, :rows #{0 1 2}, :cols #{1 2 3}, :xy-diag #{0 1 2 4 5 6}, :yx-diag #{-2 -1 0 1 2 3}, :queen-locs #{[0 3]}}
         {:size 4, :rows #{0 1 2}, :cols #{0 2 3}, :xy-diag #{0 1 2 3 5 6}, :yx-diag #{-3 -1 0 1 2 3}, :queen-locs #{[1 3]}}
         {:size 4, :rows #{0 1 2}, :cols #{0 1 3}, :xy-diag #{0 1 2 3 4 6}, :yx-diag #{-3 -2 0 1 2 3}, :queen-locs #{[2 3]}}
         {:size 4, :rows #{0 1 2}, :cols #{0 1 2}, :xy-diag #{0 1 2 3 4 5}, :yx-diag #{-3 -2 -1 1 2 3}, :queen-locs #{[3 3]}})


  (-> (new-board 6)
      (place-queen [2 3])
      (place-queen [0 0])
      (place-queen [5 4])
      ((juxt trial-boards
             #(drawn-board % (safe-spaces %)))))
  #_=> [({:size       6,
          :rows       #{2 5},
          :cols       #{1 4},
          :xy-diag    #{1 2 3 6 7 8 10},
          :yx-diag    #{-5 -4 -3 -2 3 4 5},
          :queen-locs #{[0 0] [2 3] [5 4] [3 1]}}
         {:size       6,
          :rows       #{1 5},
          :cols       #{1 3},
          :xy-diag    #{1 2 3 4 7 8 10},
          :yx-diag    #{-5 -4 -3 -2 3 4 5},
          :queen-locs #{[0 0] [2 3] [5 4] [4 2]}}
         {:size       6,
          :rows       #{1 2},
          :cols       #{3 4},
          :xy-diag    #{1 2 3 4 7 8 10},
          :yx-diag    #{-5 -3 -2 2 3 4 5},
          :queen-locs #{[0 0] [2 3] [5 4] [1 5]}}
         {:size       6,
          :rows       #{1 2},
          :cols       #{1 4},
          :xy-diag    #{1 2 3 4 6 7 10},
          :yx-diag    #{-5 -4 -3 2 3 4 5},
          :queen-locs #{[0 0] [2 3] [5 4] [3 5]}})
        [[Q . . . . .]
         [. . . * . .]
         [. . . . * .]
         [. . Q . . .]
         [. . . . . Q]
         [. * . * . .]]]

  (->> (new-board 4)
       dfs-nqueens-clean
       (mapv (juxt identity
                   drawn-board)))
  #_=> [[{:size 4, :rows #{}, :cols #{}, :xy-diag #{0 3 6}, :yx-diag #{-3 0 3}, :queen-locs #{[1 0] [2 3] [0 2] [3 1]}}
         [[. Q . .]
          [. . . Q]
          [Q . . .]
          [. . Q .]]]
        [{:size 4, :rows #{}, :cols #{}, :xy-diag #{0 3 6}, :yx-diag #{-3 0 3}, :queen-locs #{[1 3] [2 0] [3 2] [0 1]}}
         [[. . Q .]
          [Q . . .]
          [. . . Q]
          [. Q . .]]]]


  (->> (new-board 6)
       dfs-nqueens-clean
       (mapv (juxt identity
                   drawn-board)))
  #_=> [[{:size       6,
          :rows       #{},
          :cols       #{},
          :xy-diag    #{0 2 5 8 10},
          :yx-diag    #{-5 -4 0 4 5},
          :queen-locs #{[1 0] [5 2] [0 3] [2 4] [4 5] [3 1]}}
         [[. Q . . . .]
          [. . . Q . .]
          [. . . . . Q]
          [Q . . . . .]
          [. . Q . . .]
          [. . . . Q .]]]
        [{:size       6,
          :rows       #{},
          :cols       #{},
          :xy-diag    #{0 1 5 9 10},
          :yx-diag    #{-5 -3 0 3 5},
          :queen-locs #{[4 3] [5 1] [2 0] [0 4] [1 2] [3 5]}}
         [[. . Q . . .]
          [. . . . . Q]
          [. Q . . . .]
          [. . . . Q .]
          [Q . . . . .]
          [. . . Q . .]]]
        [{:size       6,
          :rows       #{},
          :cols       #{},
          :xy-diag    #{0 2 5 8 10},
          :yx-diag    #{-5 -4 0 4 5},
          :queen-locs #{[2 5] [5 4] [4 2] [3 0] [1 3] [0 1]}}
         [[. . . Q . .]
          [Q . . . . .]
          [. . . . Q .]
          [. Q . . . .]
          [. . . . . Q]
          [. . Q . . .]]]
        [{:size       6,
          :rows       #{},
          :cols       #{},
          :xy-diag    #{0 1 5 9 10},
          :yx-diag    #{-5 -3 0 3 5},
          :queen-locs #{[3 4] [5 3] [1 5] [0 2] [2 1] [4 0]}}
         [[. . . . Q .]
          [. . Q . . .]
          [Q . . . . .]
          [. . . . . Q]
          [. . . Q . .]
          [. Q . . . .]]]]

  (time
    (->> (new-board 8)
         dfs-nqueens-clean
         ((juxt #(->> (take-last 4 %)
                      (mapv (juxt identity drawn-board)))
                count))))
  ;; "Elapsed time: 51525.088584 msecs"
  #_=> [[[{:size       8,
           :rows       #{},
           :cols       #{},
           :xy-diag    #{0 1 4 6 11 13 14},
           :yx-diag    #{-7 -6 -5 3 4 5 6},
           :queen-locs #{[1 1] [5 7] [6 4] [0 3] [4 5] [7 0] [2 6] [3 2]}}
          [[. . . . . . . Q]
           [. Q . . . . . .]
           [. . . Q . . . .]
           [Q . . . . . . .]
           [. . . . . . Q .]
           [. . . . Q . . .]
           [. . Q . . . . .]
           [. . . . . Q . .]]]
         [{:size       8,
           :rows       #{},
           :cols       #{},
           :xy-diag    #{0 1 3 8 10 13 14},
           :yx-diag    #{-7 -6 -5 3 4 5 6},
           :queen-locs #{[2 3] [1 1] [4 2] [6 5] [5 7] [3 6] [7 0] [0 4]}}
          [[. . . . . . . Q]
           [. Q . . . . . .]
           [. . . . Q . . .]
           [. . Q . . . . .]
           [Q . . . . . . .]
           [. . . . . . Q .]
           [. . . Q . . . .]
           [. . . . . Q . .]]]
         [{:size       8,
           :rows       #{},
           :cols       #{},
           :xy-diag    #{0 1 4 6 11 13 14},
           :yx-diag    #{-7 -6 -5 3 4 5 6},
           :queen-locs #{[6 6] [5 3] [1 4] [4 5] [7 0] [0 2] [2 1] [3 7]}}
          [[. . . . . . . Q]
           [. . Q . . . . .]
           [Q . . . . . . .]
           [. . . . . Q . .]
           [. Q . . . . . .]
           [. . . . Q . . .]
           [. . . . . . Q .]
           [. . . Q . . . .]]]
         [{:size       8,
           :rows       #{},
           :cols       #{},
           :xy-diag    #{0 1 3 8 10 13 14},
           :yx-diag    #{-7 -6 -5 3 4 5 6},
           :queen-locs #{[2 3] [5 4] [6 6] [4 7] [1 5] [7 0] [0 2] [3 1]}}
          [[. . . . . . . Q]
           [. . . Q . . . .]
           [Q . . . . . . .]
           [. . Q . . . . .]
           [. . . . . Q . .]
           [. Q . . . . . .]
           [. . . . . . Q .]
           [. . . . Q . . .]]]]
        92]

  (time
    (loop [x 0]
      (if (> x (* 1000 1000))
        0
        (recur (inc x)))))


  (let [java-arr (make-array Integer/TYPE 4)]
    (loop )
    )

  (make-array)

  (time
    (for [x (range 0 (* 1000 1000))]
      )

    )



  )