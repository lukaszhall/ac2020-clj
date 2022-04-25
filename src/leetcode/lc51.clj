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




(defn new-board
  "Create a board of nxn size with sets of all free :rows :cols :xy-diags and :yx-diags"
  [n]
  {:rows    (apply sorted-set (for [x (range n)] x))
   :cols    (apply sorted-set (for [x (range n)] x))
   :xy-diag (apply sorted-set (for [x (range (inc (xy-diag [(dec n) (dec n)])))] x))
   :yx-diag (apply sorted-set (for [x (range (yx-diag [0 (dec n)])
                                             (inc (yx-diag [(dec n) 0])))] x))})


(defn- xy-diag
  "compute xy diagonal from x and y coords"
  [[x y]]
  (+ y x))

(defn- yx-diag
  [[x y]]
  (- x y))

(defn safe-space?
  "true iff placing a queen is non-attacking on the board. i.e., does the queen
  intersect with any used rows/cols/diags"
  [[x y] board]
  (let [row-free (any? ((-> board :rows) y))
        col-free (any? ((-> board :cols) x))
        xy-free  (any? ((-> board :xy-diag) (xy-diag [x y])))
        yx-free  (any? ((-> board :yx-diag) (yx-diag [x y])))]
    (println {:row-free row-free
              :col-free col-free
              :xy-free  xy-free
              :yx-free  yx-free})
    (and row-free
         col-free
         xy-free
         yx-free)))

(defn place-queen
  "Remove free spaces from "
  [[x y] board]
  (let [re-assoc ])
  (-> board
      (fn [b] (assoc b :rows (disj (:rows board) y)))
      )
  )

(defn dfs-nqueens
  "dfs search with constraint prop "
  [n board]
  (for [x (range n)]
    (for [y (range n)]

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

  ()

  )