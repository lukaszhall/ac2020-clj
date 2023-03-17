(ns ac2022-clj.day5
  (:require [ac2022-clj.util :as util]
            [clojure.string :as str]))

;; Parsing

(defn parse-crate-str
  "[Z] -> Z"
  [create-str]
  (re-find #"[([A-Z])]" create-str))

(defn parse-stack-line
  "  '    [D]'  =>  [nil 'D'] "
  [horizontal-stack-line]
  (->> (partition-all 4 horizontal-stack-line)
       (mapv (partial apply str))
       (mapv parse-crate-str)))

(defn padded-vector
  "Pad a vector to specified size"
  [target-size base-vec]
  (let [size-delta (max 0
                        (- target-size
                           (count base-vec)))
        new-vec    (concat base-vec
                           (take size-delta (repeat nil)))]
    (vec new-vec)))

(defn transpose
  "Transpose 2D vector along diagonal axis"
  [matrix]
  (let [width             (->> (map count matrix)
                               (apply max))
        padded-matrix     (mapv (partial padded-vector width)
                                matrix)
        transposed-matrix (apply mapv list padded-matrix)
        flipped-matrix    (mapv reverse transposed-matrix)
        vectorized-matrix (mapv vec flipped-matrix)]
    vectorized-matrix))

(defn parse-stack-diagram
  [stack-line-s]
  (let [horizontal-stacks (->> stack-line-s
                               (drop-last 1)
                               (mapv parse-stack-line))
        vertical-stacks   (transpose horizontal-stacks)
        cleaned-v-stacks  (mapv #(filterv some? %) vertical-stacks)]
    cleaned-v-stacks))

(defn parse-procedure
  [instr-str]
  (let [[_ cnt-str from-str to-str] (re-matches #"move (\d+) from (\d+) to (\d+)" instr-str)
        [cnt from to] (mapv #(Integer/parseInt %)
                            [cnt-str from-str to-str])]
    {:cnt  cnt
     :from from
     :to   to}))

(defn parse-stack-and-procedure
  [str-s]
  (let [[stack-diag _ procedure-str-s] (partition-by empty? str-s)
        stack       (parse-stack-diagram stack-diag)
        procedure-s (mapv parse-procedure procedure-str-s)]
    {:stack      stack
     :procedures procedure-s}))


;; Execute Procedure

(defn move-blocks
  "The 'crane-fn'.
  Moves 'cnt' blocks one-by-one 'from'  1 indexed column
  'to' another 1 indexed column in the stack"
  [{:keys [from to cnt] :as procedure} stack]
  (if (= cnt 0)
    stack
    (let [from-idx  (dec from)
          to-idx    (dec to)
          box       (-> (get stack from-idx)
                        (last))
          from-col  (-> (get stack from-idx)
                        (pop))
          to-col    (-> (get stack to-idx)
                        (conj box))
          new-stack (-> stack
                        (assoc from-idx from-col)
                        (assoc to-idx to-col))]
      (move-blocks {:from from
                    :to   to
                    :cnt  (dec cnt)}
                   new-stack))))


(defn iterate-procedures
  "Applies the crane-fn to the stack sequentially for each procedure, building
  a history of applications"
  [crane-fn {:keys [stack procedures]}]
  (loop [rem-procedure-s procedures
         current-stack   stack
         history         (list {:stack      stack
                                :procedures procedures})]
    (if (= 0 (count rem-procedure-s))
      history
      (let [next-procedure (first rem-procedure-s)
            next-stack     (crane-fn next-procedure current-stack)
            history-elmnt  {:stack      next-stack
                            :procedures (rest rem-procedure-s)}]
        (recur (rest rem-procedure-s)
               next-stack
               (conj history history-elmnt))))))


(defn move-blocks-atomically
  "The 'crane-fn'.
  Moves 'cnt' blocks all at once 'from'  1 indexed column
  'to' another 1 indexed column in the stack"
  [{:keys [from to cnt] :as procedure} stack]
  (if (= cnt 0)
    stack
    (let [from-idx  (dec from)
          to-idx    (dec to)
          boxes     (->> (get stack from-idx)
                         (take-last cnt))
          from-col  (->> (get stack from-idx)
                         (drop-last cnt)
                         (vec))
          to-col    (-> (get stack to-idx)
                        (concat boxes)
                        (vec))
          new-stack (-> stack
                        (assoc from-idx from-col)
                        (assoc to-idx to-col))]
      new-stack)))










(comment

  (do (def input-sample (util/file-as-seq "ac2022/day5/input_sample.txt"))
      input-sample)
  #_=> ["    [D]"
        "[N] [C]"
        "[Z] [M] [P]"
        " 1   2   3"
        ""
        "move 1 from 2 to 1"
        "move 3 from 1 to 3"
        "move 2 from 2 to 1"
        "move 1 from 1 to 2"]

  (parse-procedure "move 1 from 2 to 1")
  #_=> {:count "1", :from "2", :to "1"}

  (mapv parse-crate-str ["[N] " "[C]" ""])
  #_=> ["N" "C" nil]

  (mapv parse-stack-line ["    [D]"
                          "[N] [C]"])
  #_=> [[nil "D"] ["N" "C"]]

  (mapv (partial padded-vector 4) [[1 2 3]
                                   [1 2 3 4]
                                   [1 2 3 4 5]
                                   []])
  #_=> [[1 2 3 nil] [1 2 3 4] [1 2 3 4 5] [nil nil nil nil]]


  (transpose [[1 2 3]
              [4 5 6 7]
              [8 9 9 10]])
  #_=> [[8 4 1]
        [9 5 2]
        [9 6 3]
        [10 7 nil]]


  (->> (parse-stack-diagram ["    [D]"
                             "[N] [C]"
                             "[Z] [M] [P]"
                             " 1   2   3"])
       (mapv #(filterv some? %)))
  #_=> [["Z" "N"] ["M" "C" "D"] ["P"]]


  (parse-stack-and-procedure input-sample)
  #_=> {:stack      [["Z" "N"] ["M" "C" "D"] ["P"]],
        :procedures [{:cnt 1, :from 2, :to 1}
                     {:cnt 3, :from 1, :to 3}
                     {:cnt 2, :from 2, :to 1}
                     {:cnt 1, :from 1, :to 2}]}


  ;; Executing Procedures
  (move-blocks {:cnt 2, :from 2, :to 1}
               [["Z" "N"]
                ["M" "C" "D"]
                ["P"]])
  #_=> [["Z" "N" "D" "C"]
        ["M"]
        ["P"]]

  (->> input-sample
       parse-stack-and-procedure
       ((partial iterate-procedures move-blocks)))
  #_=> '({:stack [["C"] ["M"] ["P" "D" "N" "Z"]], :procedures ()}
         {:stack [["C" "M"] [] ["P" "D" "N" "Z"]], :procedures ({:cnt 1, :from 1, :to 2})}
         {:stack [[] ["M" "C"] ["P" "D" "N" "Z"]], :procedures ({:cnt 2, :from 2, :to 1} {:cnt 1, :from 1, :to 2})}
         {:stack [["Z" "N" "D"] ["M" "C"] ["P"]], :procedures ({:cnt 3, :from 1, :to 3} {:cnt 2, :from 2, :to 1} {:cnt 1, :from 1, :to 2})}
         {:stack [["Z" "N"] ["M" "C" "D"] ["P"]], :procedures [{:cnt 1, :from 2, :to 1} {:cnt 3, :from 1, :to 3} {:cnt 2, :from 2, :to 1} {:cnt 1, :from 1, :to 2}]})

  ;; Part 1
  (->> (util/file-as-seq "ac2022/day5/input.txt")
       parse-stack-and-procedure
       ((partial iterate-procedures move-blocks))
       first
       :stack
       (map last)
       (apply str))
  #_=> "NTWZZWHFV"

  (->> (take-last 2 [1 2 3 4])
       (concat [0 0 0])
       (vec))

  (drop-last 2 [1 2 3 4])

  ;; Part 2
  (move-blocks-atomically {:cnt 2, :from 2, :to 1}
                          [["Z" "N"]
                           ["M" "C" "D"]
                           ["P"]])
  #_=> [["Z" "N" "C" "D"] ["M"] ["P"]]

  (->> input-sample
       parse-stack-and-procedure
       ((partial iterate-procedures move-blocks-atomically)))
  #_=> '({:stack [["M"] ["C"] ["P" "Z" "N" "D"]], :procedures ()}
         {:stack [["M" "C"] [] ["P" "Z" "N" "D"]], :procedures ({:cnt 1, :from 1, :to 2})}
         {:stack [[] ["M" "C"] ["P" "Z" "N" "D"]], :procedures ({:cnt 2, :from 2, :to 1} {:cnt 1, :from 1, :to 2})}
         {:stack [["Z" "N" "D"] ["M" "C"] ["P"]], :procedures ({:cnt 3, :from 1, :to 3} {:cnt 2, :from 2, :to 1} {:cnt 1, :from 1, :to 2})}
         {:stack [["Z" "N"] ["M" "C" "D"] ["P"]], :procedures [{:cnt 1, :from 2, :to 1} {:cnt 3, :from 1, :to 3} {:cnt 2, :from 2, :to 1} {:cnt 1, :from 1, :to 2}]})


  (->> (util/file-as-seq "ac2022/day5/input.txt")
       parse-stack-and-procedure
       ((partial iterate-procedures move-blocks-atomically))
       first
       :stack
       (map last)
       (apply str))
  #_=> "BRZGFVBTJ"

  )