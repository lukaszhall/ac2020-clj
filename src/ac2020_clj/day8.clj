(ns ac2020-clj.day8
  (:require [ac2020-clj.util :as util]
            [clojure.string :as str]))

;;sample inputs
(util/file-as-seq "day8/input_sample.txt")
#_=> ["nop +0" "acc +1" "jmp +4" "acc +3" "jmp -3" "acc -99" "acc +1" "jmp -4" "acc +6"]

(def instr-regex #"(acc|jmp|nop) ([+-]\d+)")


(defn parse-instr
  [instr-str]
  (let [[_ op arg] (re-matches instr-regex instr-str)]
    {:op  (keyword op)
     :arg (Integer/parseInt arg)}))

(defn parse-program
  [instr-str-s]
  (->> instr-str-s
       (mapv parse-instr)))

(defn exec-program
  "take an instruction seq and exec the program. Stop execution is instruction is re-encountered"
  [instr-s]
  (loop [pc            0
         acc           0
         visited-instr #{}]
    #_(println (str "pc " pc " acc " acc " vis: " visited-instr))
    (cond
      ;; End of Program
      (> pc (dec (count instr-s))) {:terminates? true
                                    :acc         acc
                                    :last-instr  pc}
      ;; Loop detected
      (contains? visited-instr pc) {:terminates? false
                                    :acc         acc
                                    :last-instr  pc}
      ;; Continue execution
      :else
      (let [{:keys [op arg]} (nth instr-s pc)]
        (case op
          :nop (recur (inc pc)
                      acc
                      (conj visited-instr pc))
          :acc (recur (inc pc)
                      (+ acc arg)
                      (conj visited-instr pc))
          :jmp (recur (+ pc arg)
                      acc
                      (conj visited-instr pc)))))))

;; Part 2
(defn jump|nop-indices
  "Return indices of all instruction address with either a nop or jump"
  [instr-s]
  (loop [pc      0
         indices []]
    #_(println (str "pc " pc " indices " indices))
    (cond
      (> pc (dec (count instr-s))) indices

      (or (= :jmp (:op (nth instr-s pc)))
          (= :nop (:op (nth instr-s pc)))) (recur (inc pc)
                                                  (conj indices pc))

      :else (recur (inc pc)
                   indices))))

(defn swap-nop<=>jump
  "Swap nop->jmp and jmp->nop at the specified program index"
  [instr-s idx]
  (let [instr         (nth instr-s idx)
        op            (:op instr)
        flipped-op    (get {:jmp :nop
                            :nop :jmp}
                           op)
        flipped-instr (assoc instr :op flipped-op)]
    (assoc instr-s idx flipped-instr)))

(defn find-terminating-program
  "Return the index of a jmp/nop that (when swapped) will result in a terminating program"
  [instr-s]
  (let [op-swap-indices (jump|nop-indices instr-s)]
    (loop [rem-op-swap-indices op-swap-indices]
      (let [swap-idx            (first rem-op-swap-indices)
            swapped-program     (swap-nop<=>jump instr-s swap-idx)
            program-exec-status (exec-program swapped-program)]
        (if (:terminates? program-exec-status)
          (assoc program-exec-status :swapped-idx swap-idx)
          (recur (rest rem-op-swap-indices)))))))



(comment

  (do (def sample-data (util/file-as-seq "day8/input_sample.txt"))
      sample-data)
  #_=> ["nop +0" "acc +1" "jmp +4" "acc +3" "jmp -3" "acc -99" "acc +1" "jmp -4" "acc +6"]

  (parse-instr "nop +0")
  #_=> {:op :nop, :arg 0}

  (parse-program sample-data)
  #_=> [{:op :nop, :arg 0}
        {:op :acc, :arg 1}
        {:op :jmp, :arg 4}
        {:op :acc, :arg 3}
        {:op :jmp, :arg -3}
        {:op :acc, :arg -99}
        {:op :acc, :arg 1}
        {:op :jmp, :arg -4}
        {:op :acc, :arg 6}]

  (exec-program (parse-program sample-data))
  #_=> {:terminates? false, :acc 5, :last-instr 1}

  ;; Answer part 1
  (->> (util/file-as-seq "day8/input.txt")
       parse-program
       exec-program)
  #_=> {:terminates? false, :acc 1915, :last-instr 4}


  ;; Part 2
  (->> (util/file-as-seq "day8/input_sample.txt")
       parse-program
       jump|nop-indices)
  #_=> [0 2 4 7]

  (-> (->> (util/file-as-seq "day8/input_sample.txt")
           parse-program)
      (swap-nop<=>jump 0))
  #_=> [{:op :jmp, :arg 0}
        {:op :acc, :arg 1}
        {:op :jmp, :arg 4}
        {:op :acc, :arg 3}
        {:op :jmp, :arg -3}
        {:op :acc, :arg -99}
        {:op :acc, :arg 1}
        {:op :jmp, :arg -4}
        {:op :acc, :arg 6}]

  (-> (->> (util/file-as-seq "day8/input_sample.txt")
           parse-program)
      (swap-nop<=>jump 2))
  #_=> [{:op :nop, :arg 0}
        {:op :acc, :arg 1}
        {:op :nop, :arg 4}
        {:op :acc, :arg 3}
        {:op :jmp, :arg -3}
        {:op :acc, :arg -99}
        {:op :acc, :arg 1}
        {:op :jmp, :arg -4}
        {:op :acc, :arg 6}]

  (->> (util/file-as-seq "day8/input_sample.txt")
       parse-program
       find-terminating-program)
  #_=> {:terminates? true, :acc 8, :last-instr 9, :swapped-idx 7}

    ;; Answer part 2
  (->> (util/file-as-seq "day8/input.txt")
       parse-program
       find-terminating-program)
  #_=> {:terminates? true, :acc 944, :last-instr 660, :swapped-idx 447}


  (.namedGroups (.pattern (.matcher #"<my-str>" "hi")))

  )