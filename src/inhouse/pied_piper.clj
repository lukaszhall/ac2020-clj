(ns inhouse.pied-piper
  (:require [clojure.string :as str]))


;The Pied Piper has been enlisted to play his magical tune and coax all the rats out of town.
;But some of the rats are deaf and are going the wrong way!
;How many deaf rats are there?
;
;Legend
;
;P = The Pied Piper
;O~ = Rat going left
;~O = Rat going right
;Example
;
;ex1 ~O~O~O~O P has 0 deaf rats
;ex2 P O~ O~ ~O O~ has 1 deaf rat
;ex3 ~O~O~O~OP~O~OO~ has 2 deaf rats


(def input-samples ["~O~O~O~O P"
                    "P O~ O~ ~O O~"
                    "~O~O~O~OP~O~OO~"])

(defn str->kw
  [rats]
  (let [rat-s (partition 2 rats)
        kw-fn (fn [[left-char right-char]]
                (case [left-char right-char]
                  [\O \~] :l
                  [\~ \O] :r
                  :unknown))]
    (mapv kw-fn rat-s)))


(defn decode
  [rat-str]
  (let [[left-rats right-rats] (-> rat-str
                                   (str/replace #"\s*" "")
                                   (str/split #"P"))]
    [(str->kw left-rats)
     (str->kw right-rats)]))


(defn deaf-rats
  [[left-rats right-rats]]
  (let [[deaf-left-rats deaf-right-rats] [(filterv #(= :l %) left-rats)
                                          (filterv #(= :r %) right-rats)]]
    [deaf-left-rats deaf-right-rats]))


(defn deaf-rats
  [rat-dir-s piper-loc]
  (if (= :r piper-loc)
    (filter #(= :r %) rat-dir-s)
    (filter #(= :l %) rat-dir-s)))

(defn deaf-rats
  [rats-loc rat-dir-s]
  (let [deaf-dir (case rats-loc
                   :r :r
                   :l :l)]
    (filter #(= deaf-dir %) rat-dir-s)))


#_(filter (partial deaf-rats :l) left-rats)

(comment

  input-samples
  #_=> ["~O~O~O~O P"
        "P O~ O~ ~O O~"
        "~O~O~O~OP~O~OO~"]

  ;; Decode single string to kws
  (str->kw "~O~OO~")
  #_=> [:r :r :l]

  ;; Bifurcate and decode input string
  (map decode input-samples)
  #_=> ([[:r :r :r :r] []]
        [[] [:l :l :r :l]]
        [[:r :r :r :r] [:r :r :l]])

  ;; Filter for deaf rats
  (deaf-rats [[:r :l :r :r] [:r :r :l]])
  #_=> [[:l] [:r :r]]


  ;; Problem solution
  (->> input-samples
       (map decode)
       (map deaf-rats)
       (map (fn [x] (->> (map count x)
                         (reduce +)))))
  )