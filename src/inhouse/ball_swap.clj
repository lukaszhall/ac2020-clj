(ns inhouse.ball-swap)


;An illusionist is performing ball swaps.
;There are three cups on a table, at positions A, B, and C. At the start, there is a ball hidden under the cup at position B.
;Create a function that returns the letter position where the ball final position is once the swapping is finished.
;There will be several swaps perform, represented by two letters.
;For example, if I swap the cups at positions A and B, this can be represented as AB or BA.
;Examples
;swap-cups(["AB" "CA"]) ➞ "C"
;
;swap-cups(["AC" "CA", "CA", "AC"]) => "B"
;
;swap-cups(["BA" "AC", "CA", "BC"]) => "A"

(defn cups->set
  [cups-str]
  (->> (concat cups-str)
       (map #(Character/toString %))
       (map keyword)
       set))

(defn parse-cup-swaps
  [cup-swaps]
  (mapv cups->set cup-swaps))


(def starting-cup-paths
  [[:A] [:B] [:C]])

(defn next-path
  "Given a cup path, advance the path with the swap, else itself "
  [cup-path swap-set]
  (let [last-cup (last cup-path)
        next-cup (if (contains? swap-set last-cup)
                   (-> (disj swap-set last-cup)
                       first)
                   last-cup)]
    (conj cup-path next-cup)))

(defn next-paths
  [cup-paths swap-set]
  (mapv #(next-path % swap-set)
        cup-paths))

(defn full-paths
  ([swap-sets cup-paths]
   (reduce (fn [cup-paths swap-set]
             (next-paths cup-paths swap-set))
           cup-paths
           swap-sets))
  ([swap-sets]
   (full-paths swap-sets starting-cup-paths)))

;; Problem specification
(defn swap-cups
  [swaps]
  (->> swaps
       parse-cup-swaps
       full-paths
       (filter #(= :B (first %)))
       first
       last))



(comment

  (def sample-data
    [["AB" "CA"]
     ["AC" "CA" "CA" "AC"]
     ["BA" "AC" "CA" "BC"]])


  (cups->set "AB")
  #_=> #{:A :B}

  (parse-cup-swaps (first sample-data))
  #_=> [#{:A :B} #{:A :C}]

  starting-cup-paths
  #_=> [(:A) (:B) (:C)]

  (next-path [:A] #{:B :A})
  #_=> [:A :B]


  (mapv (partial next-path [:A :B])
        [#{:A :B}
         #{:A :C}
         #{:B :C}])
  #_=> [[:A :B :A]
        [:A :B :B]
        [:A :B :C]]

  (next-paths starting-cup-paths
              #{:A :B})
  #_=> [[:A :B] [:B :A] [:C :C]]

  (full-paths [#{:A :B} #{:A :C}])
  #_=> [[:A :B :B] [:B :A :C] [:C :C :A]]

  (->> sample-data
       (mapv parse-cup-swaps)
       (mapv full-paths))
  #_=> [[[:B:A :B] [:B :A :C] [:C :C :A]]
        [[:A :C :A :C :A] [:B :B :B :B :B] [:C :A :C :A :C]]
        [[:A :B :B :B :C] [:B :A :C :A :A] [:C :C :A :C :B]]]

  ;; Problem specification

  (->> ["BA" "AC", "CA", "BC"]
       parse-cup-swaps
       full-paths
       (filter #(= :B (first %)))
       first
       last)
  #_=> :A

  (->> sample-data
       ((juxt identity (partial map swap-cups)))
       (apply zipmap))
  #_=> {["AB" "CA"]           :C,
        ["AC" "CA" "CA" "AC"] :B,
        ["BA" "AC" "CA" "BC"] :A}

  )

