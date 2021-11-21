(ns ac2020-clj.day7-corelogic
  (:refer-clojure :exclude [==])
  (:require [ac2020-clj.util :as util]
            [ac2020-clj.day7 :as day7]

            [clojure.core.logic :as cl]
            [clojure.core.logic.pldb :as cldb]))

;; Graph Building
(defn bagline-s->weighted-graph
  [bagline-s]
  (->> bagline-s
       (map day7/parse-bagline)
       (apply merge)))


(defn weighted-graph->graph
  [weighted-graph]
  (->> weighted-graph
       (map (fn [[name m]]
              {name (or (set (keys m))
                        #{})}))
       (apply merge)))

;; Part 1

#_(cldb/db-rel )


(cl/run* [q r]
         (cl/membero q [1 2 3])
         (cl/membero r [1 2 3])
         )

(cl/fresh [q r]
          (cl/membero q [1 2 3])
          (cl/membero r [1 2 3])
          )




;; Part 1 - BFS
(def map-entry-key first)
(defn containers-of
  [graph contained-bag]
  (->> graph
       (filter (fn [[_container contained]]
                 (contains? contained contained-bag)))
       (map map-entry-key)))

(defn containers-of-bag
  "BFS search"
  [graph bag]
  (loop [found-containers #{}
         process-list     [bag]]
    (let [parent-containers (mapcat (partial containers-of graph)
                                    process-list)]
      (if (empty? parent-containers)
        (disj found-containers bag)
        (recur (into found-containers process-list)
               parent-containers)))))

; Part 2 -
;     _    _    _    _    _
;  __( )__( )__( )__( )__( )__
; '--. .--. .--. .--. .--. .--'
;   / _ \/ _ \/ _ \/ _ \/ _ \
;   (/ \)(/ \)(/ \)(/ \)(/ \)
(defn babushka-bag
  [weighted-graph bag]
  (let [inner-bags (->> (get weighted-graph bag)
                        (map (fn [[inner-bag n]]
                               (into [] (repeat n (babushka-bag weighted-graph inner-bag))))))]
    (into [bag] inner-bags)))


(defn bags-in-container->count
  [weighted-graph bag]
  (let [inner-bag-freqs (-> (babushka-bag weighted-graph bag)
                            flatten
                            frequencies
                            (dissoc bag))]

    inner-bag-freqs))

;; Journal
(comment


  (->> (util/file-as-seq "day7/input_sample.txt")
       bagline-s->weighted-graph)
  #_=> {"muted yellow" {"shiny gold" 2, "faded blue" 9},
        "light red"    {"bright white" 1, "muted yellow" 2},
        "dotted black" nil,
        "dark orange"  {"bright white" 3, "muted yellow" 4},
        "bright white" {"shiny gold" 1},
        "shiny gold"   {"dark olive" 1, "vibrant plum" 2},
        "faded blue"   nil,
        "vibrant plum" {"faded blue" 5, "dotted black" 6},
        "dark olive"   {"faded blue" 3, "dotted black" 4}}


  (->> (util/file-as-seq "day7/input_sample.txt")
       bagline-s->weighted-graph
       weighted-graph->graph)
  #_=> {"muted yellow" #{"shiny gold" "faded blue"},
        "light red"    #{"muted yellow" "bright white"},
        "dotted black" #{},
        "dark orange"  #{"muted yellow" "bright white"},
        "bright white" #{"shiny gold"},
        "shiny gold"   #{"vibrant plum" "dark olive"},
        "faded blue"   #{},
        "vibrant plum" #{"dotted black" "faded blue"},
        "dark olive"   #{"dotted black" "faded blue"}}

  (-> (->> (util/file-as-seq "day7/input_sample.txt")
           bagline-s->weighted-graph
           weighted-graph->graph)
      (containers-of-bag "dotted black"))
  #_=> #{"muted yellow" "bright white" "shiny gold" "vibrant plum" "dark olive"}

  ;; Part 1
  (-> (->> (util/file-as-seq "day7/input.txt")
           bagline-s->weighted-graph
           weighted-graph->graph)
      (containers-of-bag "shiny gold")
      count)
  #_=> 177

  ;; Part 2
  (-> (->> (util/file-as-seq "day7/input_sample.txt")
           bagline-s->weighted-graph)
      (babushka-bag "muted yellow"))
  #_=> ["muted yellow"
        [["shiny gold"
          [["dark olive"
            [["faded blue"] ["faded blue"] ["faded blue"]]
            [["dotted black"] ["dotted black"] ["dotted black"] ["dotted black"]]]]
          [["vibrant plum"
            [["faded blue"] ["faded blue"] ["faded blue"] ["faded blue"] ["faded blue"]]
            [["dotted black"] ["dotted black"] ["dotted black"] ["dotted black"] ["dotted black"] ["dotted black"]]]
           ["vibrant plum"
            [["faded blue"] ["faded blue"] ["faded blue"] ["faded blue"] ["faded blue"]]
            [["dotted black"] ["dotted black"] ["dotted black"] ["dotted black"] ["dotted black"] ["dotted black"]]]]]
         ["shiny gold"
          [["dark olive"
            [["faded blue"] ["faded blue"] ["faded blue"]]
            [["dotted black"] ["dotted black"] ["dotted black"] ["dotted black"]]]]
          [["vibrant plum"
            [["faded blue"] ["faded blue"] ["faded blue"] ["faded blue"] ["faded blue"]]
            [["dotted black"] ["dotted black"] ["dotted black"] ["dotted black"] ["dotted black"] ["dotted black"]]]
           ["vibrant plum"
            [["faded blue"] ["faded blue"] ["faded blue"] ["faded blue"] ["faded blue"]]
            [["dotted black"] ["dotted black"] ["dotted black"] ["dotted black"] ["dotted black"] ["dotted black"]]]]]]
        [["faded blue"]
         ["faded blue"]
         ["faded blue"]
         ["faded blue"]
         ["faded blue"]
         ["faded blue"]
         ["faded blue"]
         ["faded blue"]
         ["faded blue"]]]

  (-> (->> (util/file-as-seq "day7/input_sample.txt")
           bagline-s->weighted-graph)
      (babushka-bag "muted yellow")
      flatten
      frequencies)
  #_=> {"muted yellow" 1, "shiny gold" 2, "dark olive" 2, "faded blue" 35, "dotted black" 32, "vibrant plum" 4}


  (->> (-> (->> (util/file-as-seq "day7/input_sample.txt")
                bagline-s->weighted-graph)
           (bags-in-container->count "muted yellow"))
       (reduce (fn [s [_ n]] (+ s n))
               0))
  #_=> 75



  (let [graph     (->> (util/file-as-seq "day7/input.txt")
                       bagline-s->weighted-graph)
        bag-freqs (bags-in-container->count graph "shiny gold")
        bag-count (reduce (fn [s [_ n]]
                            (+ s n))
                          0
                          bag-freqs)]
    bag-count))
#_=> 34988

