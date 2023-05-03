(ns ac2022-clj.day7
  (:require [ac2022-clj.util :as util]
            [clojure.string :as str]))

(defrecord FSNode [dir? name size children])

(defn m->FSNode
  [{:keys [dir? name size children]
    :or   {size     0
           children '()}}]
  (->FSNode type name size children))

(defn str-tokens
  "tokenize/split a cli string by space"
  [cli-str]
  (str/split cli-str #" "))

(defmulti ls-str-s->FSNode
          (fn [[first-str & _remaining :as _ls-str-s]]
            (println "Finding cond...")
            (cond
              (= "dir" first-str)
              :dir

              (number? (parse-long first-str))
              :file)))

(defmethod ls-str-s->FSNode :dir
  [[_dir dir-name]]
  (m->FSNode {dir?  true
              :name dir-name}))

(defmethod ls-str-s->FSNode :file
  [[size-str file-name]]
  (m->FSNode {:dir? false
              :name file-name
              :size (parse-long size-str)}))


(defn parse-ls
  [ls-s]

  )



(defn parse-history
  [cli-history]
  (let [root-node (m->FSNode {:type :dir
                              :name "/"})]
    (loop [cur-node    root-node
           rem-history cli-history]

      ))
  )




























(comment

  (do (def sample-cli-history (util/file-as-seq "ac2022/day7/input_sample.txt"))
      sample-cli-history)
  #_=> ["$ cd /"
        "$ ls"
        "dir a"
        "14848514 b.txt"
        "8504156 c.dat"
        "dir d"
        "$ cd a",,,,,]

  (str-tokens "dir a")
  #_=> ["dir" "a"]

  (mapv ls-str-s->FSNode [["dir" "a"]
                          ["14848514" "b.txt"]])
  #_=> [#ac2022_clj.day7.FSNode{:dir? true, :name "a", :size 0, :children ()}
        #ac2022_clj.day7.FSNode{:dir? false, :name "b.txt", :size 14848514, :children ()}]

  )