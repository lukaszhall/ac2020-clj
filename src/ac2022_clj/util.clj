(ns ac2022-clj.util
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn spy
  "Print and passthrough value"
  ([value]
   (println value)
   value)
  ([string value]
   (println (str string value))
   value))


(defn file-as-seq
  "Return "
  [filename]
  (-> (io/resource filename)
      slurp
      (string/split #"\n")))