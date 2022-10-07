(ns inhouse.flip-big-strings
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


; Write a function that takes in a string of one or more words and returns the same string,
; but with all words with five letters or more reversed. Strings passed in will consist of only letters and spaces.
; Be sure to keep the order of the words the same and only reverse the letters.
; ---------------
; Good luck, yppaH gnidoc!


(defn large-word?
  [word]
  (>= (count word)
      5))

(defn palindrome?
  [_word]
  false)

(defmulti xform-word
          #(cond
             (large-word? %) :large-word
             (palindrome? %) :palindrome
             :else :small-word))


(defmethod xform-word :large-word
  [word]
  (str/reverse word))


(defmethod xform-word :default
  [word]
  word)


(defn xform-words
  [word-string]
  (let [words          (str/split word-string #" ")
        xformed-words  (map xform-word words)
        xformed-string (str/join " " xformed-words)]
    xformed-string))



(comment

  (def sample-word-strings
    ["Good luck Happy coding"
     "Good luck yppaH gnidoc"])


  (mapv (juxt identity large-word?)
        ["1" "12" "123" "1234" "12345" "123456"])
  #_=> => [["1" false] ["12" false] ["123" false] ["1234" false] ["12345" true] ["123456" true]]



  (mapv (juxt identity xform-word)
        ["1" "12" "123" "1234" "12345" "123456"])
  #_=> [["1" "1"] ["12" "12"] ["123" "123"] ["1234" "1234"] ["12345" "54321"] ["123456" "654321"]]

  (mapv xform-words sample-word-strings)
  #_=> ["Good luck yppaH gnidoc" "Good luck Happy coding"]

  )