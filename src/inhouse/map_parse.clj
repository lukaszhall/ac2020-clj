(ns inhouse.map-parse
  (:require [clojure.string :as str]))

;Write a function to parse any string of the following format into a map:
;
;status:321;someText:a text thing;startIndex:1234
;
; Focus should be on code clarity above all else
; Make sure to handle edge cases appropriately
;    Null input/processing
;    elements without pairs (no colon :), etc



;; Sample data

(def sample-strings ["status:321;someText:a text thing;startIndex:1234"
                     "status:321;asdfasdfads;"
                     nil])



(defn parse-msg
  [msg-str]
  (->> ((fnil str/split "") msg-str #";")
       (map #(str/split % #":"))
       (filter #(= (count %) 2))
       (into {})))


;; Not related to above solution - for discussion
(defmulti commit-status
          (fn [_path _header body] (:commit-status body)))

(defmethod commit-status "SUCCESS" [path header body]
  (complete-file! path header))

(defmethod commit-status "FAILURE" [path header body]
  (error-processing-file path body))

(defmethod commit-status :default [{:as path :keys [resource-type node file-name]}
                                   header
                                   {:as body :keys [commit-status]}]
  (success-response (str "Error: Invalid status received for Type: " resource-type
                         " Node: " node
                         " File-name: " file-name
                         " Body: " body)))



(cond (:commit-status body)
      "SUCCCESS" (complete-file! path header)
      "FAILURE" (error-processing-file path body)
      :else (success-response (str "Error: Invalid status received for Type: " resource-type
                                   " Node: " node
                                   " File-name: " file-name
                                   " Body: " body)))


(comment

  (-> (nth sample-strings 0)
      (parse-msg))
  #_=> {"status" "321", "someText" "a text thing", "startIndex" "1234"}


  (map parse-msg sample-strings)
  #_=> ({"status" "321", "someText" "a text thing", "startIndex" "1234"} {"status" "321"} {})

  )