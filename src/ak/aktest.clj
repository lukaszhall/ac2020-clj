(ns ak.aktest)


;; Binding

(def a 2)


;; Functions

(+ 1 2 3)

(defn my-fun
  [x]
  x)



;; Data Structures
'(1 2 3)

[1 2 3]

(def dancers
  {:adam   {:age    27
            :lname  "Kim"
            :dances [:LindyHop :Balboa]}
   :lukasz {:age    38
            :lname  "Hall"
            :dances [:Balboa :Tango]}})

(def dancer-metadata1
  {:favorite-dance {:path [:dances]
                    :fn   first}})

#{1 2 3}

;;---------------------

(defn favorite-dance
  [dancer]
  #_(let [dances (:dances dancer)
          a      1
          b      2]
      (first dances))

  (-> (:dances dancer)
      first
      name)

  )

(comment

  ;;
  (favorite-dance (:adam dancers))

  (map inc [1 2 3])
  #_=> '(2 3 4)

  (map favorite-dance (vals dancers))


  ({:adam   27
    :lukasz 38} :lukasz))




(defn favorite-dance
  [dancer dancer-metadata]
  (let [dances-path    (-> dancer-metadata :favorite-dance :path)
        fav-dance-fn   (-> dancer-metadata :favorite-dance :fn)
        dances         (get-in dancer dances-path)
        favorite-dance (fav-dance-fn dances)
        ]

    favorite-dance
    #_(first dances)

    ))

(favorite-dance (:adam dancers)
                dancer-metadata1)

(->>
  (map (fn [[dancer-name dancer-profile]]
         {dancer-name (favorite-dance dancer-profile dancer-metadata1)})
       dancers)
  (apply merge))


(map identity dancers)
