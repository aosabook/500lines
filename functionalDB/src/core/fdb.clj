(ns core.fdb)

(defrecord Entity [e_id name attrs])
(defrecord Attr [name type value ts prev-ts])


(defn make-entity [name] (Entity.  (keyword name) name {}))
(defn make-attr[name val type]  (Attr. name type val -1 -1))
(defn add-attr[ ent attr] (assoc-in ent [:attrs (keyword (:name attr))] attr))
(defn next-ts [db] (inc (:curr-time db)))


(defn update-creation-ts [ent tsVal]
  (let [ks (keys (:attrs ent))
        vls (vals (:attrs ent))
        updatedAttrsVals (map #(assoc % :ts tsVal) vls)
        updatedAttrs (interleave ks updatedAttrsVals)
        ]
        (assoc ent :attrs updatedAttrs)

    )

  )

;when adding an entity, its attributes' timestamp would be set to be the current one
(defn add-entity[db ent]   (let [
                                 new-ts (next-ts db)
                                 ts-mp (last (:timestamped db))
                                 new-eavt (assoc (:EAVT ts-mp) (:e_id ent)  (update-creation-ts ent new-ts) )
                                 new-avet (:AVET ts-mp)
                                 new-timesampedVal (assoc ts-mp :AVET new-avet :EAVT new-eavt )]
                                (assoc db
                                  :timestamped  (conj (:timestamped db) new-timesampedVal)
                                  :curr-time new-ts )
                             ))

(defn make-db[]
  (atom {:timestamped [{:EAVT {} :AVET {}}]
                  :topId 0
                  :curr-time 0
                  }
    )
  )



(def db1 (make-db))

(def en1 (-> (make-entity "hotel")

          (add-attr (make-attr "room" 12 :number))
          (add-attr (make-attr "address" "where" :string)))

  )

(def en2 (-> (make-entity "book")

          (add-attr (make-attr "length" -1 :number))
          (add-attr (make-attr "auother" "jon" :string)))

  )

(swap! db1 add-entity en1)

(swap! db1 add-entity en2)






;(defn recent-ts-val [db](last (:timestamped db)))


;(defn update-ts-with-EAV [ts &[e a v] :as more]
;  (let[eavt (:EAVT ts)]
;    (assoc-in ts :EAVT e a v)
;    )
;  )

;(defn _add-timestamp[db & more]
;  (let [ ts {:EAVT (:EAVT  (recent-ts-val db))}]
;    (update-ts-with-EAV ts more)
;
;    (assoc db :timestamped (conj (:timestamped db) ts))
;    )
; )




   ;(add-attr @db1 "name" :string "Jim" )
 ;  (add-attr  @db1 "sur-name" :string "Doe" )
 ;  (add-attr  @db1 "age-name" :number 39 )


;db
;; (defn fact[n]
;;   (if (<= n 0) 1
;;     (* n (fact (dec n)))
;;     )

;;   )

;; (fact 1)

;(swap! db assoc :6 5
