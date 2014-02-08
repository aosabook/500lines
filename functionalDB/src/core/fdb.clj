(ns core.fdb)

(defrecord Entity [e_id name attrs])
(defrecord Attr [name type value ts prev-ts])


(defn make-entity [name] (Entity.  (keyword name) name {}))
(defn make-attr[name val type]  (Attr. name type val -1 -1))
(defn add-attr[ ent attr] (assoc-in ent [:attrs (keyword (:name attr))] attr))

;(defn update-creation-ts [ent ts]
;  (let ks [k])
;
;
;  )

(defn add-entity[db ent]   (let [
                                 ts-mp (last (:timestamped db))
                                 new-eavt (assoc (:EAVT ts-mp) (:e_id ent) ent)
                                 new-avet (:AVET ts-mp)
                                 new-ts (assoc ts-mp :AVET new-avet :EAVT new-eavt )]
                                (assoc db :timestamped  (conj (:timestamped db) new-ts))
                             ))

(defn make-db[]
  (atom {:timestamped [{:EAVT {} :AVET {}}]
                  :topId 0
                  :curr-time 0
                  }
    )
  )



(def db1 (make-db))


(def en (make-entity "hotel"))

(add-attr en (make-attr "room" 12 :number))

(swap! db1 add-entity en)







(defn recent-ts-val [db](last (:timestamped db)))


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



@db1

;(swap! db1 _add-timestamp)

;@db1




(defn next-id [db] (let [curr (@db :curr-time)]
                   (swap! db assoc :curr-time (inc curr))
                   curr))


(defn make-entity[db name]  (Entity. (db :curr-time) name))

(defn add-entity [db ent]
  (let [
       timestamped-vec (:timestamped db)
        eavt-map (:EAVT (recent-ts-val db))
        ]
    (assoc db :timestamped (conj timestamped-vec {:EAVT
                                 (assoc eavt-map (keyword (:name ent)) ent)}  ) )
    )
  )


 @db1
   (add-entity @db1
               (make-entity @db1 "person")
              )
   ;(add-attr @db1 "name" :string "Jim" )
 ;  (add-attr  @db1 "sur-name" :string "Doe" )
 ;  (add-attr  @db1 "age-name" :number 39 )








(next-id db1)
(next-id db1)
;db
(defn fact[n]
  (if (<= n 0) 1
    (* n (fact (dec n)))
    )

  )

(fact 1)

;(swap! db assoc :6 5
