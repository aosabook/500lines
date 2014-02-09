(ns core.fdb)

(defrecord Entity [e_id name attrs])
(defrecord Attr [name type value ts prev-ts])


(defn make-entity [name] (Entity.  :no-id-yet name {}))
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

; avet is a map as follows:  {attrName -> {REFed-ent-id -> [REFing-elems-ids]}}

(defn add-ref-to-avet[ent avet attr]
  (let [
        attr-name (:name attr)
        attr-val (:value attr)
        attr-name-map (avet attr-name)
        attr-name-map (if attr-name-map attr-name-map {attr-name {attr-val []}})
        reffed-ent-vec (attr-name-map attr-val)
        reffed-ent-vec (if reffed-ent-vec reffed-ent-vec [])
        ]

  (assoc-in avet [attr-name attr-val] (conj reffed-ent-vec (:e_id ent))))

)

(defn update-avet[old-avet ent]
  (let [reffingAttrs (filter #(= :REF (:type %)) (vals (:attrs ent)))
        add-ref (partial add-ref-to-avet ent)]
       (reduce add-ref old-avet reffingAttrs))
  )

;when adding an entity, its attributes' timestamp would be set to be the current one
(defn add-entity[db ent]   (let [
                                 ent-id (inc (:topId db))
                                 new-ts (next-ts db)
                                 ts-mp (last (:timestamped db))
                                 fixed-ent (assoc ent :e_id ent-id)
                                 new-eavt (assoc (:EAVT ts-mp) ent-id  (update-creation-ts fixed-ent new-ts) )
                                 new-avet (update-avet (:AVET ts-mp) fixed-ent)
                                 new-indices (assoc ts-mp :AVET new-avet :EAVT new-eavt )]
                                (assoc db
                                  :timestamped  (conj (:timestamped db) new-indices)
                                  :curr-time new-ts
                                  :topId ent-id)
                             ))

(defn make-db[]
  (atom {:timestamped [{:EAVT {} ; all the entity info
                        :AVET {} ; for attrs who are REFs, we hold the back-pointing (from the REFFed entity to the REFing entities)
                        }]
                  :topId 0
                  :curr-time 0
                  }
    )
  )



(def db1 (make-db))

(def en1 (-> (make-entity "hotel")

          (add-attr (make-attr :hotel/room 12 :number))
          (add-attr (make-attr :hotel/address "where" :string)))

  )

(def en2 (-> (make-entity "book")

          (add-attr (make-attr :book/found-at 1 :REF))
          (add-attr (make-attr :book/author "jon" :string)))

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
