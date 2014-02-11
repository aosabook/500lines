(ns core.fdb)

(defrecord Entity [e_id name attrs])
(defrecord Attr [name type value ts prev-ts])

(defn make-entity  ([name] (make-entity :no-id-yet name))
  ([name id] (Entity.  id name {})))

(defn make-attr[name val type]  (Attr. name type (if (= :REF type) (:e_id val) val)-1 -1))
(defn add-attr[ ent attr] (assoc-in ent [:attrs (keyword (:name attr))] attr))
(defn next-ts [db] (inc (:curr-time db)))

(defn update-creation-ts [ent tsVal]
  (let [ks (keys (:attrs ent))
        vls (vals (:attrs ent))
        updatedAttrsVals (map #(assoc % :ts tsVal) vls)
        updatedAttrs (interleave ks updatedAttrsVals)
        ](assoc ent :attrs updatedAttrs)))

;  AEVT -> {REFed-ent-id -> {attrName -> [REFing-elems-ids]}}
; this basically provides the info - for each entity that is REFFed by others, who are the others who are REFing it, separated
; by the names of the attribute used for reffing
(defn add-ref-to-aevt[ent aevt attr]
  (let [reffed-id (:value attr)
        attr-name (:name attr)
        reffing-ent (get-in aevt [reffed-id attr-name] #{} )
        ] (assoc-in aevt [reffed-id attr-name] (conj reffing-ent (:e_id ent)))
     ))

(defn update-aevt[old-aevt ent]
  (let [reffingAttrs (filter #(= :REF (:type %)) (vals (:attrs ent)))
        add-ref (partial add-ref-to-aevt ent)]
       (reduce add-ref old-aevt reffingAttrs)))

(defn nextId[db ent] (
                      let [ topId (:topId db)
                            entId (:e_id ent)
                            [idToUse nextTop] (if (= entId :no-id-yet) [(inc topId) (inc topId)] [entId topId])]
                      [idToUse (keyword (str idToUse)) nextTop]))

;when adding an entity, its attributes' timestamp would be set to be the current one
(defn add-entity[db ent]   (let [[ent-id ent-id-key next-top] (nextId db ent)
                                 new-ts (next-ts db)
                                 ts-mp (last (:timestamped db))
                                 fixed-ent (assoc ent :e_id ent-id-key)
                                 new-eavt (assoc (:EAVT ts-mp) ent-id-key  (update-creation-ts fixed-ent new-ts) )
                                 old-aevt (:AEVT ts-mp)
                                 new-aevt (update-aevt old-aevt fixed-ent)
                                 new-indices (assoc ts-mp :AEVT new-aevt :EAVT new-eavt )
                                ](assoc db
                                  :timestamped  (conj (:timestamped db) new-indices)
                                  :topId next-top)))

(defn db-from-transacted [initial   txs]
    (loop [[tx & rst-tx] txs transacted initial]
      (if tx
        (recur rst-tx ((first tx) transacted (second tx)))
        (let [new-ts (next-ts initial)
               initial-indices (:timestamped initial )
              new-indices (last (:timestamped transacted))
              new-topId (:topId transacted)]
         (assoc initial :timestamped (conj  initial-indices new-indices) :curr-time new-ts :topId   new-topId)))))

(defmacro transact [db & txs]
  (when txs
    (loop [[frst-tx# & rst-tx#] txs   res# ['swap! db 'db-from-transacted] cnt# []]
      (if frst-tx#
        (recur rst-tx# res# (conj cnt#  (vec  frst-tx#)))
        (list* (conj res# cnt#))))))

(defn make-db[] ; EAVT: all the entity info, AEVT for attrs who are REFs, we hold the back-pointing (from the REFFed entity to the REFing entities)
  (atom {:timestamped [{:EAVT {} :AEVT {}}]
          :topId 0 :curr-time 0}))

(def db1 (make-db))

(def en1 (-> (make-entity "hotel" "hiilt" )

          (add-attr (make-attr :hotel/room 12 :number))
          (add-attr (make-attr :hotel/address "where" :string)))

  )
(def ref1  (:hiilt (:EAVT(last (:timestamped @db1)))))

(def en2 (-> (make-entity "book")
          (add-attr (make-attr :book/author "jon" :string))
          (add-attr (make-attr :book/found-at ref1 :REF))))


(transact db1 (add-entity en1) (add-entity en2))

(macroexpand-1 '(transact2 db1 (add-entity en1) (add-entity en2)))
;(swap! db1 add-entity en1)
;(def ref1  (:hiilt (:EAVT(last (:timestamped @db1)))))



;; (def en3 (-> (make-entity "gate")
;;           (add-attr (make-attr :gate/color "black" :string))
;;          (add-attr (make-attr :book/found-at ref1 :REF)) ))

;(swap! db1 add-entity en2)


;(swap! db1 add-entity en3)


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
