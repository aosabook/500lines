(ns core.fdb)

(defrecord Entity [e_id name attrs])
(defrecord Attr [name type value ts prev-ts])

(defn make-db[] ; EAVT: all the entity info, AEVT for attrs who are REFs, we hold the back-pointing (from the REFFed entity to the REFing entities)
  (atom {:timestamped [{:EAVT {} :AEVT {}}]
          :topId 0 :curr-time 0}))

(defn make-entity  ([name] (make-entity :no-id-yet name))
  ([id name] (Entity.  id name {})))

(defn make-attr[name val type]  (Attr. name type (val-from-ref type val) -1 -1))
(defn add-attr[ ent attr] (assoc-in ent [:attrs (keyword (:name attr))] attr))

(defn next-ts [db] (inc (:curr-time db)))
(defn nextId[db ent] (
                      let [ topId (:topId db)
                            entId (:e_id ent)
                            [idToUse nextTop] (if (= entId :no-id-yet) [(inc topId) (inc topId)] [entId topId])]
                      [idToUse (keyword (str idToUse)) nextTop]))

(defn val-from-ref[attr-type attr-val](if (= :REF attr-type) (:e_id attr-val) attr-val ))

(defn update-creation-ts [ent tsVal]
  (let [ks (keys (:attrs ent))
        vls (vals (:attrs ent))
        updatedAttrsVals (map #(assoc % :ts tsVal) vls)
        updatedAttrs (zipmap ks updatedAttrsVals)
        ](assoc ent :attrs updatedAttrs)))

;  AEVT -> {REFed-ent-id -> {attrName -> [REFing-elems-ids]}}
; this basically provides the info - for each entity that is REFFed by others, who are the others who are REFing it, separated
; by the names of the attribute used for reffing
(defn add-ref-to-aevt[ent operation aevt attr]
  (let [reffed-id (:value attr)
        attr-name (:name attr)
        back-reffing-set (get-in aevt [reffed-id attr-name] #{} )
        new-back-reffing-set (operation back-reffing-set (:e_id ent))
        ] (assoc-in aevt [reffed-id attr-name] new-back-reffing-set)
  ))

(defn update-aevt[old-aevt ent operation]
  (let [reffingAttrs (filter #(= :REF (:type %)) (vals (:attrs ent)))
        add-ref (partial add-ref-to-aevt ent operation)]
       (reduce add-ref old-aevt reffingAttrs)))

;when adding an entity, its attributes' timestamp would be set to be the current one
(defn add-entity[db ent]   (let [[ent-id ent-id-key next-top] (nextId db ent)
                                 new-ts (next-ts db)
                                 indices (last (:timestamped db))
                                 fixed-ent (assoc ent :e_id ent-id-key)
                                 new-eavt (assoc (:EAVT indices) ent-id-key  (update-creation-ts fixed-ent new-ts) )
                                 new-aevt (update-aevt  (:AEVT indices) fixed-ent conj)
                                 new-indices (assoc indices :AEVT new-aevt :EAVT new-eavt )
                                ](assoc db
                                  :timestamped  (conj (:timestamped db) new-indices)
                                  :topId next-top)))

(defn remove-entity[db ent]
  (let [ent-id (:e_id ent)
         indices (last (:timestamped db))
        aevt (update-aevt  (:AEVT indices) ent disj)
        new-eavt (dissoc (:EAVT indices) ent-id) ; removing the entity
        new-aevt (dissoc aevt ent-id) ; removing incoming REFs to the entity
        new-indices (assoc indices :EAVT new-eavt :AEVT new-aevt)
        res  (assoc db :timestamped (conj  (:timestamped db) new-indices))]
        res))

(defn transact-on-db [initial-db txs]
    (loop [[tx & rst-tx] txs transacted initial-db]
      (if tx    (recur rst-tx (apply (first tx) transacted (rest tx)))
                 (let [ initial-indices  (:timestamped initial-db)
                          new-indices (last (:timestamped transacted))
                        transacted transacted
                        res (assoc initial-db :timestamped (conj  initial-indices new-indices)
                                           :curr-time (next-ts initial-db)
                                           :topId (:topId transacted)) ]
                       res))))

(defmacro transact [db & txs]
  (when txs
    (loop              [[frst-tx# & rst-tx#] txs         res#   ['swap! db 'transact-on-db] cnt# []]
      (if frst-tx#     (recur                     rst-tx#              res#                                          (conj cnt#  (vec  frst-tx#)))
                           (list* (conj res# cnt#))))))

(defn update-aevt-for-datum [aevt  ent-id attr new-val]
  (if (not= :REF (:type attr ))
    aevt
    (let [ old-ref-id (:value attr)
             attr-name (:name attr)
             old-reffed (get-in aevt [old-ref-id attr-name])
             cleaned-aevt (assoc-in aevt [old-ref-id attr-name] (disj old-reffed ent-id))
             to-be-updated-ref (get-in aevt [new-val attr-name] #{})
             updated-aevt (assoc-in cleaned-aevt [new-val attr-name] (conj  to-be-updated-ref ent-id) )
          ] updated-aevt)))

(defn update-datum [db ent-id att-name  new-val]
     (let [ new-ts (next-ts db)
            indices (last (:timestamped db))
            attr (get-in indices [:EAVT ent-id :attrs  att-name] )
            real-new-val  (val-from-ref (:type attr) new-val)
            updated-attr(assoc attr :value real-new-val :ts new-ts :prev-ts ( :ts attr))
           eavt-updated-indices (assoc-in indices [:EAVT ent-id :attrs att-name] updated-attr )
           new-aevt (update-aevt-for-datum (:AEVT indices) ent-id attr real-new-val)
           fully-updated-indices (assoc eavt-updated-indices :AEVT new-aevt)
           new-db (assoc db :timestamped (conj  (:timestamped db) fully-updated-indices))
           ]new-db))


(def db1 (make-db))

(def en1 (-> (make-entity "hiilt" "hotel" )
         (add-attr (make-attr :hotel/room 12 :number))
          (add-attr (make-attr :hotel/address "where" :string))))
(transact db1 (add-entity en1))

;(def rel-e1 (get-in (last (:timestamped @db1)) [:EAVT :hiilt]))




;(add-entity @db1 en1)

(def ref1  (:hiilt (:EAVT(last (:timestamped @db1)))))
(:e_id ref1)
;(type(:attrs ref1))
(def en2 (-> (make-entity "book")
          (add-attr (make-attr :book/author "jon" :string))
          (add-attr (make-attr :book/found-at ref1 :REF))))

 (def en3 (-> (make-entity "gate")
           (add-attr (make-attr :gate/color "black" :string))
          (add-attr (make-attr :gate/found-at ref1 :REF)) ))
;(transact db1  (add-entity en2) (add-entity en3))


 ;(transact db1 (remove-entity ref1))



(transact db1  (add-entity en2) (add-entity en3))
(def ref12  (:1 (:EAVT(last (:timestamped @db1)))))


(transact db1 (update-datum  :2  :gate/found-at ref12 ))


;(swap! db1 transact-on-db [[remove-entity ref1]])
;(macroexpand-1 '(transact db1 (remove-entity ref1)))
;(macroexpand-1 '(transact2 db1 (add-entity en1) (add-entity en2)))
;(swap! db1 add-entity en1)
;(def ref1  (:hiilt (:EAVT(last (:timestamped @db1)))))





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
