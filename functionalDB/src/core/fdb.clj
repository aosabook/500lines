(ns core.fdb)

(defrecord Database [timestamped top-id curr-time])
(defrecord Indices [EAVT VAET AVET])
(defrecord Entity [id attrs])
(defrecord Attr [name type value ts prev-ts])

(defn make-db[] (atom (Database. [(Indices. {} {} {})] 0 0))) ;EAVT: all the entity info, vaet for attrs who are REFs, we hold the back-pointing (from the REFFed entity to the REFing entities)

(defn make-entity  ([] (make-entity :db/no-id-yet ))
  ([id] (Entity.  id {})))

(defn make-attr
  ([name value type ]  (make-attr name value type :db/single))
  ([name value type cardinality]  (make-attr name value type cardinality false))
  ([name value type cardinality indexed] (with-meta (Attr. name type value -1 -1) {:indexed indexed :cardinality cardinality} )))

(defn update-attr-value [attr value operation]
  (cond
      (= :db/single (:cardinality (meta attr)))    (assoc attr :value #{value})
   ; now = :db/mutiple (:cardinality (meta attr)))

      (= :db/reset-to operation)  (assoc attr :value #{value})
      (= :db/add operation)        (assoc attr :value (conj (:value attr) value ))
      (= :db/remove operation)  (assoc attr :value (disj (:value attr) value ))
   ))

;;   (if (= :db/single (:cardinality (meta attr)))
;;     (assoc attr :value #{value})
;;     (assoc attr :value (conj (:value attr) value))))

(defn conj-to-attr [old new operation]
  (let [new-val (:value new)]
   (if old
    (update-attr-value old (:value new) operation)
    (update-attr-value new (:value new) :db/reset-to   ))))

(defn add-attr[ ent attr]
  (let [attr-id (keyword (:name attr))
          existing-attr (get-in ent [:attrs attr-id])
          updated-attr (conj-to-attr existing-attr attr :db/add)]
     (assoc-in ent [:attrs attr-id ] updated-attr)))

(defn next-ts [db]
  (inc (:curr-time db)))

(defn nextId[db ent]
  (let [top-id (:top-id db)
          entId (:id ent)
          inceased-id (inc top-id)
          [idToUse nextTop] (if (= entId :db/no-id-yet) [(keyword (str inceased-id)) inceased-id] [entId top-id])]
                              [idToUse nextTop]))

(defn update-creation-ts [ent tsVal]
  (let [ks (keys (:attrs ent))
        vls (vals (:attrs ent))
        updatedAttrsVals (map #(assoc % :ts tsVal) vls)
        updatedAttrs (zipmap ks updatedAttrsVals)
        ](assoc ent :attrs updatedAttrs)))

;  vaet -> {REFed-ent-id -> {attrName -> [REFing-elems-ids]}}
; this basically provides this info - for each entity that is REFFed by others, who are the others who are REFing it, separated
; by the names of the attribute used for reffing
(defn add-ref-to-vaet[ent operation vaet attr]
  (let [reffed-id (first (:value attr))
        attr-name (:name attr)
        back-reffing-set (get-in vaet [reffed-id attr-name] #{} )
        new-back-reffing-set (operation back-reffing-set (:id ent))
        ] (assoc-in vaet [reffed-id attr-name] new-back-reffing-set)))

(defn update-attr-in-avet[ent operation avet attr]
  (let [attr-name (:name attr)
         attr-value (first (:value attr)) ; a set
         curr-entities-set (get-in avet [attr-name attr-value] #{})
         updated-entities-set (operation curr-entities-set (:id ent))
        ] (assoc-in avet [attr-name attr-value] updated-entities-set)))

(defn update-vaet[old-vaet ent operation]
  (let [reffingAttrs (filter #(= :REF (:type %)) (vals (:attrs ent)))
        add-ref (partial add-ref-to-vaet ent operation)]
       (reduce add-ref old-vaet reffingAttrs)))

;avet : attr-name -> attr-value -> #{ids of ents}
(defn update-avet [old-avet ent operation]
     (let [indexed-attrs (filter #(:indexed (meta %)) (vals (:attrs ent)))
           update-attr-in-avet-fn (partial update-attr-in-avet ent operation)]
       (reduce update-attr-in-avet-fn old-avet indexed-attrs)))

;when adding an entity, its attributes' timestamp would be set to be the current one
(defn add-entity[db ent]
  (let [[ent-id next-top] (nextId db ent)
                                 new-ts (next-ts db)
                                 indices (last (:timestamped db))
                                 fixed-ent (assoc ent :id ent-id)
                                 new-eavt (assoc (:EAVT indices) ent-id  (update-creation-ts fixed-ent new-ts) )
                                 new-vaet (update-vaet  (:VAET indices) fixed-ent conj)
                                 new-avet (update-avet (:AVET indices) fixed-ent conj)
                                 new-indices (assoc indices :VAET new-vaet :EAVT new-eavt :AVET new-avet )
                                ](assoc db :timestamped  (conj (:timestamped db) new-indices)
                                                 :top-id next-top)))

(defn remove-entity[db ent]
  (let [ent-id (:id ent)
         indices (last (:timestamped db))
        vaet (update-vaet  (:VAET indices) ent disj)
        avet (update-avet  (:AVET indices) ent disj)
        new-eavt (dissoc (:EAVT indices) ent-id) ; removing the entity
        new-vaet (dissoc vaet ent-id) ; removing incoming REFs to the entity
        new-indices (assoc indices :EAVT new-eavt :VAET new-vaet :AVET avet)
        res  (assoc db :timestamped (conj  (:timestamped db) new-indices))]
        res))

(defn transact-on-db [initial-db  txs]
    (loop [[tx & rst-tx] txs transacted initial-db]

      (if tx    (recur rst-tx (apply (first tx) transacted (rest tx)))
                 (let [ initial-indices  (:timestamped initial-db)
                          new-indices (last (:timestamped transacted))
                        res (assoc initial-db :timestamped (conj  initial-indices new-indices)
                                           :curr-time (next-ts initial-db)
                                           :top-id (:top-id transacted))]
                  res))))

(defmacro transact_ [db op & txs]
  (when txs
    (loop              [[frst-tx# & rst-tx#] txs         res#   [op db 'transact-on-db]               accum-txs# []]
      (if frst-tx#     (recur                     rst-tx#              res#                                          (conj  accum-txs#  (vec  frst-tx#)))
                           (list* (conj res#  accum-txs#))))))

(defn _what-if [ db f  txs] (f db txs))

(defmacro what-if [db & txs]  `(transact_ ~db   _what-if  ~@txs))

(defmacro transact [db & txs] `(transact_ ~db swap! ~@txs))

(defn remove-entry-from-index [index path val-to-remove operation]
  (if (= operation :db/add)
       index
      (let [old-entries-set (get-in index path )]
        (assoc-in index path (disj old-entries-set val-to-remove)))))

(defn add-entry-to-index [index path val-to-add operation]
  (if (= operation :db/remove)
      index
    (let [to-be-updated-set (get-in index path #{})]
      (assoc-in index path (conj to-be-updated-set val-to-add)))))

(defn update-vaet-for-datom [vaet  ent-id attr new-ref-id operation]
  (if (not= :REF (:type attr ))
    vaet
    (let [
          old-ref-id (first (:value attr))
          attr-name (:name attr)
          ;; removing old value
          cleaned-vaet (remove-entry-from-index vaet [old-ref-id attr-name] ent-id)

;;           old-reffed (get-in vaet [old-ref-id attr-name])
;;           cleaned-vaet (assoc-in vaet [old-ref-id attr-name] (disj old-reffed ent-id))
          ;; adding new value
          updated-vaet (add-entry-to-index cleaned-vaet [new-ref-id  attr-name] ent-id operation)
;;           to-be-updated-ref (get-in cleaned-vaet [new-val attr-name] #{})
;;           updated-vaet (assoc-in cleaned-vaet [new-val attr-name] (conj  to-be-updated-ref ent-id))
          ] updated-vaet)))

(defn update-avet-for-datom [avet ent-id attr new-attr-val operation]
  (if (:indexed (meta attr))
    avet
    (let [old-attr-val (first (:value attr))
          attr-name (:name attr)
          ;; removing old value
          cleaned-avet (remove-entry-from-index avet [attr-name old-attr-val] ent-id operation)

;;           old-entities-set (get-in avet [attr-name old-attr-val])
;;           cleaned-avet (assoc-in avet [attr-name old-attr-val] (disj old-entities-set ent-id))
       ;;   adding new value
          updated-avet (add-entry-to-index cleaned-avet [attr-name new-attr-val] ent-id operation)
;;           to-be-updated-entities-set (get-in cleaned-avet [attr-name new-val] #{})
;;           updated-avet (assoc-in cleaned-avet [attr-name new-val] (conj to-be-updated-entities-set ent-id))
          ] updated-avet )))

(defn update-eavt-for-datom [eavt ent-id new-attr]
  (assoc-in eavt [ent-id (:name new-attr)] new-attr))

(defn update-datom
  ([db ent-id att-name  new-val]  (update-datom db ent-id att-name  new-val  :db/reset-to ))
  ([db ent-id att-name  new-val operation ] ; operation may be either  :db/reset-to  :db/add ,or :db/remove (the last two are valid only if the attr cardinality is :db/mutiple)
     (let [ new-ts (next-ts db)
            indices (last (:timestamped db))
            attr (get-in indices [:EAVT ent-id :attrs  att-name] )
            real-new-val  (if (= :REF (:type attr)) (:id new-val) new-val)
            updated-timed-attr (assoc attr  :ts new-ts :prev-ts ( :ts attr))
            updated-attr (conj-to-attr updated-timed-attr {:value real-new-val} operation)
            new-eavt (update-eavt-for-datom (:EAVT indices) ent-id updated-attr)
            new-vaet (update-vaet-for-datom (:VAET indices) ent-id attr real-new-val operation) ; intentionally attr is passed and not updated-attr, we need to use the old value to locate where to update in the index
            new-avet (update-avet-for-datom (:AVET indices) ent-id attr real-new-val operation) ; intentionally attr is passed and not updated-attr, we need to use the old value to locate where to update in the index
            fully-updated-indices (assoc indices :EAVT new-eavt :VAET new-vaet :AVET new-avet)
            new-db (assoc db :timestamped (conj  (:timestamped db) fully-updated-indices))
           ]new-db)))

(defn entity-at ([db ent-id ts] ((keyword ent-id) ((:timestamped db) ts)))
                      ([db ent-id] (entity-at db ent-id (:curr-time db))) )

(defn attr-at "The attribute of an entity at a given time (defaults to recent time)"
  ([db ent-id attr-name] (attr-at db ent-id attr-name (:curr-time db)))
  ([db ent-id attr-name ts]
   (let [indices ((:timestamped db) ts)]  (get-in indices [:EAVT ent-id :attrs attr-name]))))

(defn value-of-at  "value of a datom at a given time, if no time is provided, we default to the most recent value"
  ([db ent-id attr-name]  (:value (attr-at db ent-id attr-name)))
  ([db ent-id attr-name ts] (:value (attr-at db ent-id attr-name ts))))

(defn relates-to-as "returns a seq of all the entities that REFed to a specific entity with the given attr-name (alternativly had an attribute named attr-name whose type is REF and the value was ent-id), all this at a given time"
   ([db ent-id attr-name]  (relates-to-as db ent-id attr-name (:curr-time db)))
  ([db ent-id attr-name ts]
      (let [indices ((:timestamped db) ts)
              reffing-ids (get-in indices [:VAET ent-id attr-name])]
        (map #(get-in indices [:EAVT %]) reffing-ids ))))

(defn evolution-of "The sequence of the values of of an entity's attribute, as changed through time" [db ent-id attr-name]
  (loop [res [] ts (:curr-time db)]
    (if (= -1 ts) (reverse res)
        (let [attr (attr-at db ent-id attr-name ts)]
          (recur (conj res {ts (:value attr)})  (:prev-ts attr))))))

(defn db-before [db ts]
  (let [indices-before (subvec (:timestamped db) 0 ts )]
    (assoc db :timestamped indices-before :curr-time ts)))
