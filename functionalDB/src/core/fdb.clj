(ns core.fdb
  [:require [clojure.set :as CS :only (union difference )]])

(defrecord Database [timestamped top-id curr-time])
(defrecord Indices [EAVT VAET AVET])
(defrecord Entity [id attrs])
(defrecord Attr [name value ts prev-ts])

(defn make-db "Create an empty database" []
  (atom (Database. [(Indices. {} {} {})] 0 0))) ;EAVT: all the entity info, vaet for attrs who are of type :db/ref, we hold the back-pointing (from the REFFed entity to the REFing entities)

(defn make-entity "creates an entity, if id is not supplied, a running id is assigned to the entity"
  ([] (make-entity :db/no-id-yet ))
  ([id] (Entity.  id {})))

(defn make-attr
  "creation of an attribute. The name, value and type of an attribute are mandatory arguments, further arguments can be passed as named arguguments.
   The type of the attribute may be either :string, :number, :boolean or :db/ref. If the type is :db/ref, the value is an id of another entity and indexing of backpointing is maintained.
  The named arguments are as follows:
  :indexed - a boolean, can be either true or false - marks whether this attribute should be indexed. By defaults attributes are not inexed.
  :cardinality - the cardinality of an attribute, can be either:
                     :db/single - which means that this attribute can be a single value at any given time (this is the default cardinality)
                     :db/multiple - which means that this attribute is actually a set of values. In this case updates of this attribute may be one of the following (NOTE that all these operations accept a set as argument):
                                          :db/add - adds a set of values to the currently existing set of values
                                          :db/reset-to - resets the value of this attribute to be the given set of values
                                          :db/remove - removes the given set of values from the attribute's current set of values"
  ([name value type ; these ones are required
      & {:keys [indexed cardinality] :or {indexed false cardinality :db/single}} ]
   (with-meta (Attr. name value -1 -1) {:type type :indexed indexed :cardinality cardinality} )))

(defn- single? [attr]
  (= :db/single (:cardinality (meta attr))))

(defn- indexed? [attr]
  (:indexed (meta attr)))

(defn- ref? [attr]
  (= :db/ref (:type (meta attr))))

(defn- update-attr-value
  "updating the attribute value based on the kind of the operation, the cardinality defined for this attribute and the given value"
  [attr value operation]
  (cond
      (single? attr)    (assoc attr :value #{value})
   ; now we're talking about an attribute of multiple values
      (= :db/reset-to operation)  (assoc attr :value value)
      (= :db/add operation)        (assoc attr :value (CS/union (:value attr)  value))
      (= :db/remove operation)  (assoc attr :value (CS/difference (:value attr) value))))

(defn- conj-to-attr
  "conjoins a value to an attribute, this function can be used either when initializing a new attribute or updating an existing one"
  [old new operation]
  (let [new-val (:value new)]
   (if old
    (update-attr-value old (:value new) operation)
    (update-attr-value new (:value new) :db/reset-to))))

(defn add-attr
  "adds an attribute to an entity"
  [ent attr]
  (let [attr-id (keyword (:name attr))
          existing-attr (get-in ent [:attrs attr-id])
          updated-attr (conj-to-attr existing-attr attr :db/add)]
     (assoc-in ent [:attrs attr-id ] updated-attr)))

(defn- next-ts [db]
  "returns the next timestamp of the given database"
  (inc (:curr-time db)))

(defn- next-id
  "returns a pair composed of the id to use for the given entity and the next free running id in the database"
  [db ent]
  (let [top-id (:top-id db)
          ent-id (:id ent)
          inceased-id (inc top-id)
          [id-to-use next-top] (if (= ent-id :db/no-id-yet) [(keyword (str inceased-id)) inceased-id] [ent-id top-id])]
                              [id-to-use next-top]))

(defn- update-creation-ts
  "updates the timestamp value of all the attributes of an entity to the given timestamp"
  [ent tsVal]
  (let [ks (keys (:attrs ent))
        vls (vals (:attrs ent))
        updatedAttrsVals (map #(assoc % :ts tsVal) vls)
        updatedAttrs (zipmap ks updatedAttrsVals)
        ](assoc ent :attrs updatedAttrs)))

;  VAET  structed like this:  {REFed-ent-id -> {attrName -> #{REFing-elems-ids}}}
; this basically provides this info: for each entity that is REFFed by others (V), separated by the names of the attribute used for reffing (A), hold the set of the ids of the REFing (E), all this at a given time (T)
;  can be used to know who REFs a specific entity
(defn- update-ref-in-vaet
  "adding an entry to th VAET index"
  [ent operation vaet attr]
  (let [reffed-id (first (:value attr))
        attr-name (:name attr)
        back-reffing-set (get-in vaet [reffed-id attr-name] #{} )
        new-back-reffing-set (operation back-reffing-set (:id ent))]
    (if (nil? reffed-id)
       vaet
       (assoc-in vaet [reffed-id attr-name] new-back-reffing-set))))

;  AVET  structed like this:  {attrName-> {attrValue -> #{holding-elems-ids}}
; this basically provides this info: for each attributeName (A) that we chose to index, separated by the values of the attribute  (V), hold the set of the ids of the hold thes attributes (E), all this at a given time (T)
;  can be used to know who are the entities that have a specific attribute with a specific value
(defn- update-attr-in-avet
  [ent operation avet attr]
  (let [attr-name (:name attr)
         attr-value (first (:value attr)) ; a set
         curr-entities-set (get-in avet [attr-name attr-value] #{})
         updated-entities-set (operation curr-entities-set (:id ent))
        ]
    (if (nil? attr-value)
      avet
    (assoc-in avet [attr-name attr-value] updated-entities-set))))

(defn- update-vaet[old-vaet ent operation]
  (let [reffingAttrs (filter #(ref? %) (vals (:attrs ent)))
        add-ref (partial update-ref-in-vaet ent operation)]
       (reduce add-ref old-vaet reffingAttrs)))

;avet : attr-name -> attr-value -> #{ids of ents}
(defn- update-avet [old-avet ent operation]
     (let [indexed-attrs (filter #(indexed? %) (vals (:attrs ent)))
           update-attr-in-avet-fn (partial update-attr-in-avet ent operation)]
       (reduce update-attr-in-avet-fn old-avet indexed-attrs)))

;when adding an entity, its attributes' timestamp would be set to be the current one
(defn add-entity [db ent]
  (let [[ent-id next-top] (next-id db ent)
                                 new-ts (next-ts db)
                                 indices (last (:timestamped db))
                                 fixed-ent (assoc ent :id ent-id)
                                 new-eavt (assoc (:EAVT indices) ent-id  (update-creation-ts fixed-ent new-ts) )
                                 new-vaet (update-vaet  (:VAET indices) fixed-ent conj)
                                 new-avet (update-avet (:AVET indices) fixed-ent conj)
                                 new-indices (assoc indices :VAET new-vaet :EAVT new-eavt :AVET new-avet )
                                ](assoc db :timestamped  (conj (:timestamped db) new-indices)
                                                 :top-id next-top)))

(defn add-entities [db ents-seq]
  (reduce add-entity db ents-seq))

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

(defmacro _transact [db op & txs]
  (when txs
    (loop [[frst-tx# & rst-tx#] txs  res#  [op db 'transact-on-db]  accum-txs# []]
      (if frst-tx#
          (recur rst-tx# res#  (conj  accum-txs#  (vec  frst-tx#)))
          (list* (conj res#  accum-txs#))))))

(defn _what-if [ db f  txs] (f db txs))

(defmacro what-if [db & txs]
  `(_transact ~db   _what-if  ~@txs))

(defmacro transact [db & txs]
  `(_transact ~db swap! ~@txs))

;;;;;;;;;;;;;;;;;;;;;;;  datom updates

(defn- remove-entry-from-index [val-to-remove index path ]
  (let [ old-entries-set (get-in index path )]
     (assoc-in index path (disj old-entries-set val-to-remove))))

(defn- remove-entries-from-index [index paths val-to-remove operation]
  (if (= operation :db/add)
       index
      (let  [   remover   (partial remove-entry-from-index  val-to-remove)]
       (reduce remover index (operation paths)))))

(defn- add-entry-to-index [index path val-to-add operation]
  (if (= operation :db/remove)
      index
    (let [to-be-updated-set (get-in index path #{})]
      (assoc-in index path (conj to-be-updated-set val-to-add)))))

(defn- av [attr vl] [attr vl] )
(defn- va [attr vl] [vl attr] )

(defn- update-index-for-datom
  [index ent-id attr target-val operation order-fn update-required-pred]
  (if-not (update-required-pred attr)
    index
    (let [old-vals (:value attr)
          attr-name (:name attr)
          paths-for-replace (map (fn[i](order-fn attr-name i)) old-vals) ; replace means that we need to remove all the old items, and add the new item
          paths-for-remove [(order-fn attr-name target-val)] ; remove means that the item that we received is the one to be removed
          paths {:db/remove paths-for-remove :db/reset-to paths-for-replace}
          ;; removing old values
          cleaned-index (remove-entries-from-index index paths  ent-id operation)
          ;; adding new value
           colled-target-val (if (coll?  target-val)  target-val [ target-val])
           add-func (fn [ind vl] (add-entry-to-index ind (order-fn attr-name vl) ent-id operation))
           updated-index (reduce add-func cleaned-index colled-target-val)
          ] updated-index)))

(defn- update-eavt-for-datom [eavt ent-id new-attr]
  (assoc-in eavt [ent-id :attrs (:name new-attr)] new-attr))

(defn- update-attr [attr new-val new-ts operation]
  (let [updated-timed-attr (assoc attr  :ts new-ts :prev-ts ( :ts attr))]
    (conj-to-attr updated-timed-attr {:value new-val} operation)))

(defn- update-indices [indices ent-id attr updated-attr new-val operation]
  (let [new-eavt (update-eavt-for-datom (:EAVT indices) ent-id updated-attr)
          new-vaet (update-index-for-datom (:VAET indices) ent-id attr new-val operation va ref?) ; intentionally attr is passed and not updated-attr, we need to use the old value to locate where to update in the index
          new-avet (update-index-for-datom (:AVET indices) ent-id attr new-val operation av indexed?) ; intentionally attr is passed and not updated-attr, we need to use the old value to locate where to update in the index
         ](assoc indices :EAVT new-eavt :VAET new-vaet :AVET new-avet)))

(defn update-datom
  ([db ent-id att-name  new-val]  (update-datom db ent-id att-name  new-val  :db/reset-to ))
  ([db ent-id att-name  new-val operation ] ; operation may be either  :db/reset-to  :db/add ,or :db/remove (the last two are valid only if the attr cardinality is :db/multiple)
     (let [ new-ts (next-ts db)
            indices (last (:timestamped db))
            attr (get-in indices [:EAVT ent-id :attrs  att-name])
            updated-attr (update-attr attr new-val new-ts operation)
            fully-updated-indices (update-indices indices ent-id attr updated-attr new-val operation)
            new-db (update-in db [:timestamped] conj fully-updated-indices)
           ]new-db)))

;;;;;;;;;;;;;;;;;;;;;;;  queries

(defn entity-at ([db ent-id ts] (ent-id ((:timestamped db) ts)))
                      ([db ent-id] (entity-at db ent-id (:curr-time db))) )

(defn attr-at
  "The attribute of an entity at a given time (defaults to recent time)"
  ([db ent-id attr-name] (attr-at db ent-id attr-name (:curr-time db)))
  ([db ent-id attr-name ts]
   (let [indices ((:timestamped db) ts)]  (get-in indices [:EAVT ent-id :attrs attr-name]))))

(defn value-of-at
  "value of a datom at a given time, if no time is provided, we default to the most recent value"
  ([db ent-id attr-name]  (:value (attr-at db ent-id attr-name)))
  ([db ent-id attr-name ts] (:value (attr-at db ent-id attr-name ts))))

(defn ref-to-as
  "returns a seq of all the entities that REFed to a specific entity with the given attr-name (alternativly had an attribute named attr-name whose type is :db/ref
   and the value was ent-id), all this at a given time"
   ([db ent-id attr-name]  (ref-to-as db ent-id attr-name (:curr-time db)))
  ([db ent-id attr-name ts]
      (let [indices ((:timestamped db) ts)
              reffing-ids (get-in indices [:VAET ent-id attr-name])]
        (map #(get-in indices [:EAVT %]) reffing-ids ))))

(defn evolution-of
  "The sequence of the values of of an entity's attribute, as changed through time"
  [db ent-id attr-name]
  (loop [res [] ts (:curr-time db)]
    (if (= -1 ts) (reverse res)
        (let [attr (attr-at db ent-id attr-name ts)]
          (recur (conj res {ts (:value attr)})  (:prev-ts attr))))))

(defn db-before [db ts]
  (let [indices-before (subvec (:timestamped db) 0 ts )]
    (assoc db :timestamped indices-before :curr-time ts)))

(defn ind-at
  "inspecting a specific index at a given time. The kind argument may be of of these:  :AVET :EAVT :VAET "
  [db ts kind]
  (kind ((:timestamped db) ts)))
