(ns core.fdb
  [:require [clojure.set :as CS :only (union difference intersection)]])

;; -- EAVT enty-id -> entity {:attrs -> {attr-name -> attr {:value -> the-value}}}
;; -- VAET  structed like this:  {REFed-ent-id -> {attrName -> #{REFing-elems-ids}}}
;;         this basically provides this info: for each entity that is REFFed by others (V), separated by the names of the attribute used for reffing (A), hold the set of the ids of the REFing (E), all this at a given time (T)
;;         can be used to know who REFs a specific entity
;; -- AVET  structed like this:  {attrName-> {attr-val -> #{holding-elems-ids}}}
;;         this basically provides this info: for each attributeName (A) that we chose to index, separated by the values of the attribute  (V), hold the set of the ids of the hold thes attributes (E), all this at a given time (T)
;;         can be used to know who are the entities that have a specific attribute with a specific value
;; -- EVAT structed like this: {ent-id -> {attr-val -> #{attr-names}}}
;;         this provides the info - for a given ent-id (E) and specificed value (V), what are the attributes that have a specific name (A) at a given time (T). Other way to understand
;;         this index is that it answers the question of  how a specific entity is related to a specific value.
(defrecord Database [timestamped top-id curr-time])
(defrecord Indices [EAVT VAET AVET EVAT])
(defrecord Entity [id attrs])
(defrecord Attr [name value ts prev-ts])

(defn make-db "Create an empty database" []
  (atom (Database. [(Indices. {} {} {} {})] 0 0)))

(defn make-entity "creates an entity, if id is not supplied, a running id is assigned to the entity"
  ([] (make-entity :db/no-id-yet ))
  ([id] (Entity.  id {})))

(defn make-attr
  "creation of an attribute. The name, value and type of an attribute are mandatory arguments, further arguments can be passed as named arguguments.
   The type of the attribute may be either :string, :number, :boolean or :db/ref . If the type is :db/ref, the value is an id of another entity and indexing of backpointing is maintained.
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
    {:pre [(contains? #{true false} indexed) (contains? #{:db/single :db/multiple} cardinality)]}
   (with-meta (Attr. name value -1 -1) {:type type :indexed indexed :cardinality cardinality} )))

(defn collify [x] (if (coll? x) x [x]))

(defn- single? [attr]
  (= :db/single (:cardinality (meta attr))))

(defn- indexed? [attr]
  (:indexed (meta attr)))

(defn- ref? [attr]
  (= :db/ref (:type (meta attr))))

(defn entity-at "the entity with the given ent-id at the given time (defualts to the latest time)"
  ([db ent-id ts] (get-in db [:timestamped ts :EAVT ent-id]))
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

(defn- update-attr-value
  "updating the attribute value based on the kind of the operation, the cardinality defined for this attribute and the given value"
  [attr value operation]
  (cond
      (single? attr)    (assoc attr :value #{value})
   ; now we're talking about an attribute of multiple values
      (= :db/reset-to operation)  (assoc attr :value value)
      (= :db/add operation)        (assoc attr :value (CS/union (:value attr)  value))
      (= :db/remove operation)  (assoc attr :value (CS/difference (:value attr) value))))

(defn add-attr
  "adds an attribute to an entity"
  [ent attr]
  (let [attr-id (keyword (:name attr))]
     (assoc-in ent [:attrs attr-id] attr)))

(defn- ave [ent-id attr vl] [attr vl ent-id] )
(defn- vae [ent-id attr vl] [vl attr ent-id] )
(defn- eva [ent-id attr vl] [ent-id vl attr] )

(defn- next-ts [db]
  "returns the next timestamp of the given database"
  (inc (:curr-time db)))

(defn- next-id
  "returns a pair composed of the id to use for the given entity and the next free running id in the database"
  [db ent]
  (let [top-id (:top-id db)
          ent-id (:id ent)
          inceased-id (inc top-id)
          [id-to-use next-top] (if (= ent-id :db/no-id-yet)
                                             [(keyword (str inceased-id)) inceased-id]
                                             [ent-id top-id])]
  [id-to-use next-top]))

(defn- update-creation-ts
  "updates the timestamp value of all the attributes of an entity to the given timestamp"
  [ent ts-val]
  (let [ks (keys (:attrs ent))
         vls (vals (:attrs ent))
         updated-attrs-vals (map #(assoc % :ts ts-val) vls)
         updated-attrs (zipmap ks updated-attrs-vals)
        ](assoc ent :attrs updated-attrs)))

(defn- remove-entry-from-index [index path ]
  (let [remove-path (butlast path)
          val-to-remove (last path)
          old-entries-set (get-in index remove-path )]
     (assoc-in index remove-path (disj old-entries-set val-to-remove))))

(defn- remove-entries-from-index [index attr-name ent-id order-fn path-values operation]
  (if (= operation :db/add)
       index
      (let  [paths (map #(order-fn ent-id attr-name %) path-values)]
       (reduce remove-entry-from-index index paths))))

(defn- add-entry-to-index [index path operation]
  (if (= operation :db/remove)
      index
    (let [update-path (butlast path)
            update-value (last path)
          to-be-updated-set (get-in index update-path #{})]
      (assoc-in index update-path (conj to-be-updated-set update-value) ))))

(defn- update-attr-in-index [index ent-id attr-name order-fn target-val operation]
  (let [ colled-target-val (collify target-val)
         add-entry-fn (fn[indx vl] (add-entry-to-index indx (order-fn ent-id attr-name vl) operation))
         ] (reduce add-entry-fn index colled-target-val)))

(defn- add-entity-to-index  [index ent order-fn filtering-pred]
  (let [ent-id (:id ent)
        all-attrs  (vals (:attrs ent))
        relevant-attrs (filter #(filtering-pred %) all-attrs )
        add-in-index-fn (fn [ind attr] (update-attr-in-index ind ent-id (:name attr) order-fn (:value attr) :db/add))
        ] (reduce add-in-index-fn index relevant-attrs)))

(defn- remove-entity-from-index[index ent order-fn filtering-pred]
  (let [ent-id (:id ent)
        all-attrs  (vals (:attrs ent))
        relevant-attrs (filter #(filtering-pred %) all-attrs )
        remove-from-index-fn (fn [ind attr] ( remove-entries-from-index ind (:name attr) ent-id  order-fn (collify (:value attr)) :db/remove))
        ] (reduce remove-from-index-fn index relevant-attrs)))

;when adding an entity, its attributes' timestamp would be set to be the current one
(defn add-entity [db ent]
  (let [[ent-id next-top] (next-id db ent)
                                 new-ts (next-ts db)
                                 indices (last (:timestamped db))
                                 fixed-ent (assoc ent :id ent-id)
                                 new-eavt (assoc (:EAVT indices) ent-id  (update-creation-ts fixed-ent new-ts) )
                                 new-vaet (add-entity-to-index (:VAET indices) ent vae ref?)
                                 new-avet (add-entity-to-index (:AVET indices) ent ave indexed?)
                                 new-evat (add-entity-to-index (:EVAT indices) ent eva indexed?)
                                 new-indices (assoc indices :VAET new-vaet :EAVT new-eavt :AVET new-avet :EVAT new-evat)
                                ](assoc db :timestamped  (conj (:timestamped db) new-indices)
                                                 :top-id next-top)))

(defn add-entities [db ents-seq]
  (reduce add-entity db ents-seq))

(defn remove-entity[db ent-id]
  (let [ent (entity-at db ent-id)
         indices (last (:timestamped db))
         vaet (remove-entity-from-index (:VAET indices) ent vae ref? )
         avet  (remove-entity-from-index (:AVET indices) ent ave indexed?)
         evat  (remove-entity-from-index (:EVAT indices) ent eva indexed?)
         new-eavt (dissoc (:EAVT indices) ent-id) ; removing the entity
         new-vaet (dissoc vaet ent-id) ; removing incoming REFs to the entity
         new-indices (assoc indices :EAVT new-eavt :VAET new-vaet :AVET avet :EVAT evat)
         res  (assoc db :timestamped (conj  (:timestamped db) new-indices))
        ]res))

(defn transact-on-db [initial-db  txs]
    (loop [[tx & rst-tx] txs transacted initial-db]
      (if tx
          (recur rst-tx (apply (first tx) transacted (rest tx)))
          (let [initial-indices  (:timestamped initial-db)
                  new-indices (last (:timestamped transacted))
                  res (assoc initial-db
                                  :timestamped (conj  initial-indices new-indices)
                                  :curr-time (next-ts initial-db)
                                  :top-id (:top-id transacted))]
                  res))))

(defmacro  _transact [db op & txs]
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

(defn- update-attr-modification-time [attr new-ts]
    (assoc attr :ts new-ts :prev-ts ( :ts attr)))

(defn- update-attr [attr new-val new-ts operation]
   {:pre  [ (if (single? attr)
           (= operation :db/reset-to)
           (contains? #{:db/reset-to :db/add :db/remove} operation))]}
   (-> attr
      (update-attr-modification-time new-ts)
      (update-attr-value new-val operation)))

(defn- remove-path-values [old-vals target-val operation]
  (cond
   (= operation :db/add) [] ; nothing to remove
   (= operation :db/reset-to) old-vals ; removing all of the old values
   (= operation :db/remove) (collify target-val))) ; removing the values defined by the caller

(defn- update-index-for-datom
  [index ent-id attr target-val operation order-fn update-required-pred]
  (if-not (update-required-pred attr)
    index
    (let [old-vals (:value attr)
          attr-name (:name attr)
           remove-paths  (remove-path-values old-vals target-val operation)
           cleaned-index (remove-entries-from-index  index attr-name ent-id order-fn remove-paths operation)
           updated-index  (update-attr-in-index cleaned-index ent-id attr-name order-fn target-val operation)
          ] updated-index)))

(defn- update-eavt-for-datom [eavt ent-id new-attr]
  (assoc-in eavt [ent-id :attrs (:name new-attr)] new-attr))

(defn- update-indices [indices ent-id attr updated-attr new-val operation]
  (let [ new-eavt (update-eavt-for-datom (:EAVT indices) ent-id updated-attr)
         ; in the next indices updates the attr is intentionally passed and not updated-attr, we need to use the old value to locate where to update in the index
          new-vaet (update-index-for-datom (:VAET indices) ent-id attr new-val operation vae ref?)
          new-avet (update-index-for-datom (:AVET indices) ent-id attr new-val operation ave indexed?)
          new-evat (update-index-for-datom (:EVAT indices) ent-id attr new-val operation  eva indexed?)
         ] (assoc indices :EAVT new-eavt :VAET new-vaet :AVET new-avet)))

(defn update-datom
  ([db ent-id att-name  new-val]  (update-datom db ent-id att-name  new-val  :db/reset-to ))
  ([db ent-id att-name  new-val operation ] ; operation may be either  :db/reset-to  :db/add ,or :db/remove (the last two are valid only if the attr cardinality is :db/multiple)
     (let [update-ts (next-ts db)
            indices (last (:timestamped db))
            attr (get-in indices [:EAVT ent-id :attrs  att-name])
            updated-attr (update-attr attr new-val update-ts operation)
            fully-updated-indices (update-indices indices ent-id attr updated-attr new-val operation)
            new-db (update-in db [:timestamped] conj fully-updated-indices)
           ]new-db)))

;;;;;;;;;;;;;;;;;;;;;;;  queries

(defn entities-of-ids
  "for a given seq of entity ids, return the real entities"
  [db ent-ids]
  (let [indices (last (:timestamped db))
         eavt (:EAVT indices)]
    (map #(% eavt) ent-ids)))

(defn entities-by-AV [db attr-name val-pred]
   (let [indices (last (:timestamped db))
          ve (get-in indices [:AVET :test/machine] )
          relevant-entries  (filter #(val-pred (first %)) ve)
          relevant-ids-sets (map second relevant-entries)
          relevant-ent-ids (seq (reduce CS/union relevant-ids-sets ))
         ](entities-of-ids db relevant-ent-ids)))

(defn entities-by-A [db attr-name]
  (entities-by-AV [db attr-name #(= % %)]))

(defn ref-to-as
  "returns a seq of all the entities that have REFed to the give entity with the given attr-name (alternativly had an attribute
   named attr-name whose type is :db/ref and the value was ent-id), all this at a given time"
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
          (recur (conj res {(:ts attr) (:value attr)})  (:prev-ts attr))))))

(defn db-before
  "How the db was before a given timestamp"
  [db ts]
  (let [indices-before (subvec (:timestamped db) 0 ts )]
    (assoc db :timestamped indices-before :curr-time ts)))

(defn ind-at
  "inspecting a specific index at a given time. The kind argument may be of of these:  :AVET :EAVT :VAET :EVAT"
  [db ts kind]
  (kind ((:timestamped db) ts)))
