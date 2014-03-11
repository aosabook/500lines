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
   (with-meta (Attr. name value -1 -1) {:type type :indexed indexed :cardinality cardinality} )))

(defn collify [x] (if (coll? x) x [x]))

(defn- single? [attr]
  (= :db/single (:cardinality (meta attr))))

(defn- indexed? [attr]
  (:indexed (meta attr)))

(defn- ref? [attr]
  (= :db/ref (:type (meta attr))))

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

(defn- av [attr vl] [attr vl] )
(defn- va [attr vl] [vl attr] )

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

(defn- remove-entry-from-index [val-to-remove index path ]
  (let [ old-entries-set (get-in index path )]
     (assoc-in index path (disj old-entries-set val-to-remove))))

(defn- remove-entries-from-index [index attr-name ent-id order-fn path-values operation]
  (if (= operation :db/add)
       index
      (let  [paths (map #(order-fn attr-name %) path-values)
             remover (partial remove-entry-from-index ent-id)]
       (reduce remover index paths))))

;  VAET  structed like this:  {REFed-ent-id -> {attrName -> #{REFing-elems-ids}}}
; this basically provides this info: for each entity that is REFFed by others (V), separated by the names of the attribute used for reffing (A), hold the set of the ids of the REFing (E), all this at a given time (T)
;  can be used to know who REFs a specific entity


;; ;  AVET  structed like this:  {attrName-> {attrValue -> #{holding-elems-ids}}
;; ; this basically provides this info: for each attributeName (A) that we chose to index, separated by the values of the attribute  (V), hold the set of the ids of the hold thes attributes (E), all this at a given time (T)
;; ;  can be used to know who are the entities that have a specific attribute with a specific value

(defn- add-entry-to-index [index path val-to-add operation]
  (if (= operation :db/remove)
      index
    (let [to-be-updated-set (get-in index path #{})]
      (assoc-in index path (conj to-be-updated-set val-to-add)))))

(defn- update-attr-in-index [index ent-id attr-name order-fn target-val operation]
  (let [ colled-target-val (collify target-val)
         add-entry-fn (fn[indx vl] (add-entry-to-index indx (order-fn attr-name vl) ent-id operation))
         ] (reduce add-entry-fn index colled-target-val)))

(defn- add-entity-to-index  [index ent order-fn filtering-pred]
  (let [ent-id (:id ent)
        all-attrs  (vals (:attrs ent))
        relevant-attrs (filter #(filtering-pred %) all-attrs )
        add-in-index-fn (fn [ind attr] (update-attr-in-index ind ent-id (:value attr) order-fn (:value attr) :db/add))
        ] (reduce add-in-index-fn index relevant-attrs)))

(defn- remove-entity-from-index[index ent order-fn filtering-pred]
  (let [ent-id (:id ent)
        all-attrs  (vals (:attrs ent))
        relevant-attrs (filter #(filtering-pred %) all-attrs )
        remove-from-index-fn (fn [ind attr] ( remove-entries-from-index ind (:value attr) ent-id  order-fn (collify (:value attr)) :db/remove))
        ] (reduce remove-from-index-fn index relevant-attrs)))

;when adding an entity, its attributes' timestamp would be set to be the current one
(defn add-entity [db ent]
  (let [[ent-id next-top] (next-id db ent)
                                 new-ts (next-ts db)
                                 indices (last (:timestamped db))
                                 fixed-ent (assoc ent :id ent-id)
                                 new-eavt (assoc (:EAVT indices) ent-id  (update-creation-ts fixed-ent new-ts) )
                                 new-vaet (add-entity-to-index (:VAET indices) ent va ref?)
                                 new-avet (add-entity-to-index (:AVET indices) ent av indexed?)
                                 new-indices (assoc indices :VAET new-vaet :EAVT new-eavt :AVET new-avet )
                                ](assoc db :timestamped  (conj (:timestamped db) new-indices)
                                                 :top-id next-top)))

(defn add-entities [db ents-seq]
  (reduce add-entity db ents-seq))

(defn remove-entity[db ent-id]
  (let [ent (entity-at db ent-id)
         indices (last (:timestamped db))
        vaet (remove-entity-from-index (:VAET indices) ent va ref? )
        ;vaet (update-vaet  (:VAET indices) ent disj)
       avet (remove-entity-from-index (:AVET indices) ent av indexed?)
        ;avet (update-avet  (:AVET indices) ent disj)
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

(defn- update-attr-modification-time [attr new-ts]
    (assoc attr :ts new-ts :prev-ts ( :ts attr)))

(defn- update-attr [attr new-val new-ts operation]
  (-> attr
      (update-attr-modification-time new-ts)
      (update-attr-value new-val operation)))

(defn- remove-path-values [old-vals target-val operation]
  (cond
   (= operation :db/add) []
   (= operation :db/reset-to) old-vals
   (= operation :db/remove) (collify target-val)))

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
          new-vaet (update-index-for-datom (:VAET indices) ent-id attr new-val operation va ref?) ; intentionally attr is passed and not updated-attr, we need to use the old value to locate where to update in the index
          new-avet (update-index-for-datom (:AVET indices) ent-id attr new-val operation av indexed?) ; intentionally attr is passed and not updated-attr, we need to use the old value to locate where to update in the index
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
          (recur (conj res {(:ts attr) (:value attr)})  (:prev-ts attr))))))

(defn db-before
  "How the db was before a given timestamp"
  [db ts]
  (let [indices-before (subvec (:timestamped db) 0 ts )]
    (assoc db :timestamped indices-before :curr-time ts)))

(defn ind-at
  "inspecting a specific index at a given time. The kind argument may be of of these:  :AVET :EAVT :VAET "
  [db ts kind]
  (kind ((:timestamped db) ts)))
