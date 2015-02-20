(ns fdb.constructs
   (:use fdb.storage)
  (:import [fdb.storage.InMemory]))

(defrecord Database [layers top-id curr-time])
(defrecord Layer [storage VAET AVET VEAT EAVT])
(defrecord Entity [id attrs])
(defrecord Attr [name value ts prev-ts])

(defn make-index
  "An index is a tree, implemented by nested maps, each level corresponds to either entity-id, attribute name or a value, where the leaves of the tree are sets.
  The order of the levels changes from one index to another, to allow different querying on different indices. It is possible to reorder a path in the tree to an EAV structure
  using the to-eav function, or transform an EAV to a specific index path using the from-eav, both function reside in the metadata of the index.  The usage-pred is a predicate to decide whether to index a specific attribute in an entity or not."
  [from-eav to-eav usage-pred]
   (with-meta {} {:from-eav from-eav :to-eav to-eav :usage-pred usage-pred}))

(defn from-eav [index] (:from-eav (meta index)))
(defn to-eav [index] (:to-eav (meta index)))
(defn usage-pred [index] (:usage-pred (meta index)))

(defn single? [attr] (= :db/single (:cardinality (meta attr))))

(defn ref? [attr] (= :db/ref (:type (meta attr))))
(defn always[& more] true)

(defn make-db
  "Create an empty database"
  []
  (atom (Database. [(Layer.
                     (fdb.storage.InMemory.) ; storage
                     (make-index #(vector %3 %2 %1) #(vector %3 %2 %1) #(ref? %)) ; VAET - for graph queries and joins
                     (make-index #(vector %2 %3 %1) #(vector %3 %1 %2) always) ; AVET - for filtering
                     (make-index #(vector %3 %1 %2) #(vector %2 %3 %1) always) ; VEAT - for filtering
                     (make-index #(vector %1 %2 %3) #(vector %1 %2 %3) always) )] ; EAVT - for filtering
                   0 0)))
(defn entity-at
  "the entity with the given ent-id at the given time (defaults to the latest time)"
  ([db ent-id] (entity-at db (:curr-time db) ent-id))
  ([db ts ent-id] (get-entity (get-in db [:layers ts :storage]) ent-id)))

(defn attr-at
  "The attribute of an entity at a given time (defaults to recent time)"
  ([db ent-id attr-name] (attr-at db ent-id attr-name (:curr-time db)))
  ([db ent-id attr-name ts]
   (get-in (entity-at db ts ent-id) [:attrs attr-name])))

(defn value-of-at
  "value of a datom at a given time, if no time is provided, we default to the most recent value"
  ([db ent-id attr-name]  (:value (attr-at db ent-id attr-name)))
  ([db ent-id attr-name ts] (:value (attr-at db ent-id attr-name ts))))

(defn indx-at
  "inspecting a specific index at a given time, defaults to current. The kind argument mayone of the index name (e.g., AVET)"
  ([db kind]
   (indx-at db kind  (:curr-time db)))
  ([db kind ts]
   (kind ((:layers db) ts))))

(defn collify [x] (if (coll? x) x [x]))
(defn indices[] [:VAET :AVET :VEAT :EAVT])

(defn make-entity
  "creates an entity, if id is not supplied, a running id is assigned to the entity"
  ([] (make-entity :db/no-id-yet ))
  ([id] (Entity.  id {})))

(defn make-attr
  "creation of an attribute. The name, value and type of an attribute are mandatory arguments, further arguments can be passed as named arguments.
   The type of the attribute may be either :string, :number, :boolean or :db/ref . If the type is :db/ref, the value is an id of another entity and indexing of backpointing is maintained.
  The named arguments are as follows:
  :cardinality - the cardinality of an attribute, can be either:
                     :db/single - which means that this attribute can be a single value at any given time (this is the default cardinality)
                     :db/multiple - which means that this attribute is actually a set of values. In this case updates of this attribute may be one of the following (NOTE that all these operations accept a set as argument):
                                          :db/add - adds a set of values to the currently existing set of values
                                          :db/reset-to - resets the value of this attribute to be the given set of values
                                          :db/remove - removes the given set of values from the attribute's current set of values"
  ([name value type ; these ones are required
      & {:keys [cardinality] :or {cardinality :db/single}} ]
    {:pre [(contains? #{:db/single :db/multiple} cardinality)]}
   (with-meta (Attr. name value -1 -1) {:type type :cardinality cardinality} )))

(defn add-attr
  "adds an attribute to an entity"
  [ent attr]
  (let [attr-id (keyword (:name attr))]
     (assoc-in ent [:attrs attr-id] attr)))
