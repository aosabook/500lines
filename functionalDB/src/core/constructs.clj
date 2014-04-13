(ns core.constructs
   (:use core.storage))

(defrecord Database [timestamped top-id curr-time])
(defrecord Timestamped [storage VAET AVET VEAT EAVT])
(defrecord Entity [id attrs])
(defrecord Attr [name value ts prev-ts])

(defn make-index
  "An index is a tree, implemented by nested maps, each level corresponds to either entity-id, attribute name or a value, where the leaves of the tree are sets.
  The order of the levels changes from one index to another, to allow different quering on different indices. It is possible to reorder a path in the tree to an EAV structure
  using the to-eav function, or transform an EAV to a specific index path using the from-eav, both function reside in the metadataa of the index. The leaf index is the index of
  the leaf element in an EAV triplet. The usage-pred is a predicate to decide whether to index a specific attribute in an entity or not."
  [from-eav to-eav usage-pred leaf-index]
   (with-meta {} {:from-eav from-eav :to-eav to-eav :usage-pred usage-pred :leaf-index leaf-index}))

(defn from-eav [index] (:from-eav (meta index)))
(defn to-eav [index] (:to-eav (meta index)))
(defn usage-pred [index] (:usage-pred (meta index)))
(defn leaf-index [index] (:leaf-index (meta leaf-index)))

(defn single? [attr] (= :db/single (:cardinality (meta attr))))

(defn ref? [attr] (= :db/ref (:type (meta attr))))

(defn make-db
  "Create an empty database"
  []
  (atom (Database. [(Timestamped.
                     (initial-storage) ; storage
                     (make-index #(vector %3 %2 %1) #(vector %3 %2 %1) #(ref? %) 0) ; VAET - for graph qeries and joins
                     (make-index #(vector %2 %3 %1) #(vector %3 %1 %2) #(not (not %)) 0) ; AVET - for filtering
                     (make-index #(vector %3 %1 %2) #(vector %2 %3 %1) #(not (not %)) 1) ; VEAT - for filtering
                     (make-index #(vector %1 %2 %3) #(vector %1 %2 %3) #(not (not %)) 2) )] ; EAVT - for filtering
                   0 0)))

(defn indices[] [:VAET :AVET :VEAT :EAVT])

(defn make-entity
  "creates an entity, if id is not supplied, a running id is assigned to the entity"
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
