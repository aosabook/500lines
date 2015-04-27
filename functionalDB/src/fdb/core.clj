(ns fdb.core
  [:use [fdb storage query constructs]]
  [:require [clojure.set :as CS :only (union difference intersection)]])

(defn- next-ts [db] (inc (:curr-time db)))

(defn- next-id
  "returns a pair composed of the id to use for the given entity and the next free running id in the database"
  [db ent]
  (let [top-id (:top-id db)
        ent-id (:id ent)
        increased-id (inc top-id)]
        (if (= ent-id :db/no-id-yet)
            [(keyword (str increased-id)) increased-id]
            [ent-id top-id])))

(defn- update-attr-value
  "updating the attribute value based on the kind of the operation, the cardinality defined for this attribute and the given value"
  [attr value operation]
  (cond
      (single? attr)    (assoc attr :value #{value})
   ; now we're talking about an attribute of multiple values
      (= :db/reset-to operation)  (assoc attr :value value)
      (= :db/add operation)        (assoc attr :value (CS/union (:value attr)  value))
      (= :db/remove operation)  (assoc attr :value (CS/difference (:value attr) value))))

(defn- update-creation-ts
  "updates the timestamp value of all the attributes of an entity to the given timestamp"
  [ent ts-val]
 (reduce #(assoc-in %1 [:attrs %2 :ts ] ts-val) ent (keys (:attrs ent))))

(defn- update-entry-in-index [index path operation]
  (let [update-path (butlast path)
        update-value (last path)
        to-be-updated-set (get-in index update-path #{})]
    (assoc-in index update-path (conj to-be-updated-set update-value) )))

(defn- update-attr-in-index [index ent-id attr-name target-val operation]
  (let [colled-target-val (collify target-val)
        update-entry-fn (fn [indx vl] (update-entry-in-index indx ((from-eav index) ent-id attr-name vl) operation))]
    (reduce update-entry-fn index colled-target-val)))

(defn- add-entity-to-index [ent layer ind-name]
  (let [ent-id (:id ent)
        index (ind-name layer)
        all-attrs  (vals (:attrs ent))
        relevant-attrs (filter #((usage-pred index) %) all-attrs )
        add-in-index-fn (fn [ind attr] (update-attr-in-index ind ent-id (:name attr) (:value attr) :db/add))]
       (assoc layer ind-name  (reduce add-in-index-fn index relevant-attrs))))

(defn- fix-new-entity [db ent]
      (let [[ent-id next-top-id] (next-id db ent)
            new-ts (next-ts db)]
      [(update-creation-ts (assoc ent :id ent-id) new-ts) next-top-id]))

(defn add-entity [db ent]
  (let [[fixed-ent next-top-id] (fix-new-entity db ent)
        layer-with-updated-storage (update-in  (last (:layers db)) [:storage] write-entity fixed-ent)
        add-fn (partial add-entity-to-index fixed-ent)
        new-layer (reduce add-fn layer-with-updated-storage  (indices))]
    (assoc db :layers  (conj (:layers db) new-layer) :top-id next-top-id)))

(defn add-entities [db ents-seq] (reduce add-entity db ents-seq))

(defn- update-attr-modification-time  [attr new-ts]  (assoc attr :ts new-ts :prev-ts (:ts attr)))

(defn- update-attr [attr new-val new-ts operation]
   {:pre  [(if (single? attr)
           (contains? #{:db/reset-to :db/remove} operation)
           (contains? #{:db/reset-to :db/add :db/remove} operation))]}
   (-> attr
      (update-attr-modification-time new-ts)
      (update-attr-value new-val operation)))

(defn- remove-entry-from-index [index path]
  (let [path-head (first path)
        path-to-items (butlast path)
        val-to-remove (last path)
        old-entries-set (get-in index path-to-items)]
    (cond
     (not (contains?  old-entries-set val-to-remove)) index ; the set of items does not contain the item to remove, => nothing to do here
     (= 1 (count old-entries-set))  (update-in index [path-head] dissoc (second path)) ; a path that splits at the second item - just remove the unneeded part of it
     :else (update-in index path-to-items disj val-to-remove))))

(defn- remove-entries-from-index [ent-id operation index attr]
  (if (= operation :db/add)
       index
      (let  [attr-name (:name attr)
             datom-vals (collify (:value attr))
             paths (map #((from-eav index) ent-id attr-name %) datom-vals)]
       (reduce remove-entry-from-index index paths))))

(defn- update-index [ent-id old-attr target-val operation layer ind-name]
  (if-not ((usage-pred (get-in layer [ind-name])) old-attr)
    layer
    (let [index (ind-name layer)
          cleaned-index (remove-entries-from-index  ent-id operation index old-attr)
          updated-index  ( if  (= operation :db/remove)
                                         cleaned-index
                                         (update-attr-in-index cleaned-index ent-id  (:name old-attr) target-val operation))]
      (assoc layer ind-name updated-index))))

(defn- put-entity [storage e-id new-attr]
  (assoc-in (get-entity storage e-id) [:attrs (:name new-attr)] new-attr))

(defn- update-layer
  [layer ent-id old-attr updated-attr new-val operation]
  (let [storage (:storage layer)
        new-layer (reduce (partial update-index  ent-id old-attr new-val operation) layer (indices))]
    (assoc new-layer :storage (write-entity storage (put-entity storage ent-id updated-attr)))))

(defn update-entity
  ([db ent-id attr-name new-val]
   (update-entity db ent-id attr-name new-val :db/reset-to ))
  ([db ent-id attr-name new-val operation]
     (let [update-ts (next-ts db)
           layer (last (:layers db))
           attr (attr-at db ent-id attr-name)
           updated-attr (update-attr attr new-val update-ts operation)
           fully-updated-layer (update-layer layer ent-id attr updated-attr new-val operation)]
       (update-in db [:layers] conj fully-updated-layer))))

(defn- remove-entity-from-index [ent layer ind-name]
  (let [ent-id (:id ent)
        index (ind-name layer)
        all-attrs  (vals (:attrs ent))
        relevant-attrs (filter #((usage-pred index) %) all-attrs )
        remove-from-index-fn (partial remove-entries-from-index  ent-id  :db/remove)]
    (assoc layer ind-name (reduce remove-from-index-fn index relevant-attrs))))

(defn- reffing-to [e-id layer]
  (let [vaet (:VAET layer)]
        (for [[attr-name reffing-set] (e-id vaet)
              reffing reffing-set]
             [reffing attr-name])))

(defn- remove-back-refs [db e-id layer]
  (let [reffing-datoms (reffing-to e-id layer)
        remove-fn (fn[d [e a]] (update-entity db e a e-id :db/remove))
        clean-db (reduce remove-fn db reffing-datoms)]
    (last (:layers clean-db))))

(defn remove-entity [db ent-id]
  (let [ent (entity-at db ent-id)
        layer (remove-back-refs db ent-id (last (:layers db)))
        retimed-layer (update-in layer [:VAET] dissoc ent-id)
        no-ent-layer (assoc retimed-layer :storage (drop-entity (:storage retimed-layer) ent))
        new-layer (reduce (partial remove-entity-from-index ent) no-ent-layer (indices))]
    (assoc db :layers (conj  (:layers db) new-layer))))

(defn transact-on-db [initial-db ops]
    (loop [[op & rst-ops] ops transacted initial-db]
      (if op
          (recur rst-ops (apply (first op) transacted (rest op)))
          (let [initial-layer  (:layers initial-db)
                new-layer (last (:layers transacted))]
            (assoc initial-db :layers (conj  initial-layer new-layer) :curr-time (next-ts initial-db) :top-id (:top-id transacted))))))

(defmacro  _transact [db exec-fn & ops]
    (when ops
      (loop [[frst-op# & rst-ops#] ops  res#  [exec-fn db `transact-on-db]  accum-ops# []]
        (if frst-op#
           (recur rst-ops# res#  (conj  accum-ops#  (vec frst-op#)))
           (list* (conj res#  accum-ops#))))))

(defn- _what-if
  "Operates on the db with the given transactions, but without eventually updating it"
  [db f ops] (f db ops))

(defmacro what-if [db & ops]  `(_transact ~db _what-if  ~@ops))
(defmacro transact [db-conn & ops]  `(_transact ~db-conn swap! ~@ops))

(defn evolution-of
  "The sequence of the values of an entity's attribute, as changed through time"
  [db ent-id attr-name]
  (loop [res [] ts (:curr-time db)]
    (if (= -1 ts) (reverse res)
        (let [attr (attr-at db ent-id attr-name ts)]
          (recur (conj res {(:ts attr) (:value attr)})  (:prev-ts attr))))))
