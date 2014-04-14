(ns core.fdb
  [:use [core storage query constructs]]
  [:require [clojure.set :as CS :only (union difference intersection)]])

(defn collify [x] (if (coll? x) x [x]))

(defn next-ts [db] (inc (:curr-time db)))

(defn next-id
  "returns a pair composed of the id to use for the given entity and the next free running id in the database"
  [db ent]
  (let [top-id (:top-id db)
          ent-id (:id ent)
          inceased-id (inc top-id)
          [id-to-use next-top] (if (= ent-id :db/no-id-yet)
                                             [(keyword (str inceased-id)) inceased-id]
                                             [ent-id top-id])]
  [id-to-use next-top]))

(defn entity-at
  "the entity with the given ent-id at the given time (defualts to the latest time)"
  ([db ent-id] (entity-at db ent-id (:curr-time db)))
  ([db ent-id ts] (stored-entity (get-in db [:timestamped ts :storage]) ent-id)))

(defn attr-at
  "The attribute of an entity at a given time (defaults to recent time)"
  ([db ent-id attr-name]
   (attr-at db ent-id attr-name (:curr-time db)))
  ([db ent-id attr-name ts]
  (get-in (entity-at db ent-id ts) [:attrs attr-name])))

(defn value-of-at
  "value of a datom at a given time, if no time is provided, we default to the most recent value"
  ([db ent-id attr-name]  (:value (attr-at db ent-id attr-name)))
  ([db ent-id attr-name ts] (:value (attr-at db ent-id attr-name ts))))

(defn update-attr-value
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

(defn update-creation-ts
  "updates the timestamp value of all the attributes of an entity to the given timestamp"
  [ent ts-val]
 (reduce #(assoc-in %1 [:attrs %2 :ts ] ts-val) ent (keys (:attrs ent))))

(defn add-entry-to-index [index path operation]
  (if (= operation :db/remove)
      index
    (let [update-path (butlast path)
            update-value (last path)
            to-be-updated-set (get-in index update-path #{})]
      (assoc-in index update-path (conj to-be-updated-set update-value) ))))

(defn update-attr-in-index [index ent-id attr-name target-val operation]
  (let [ colled-target-val (collify target-val)
         add-entry-fn (fn [indx vl] (add-entry-to-index indx ((from-eav index) ent-id attr-name vl) operation))]
    (reduce add-entry-fn index colled-target-val)))

(defn add-entity-to-index  [ent timestamped ind-name]
  (let [ent-id (:id ent)
        index (ind-name timestamped)
        all-attrs  (vals (:attrs ent))
        relevant-attrs (filter #((usage-pred index) %) all-attrs )
        add-in-index-fn (fn [ind attr] (update-attr-in-index ind ent-id (:name attr) (:value attr) :db/add))]
       (assoc timestamped ind-name  (reduce add-in-index-fn index relevant-attrs))))

(defn fix-new-entity [db ent]
      (let [[ent-id next-top-id] (next-id db ent)
              new-ts (next-ts db)]
      [(update-creation-ts (assoc ent :id ent-id) new-ts) next-top-id]))

(defn add-entity  [db ent]
  (let [[fixed-ent next-top-id](fix-new-entity db ent)
          new-timestamped (update-in  (last (:timestamped db)) [:storage] update-storage fixed-ent)
          add-fn (partial add-entity-to-index fixed-ent)
          new-timestamped (reduce add-fn new-timestamped  (indices))]
    (assoc db :timestamped  (conj (:timestamped db) new-timestamped) :top-id next-top-id)))

(defn add-entities  [db ents-seq] (reduce add-entity db ents-seq))

(defn update-attr-modification-time  [attr new-ts]  (assoc attr :ts new-ts :prev-ts ( :ts attr)))

(defn update-attr [attr new-val new-ts operation]
   {:pre  [(if (single? attr)
           (contains? #{:db/reset-to :db/remove} operation)
           (contains? #{:db/reset-to :db/add :db/remove} operation))]}
   (-> attr
      (update-attr-modification-time new-ts)
      (update-attr-value new-val operation)))

(defn remove-path-values [old-vals target-val operation]
  (cond
   (= operation :db/add) [] ; nothing to remove
   (= operation :db/reset-to) old-vals ; removing all of the old values
   (= operation :db/remove) (collify target-val))) ; removing the values defined by the caller

(defn remove-entry-from-index [index path]
  (let [path-head (first path)
          path-to-items (butlast path)
          val-to-remove (last path)
          old-entries-set (get-in index path-to-items)]
    (cond
     (not (contains?  old-entries-set val-to-remove)) index ; the set of items does not contain the item to remove, => nothing to do here
     (and (= 1 (count old-entries-set) ) (= 1 (count (index path-head)))) (dissoc index path-head) ; a path representing a single EAV - remove it entirely
     (= (count old-entries-set) 1)  (update-in index [path-head] dissoc (second path)) ; a path that splits at the second item - just remove the unneeded part of it
     :else (update-in index path-to-items disj val-to-remove))))

(defn remove-entries-from-index [ent-id operation index attr]
  (if (= operation :db/add)
       index
      (let  [attr-name (:name attr)
               datom-vals (collify (:value attr))
               paths (map #((from-eav index) ent-id attr-name %) datom-vals)]
       (reduce remove-entry-from-index index paths))))

(defn update-index [ent-id old-attr target-val operation timestamped ind-name]
  (if-not ((usage-pred (get-in timestamped [ind-name])) old-attr)
    timestamped
    (let [index (ind-name timestamped)
          old-vals (:value old-attr)
          attr-name (:name old-attr)
          remove-paths  (remove-path-values old-vals target-val operation)
          cleaned-index (remove-entries-from-index  ent-id operation index old-attr)
          updated-index  (update-attr-in-index cleaned-index ent-id attr-name target-val operation)]
      (assoc timestamped ind-name updated-index))))

(defn update-entity [storage e-id new-attr]
  (assoc-in (stored-entity storage e-id) [:attrs (:name new-attr)] new-attr))

(defn update-timestamped
  [timestamped ent-id old-attr updated-attr new-val operation]
  (let [storage (:storage timestamped)
         new-timestamped (reduce (partial update-index  ent-id old-attr new-val operation) timestamped (indices))]
    (assoc new-timestamped :storage (update-storage storage (update-entity storage ent-id updated-attr)))))

(defn update-datom
  ([db ent-id attr-name new-val]
   (update-datom db ent-id attr-name  new-val  :db/reset-to ))
  ([db ent-id attr-name  new-val operation ]
     (let [update-ts (next-ts db)
            timestamped (last (:timestamped db))
            attr (attr-at db ent-id attr-name)
            updated-attr (update-attr attr new-val update-ts operation)
            fully-updated-timestamped (update-timestamped timestamped ent-id attr updated-attr new-val operation)]
       (update-in db [:timestamped] conj fully-updated-timestamped))))

(defn remove-entity-from-index [ent timestamped ind-name]
  (let [ent-id (:id ent)
        index (ind-name timestamped)
        all-attrs  (vals (:attrs ent))
        relevant-attrs (filter #((usage-pred index) %) all-attrs )
        remove-from-index-fn (partial remove-entries-from-index  ent-id  :db/remove)]
    (assoc timestamped ind-name (reduce remove-from-index-fn index relevant-attrs))))

(defn reffing-datoms-to [e-id timestamped]
  (let [vaet (:VAET timestamped)]
        (for [[attr-name reffing-set] (e-id vaet)
                reffing reffing-set]
               [reffing attr-name e-id])))

(defn remove-back-refs [db e-id timestamped]
  (let [refing-datoms (reffing-datoms-to e-id timestamped)
          remove-fn (fn[d [e a v]] (update-datom db e a v :db/remove))
          clean-db (reduce remove-fn db refing-datoms)]
       (last (:timestamped db))))

(defn remove-entity  [db ent-id]
  (let [ent (entity-at db ent-id)
         timestamped (remove-back-refs db ent-id (last (:timestamped db)))
         retimed-timestamped (update-in timestamped [:VAET] dissoc ent-id)
         no-ent-timestamped (assoc retimed-timestamped :storage (remove-entity-from-storage (:storage retimed-timestamped) ent))
         new-timestamped (reduce (partial remove-entity-from-index ent) no-ent-timestamped (indices))]
    (assoc db :timestamped (conj  (:timestamped db) new-timestamped))))

(defn transact-on-db [initial-db  txs]
    (loop [[tx & rst-tx] txs transacted initial-db]
      (if tx
          (recur rst-tx (apply (first tx) transacted (rest tx)))
          (let [initial-timestamped  (:timestamped initial-db)
                  new-timestamped (last (:timestamped transacted))]
            (assoc initial-db :timestamped (conj  initial-timestamped new-timestamped)  :curr-time (next-ts initial-db) :top-id (:top-id transacted))))))

(defmacro  _transact [db op & txs]
  (when txs
    (loop [[frst-tx# & rst-tx#] txs  res#  [op db `transact-on-db]  accum-txs# []]
      (if frst-tx#
          (recur rst-tx# res#  (conj  accum-txs#  (vec  frst-tx#)))
          (list* (conj res#  accum-txs#))))))

(defn _what-if
  "Operates on the db with the given transactions, but without eventually updating it"
  [ db f  txs]
  (f db txs))

(defmacro what-if [db & txs]  `(_transact ~db   _what-if  ~@txs))

(defmacro transact [db & txs]  `(_transact ~db swap! ~@txs))

(defmacro q
  "querying the database using datalog queries built in a map structure ({:find [variables*] :where [ [e a v]* ]}).
  At the moment support only filtering queries, no joins is also assumed."
  [db query]
  `(let [query#  (q-clauses ~(:where query)) ; extracting the query clauses from the query
           ind# (choose-index ~db query#) ; selecting which index to use
           q-res# (query-index ind# query#) ; actually quering, from now on just preparing results
           binded-res# (bind-variables-to-query q-res# ind#) ; building a meta + real results structure
           needed-vars# (settify  ~(:find query))] ; extracting out the needed to be reported variables
     (map (partial locate-vars-in-query-res needed-vars# ) binded-res#))) ; picking the needed variables from the query result

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
  (let [timestamped-before (subvec (:timestamped db) 0 ts )]
    (assoc db :timestamped timestamped-before :curr-time ts)))
