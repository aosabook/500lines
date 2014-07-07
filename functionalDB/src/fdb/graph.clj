(ns fdb.graph
  (:use fdb.constructs))

(defn outgoing-refs [db ts ent-id]
  (if ent-id
    (->> (entity-at db ts ent-id)
          (:attrs) (vals) (filter ref?) (mapcat  (comp collify :value)))
    []))

(defn incoming-refs [db ts ent-id]
  (let [vaet (ind-at db :VAET ts)]
      (reduce into #{} (vals (vaet ent-id)))))

(defn- remove-explored [pendings explored restruct-fn]
  (if (contains? explored (first pendings))
    (recur (rest pendings) explored restruct-fn)
      (restruct-fn pendings)))

(defn- traverse [pendings explored exploring-fn ent-at restruct-fn]
    (let [cleaned-pendings (remove-explored pendings explored restruct-fn)
          item (first cleaned-pendings)
          all-next-items  (exploring-fn item)
          next-pends (reduce conj (restruct-fn (rest cleaned-pendings)) all-next-items)]
      (when item (cons  (ent-at item)
                                   (lazy-seq
                                      (traverse next-pends  (conj explored item) exploring-fn ent-at restruct-fn))))))

(defn traverse-db
  ([start-ent-id db algo direction] (traverse-db start-ent-id db algo direction (:curr-time db)))
  ([start-ent-id db algo direction ts]
   (let [pend-struct (if (= :bfs algo) vec list*)
         explor-fn (if (= :outgoing direction) outgoing-refs incoming-refs)]
     (traverse [start-ent-id] #{}  (partial explor-fn db ts) (partial entity-at db ts) pend-struct))))
