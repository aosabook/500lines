(ns fdb.graph
  (:use fdb.constructs))

(defn outgoing-refs [db ts ent-id]
  (if-not ent-id []
    (->> (entity-at db ts ent-id)
          (:attrs) (vals) (filter ref?) (mapcat :value))))

(defn incoming-refs [db ts ent-id]
  (let [vaet (ind-at db :VAET ts)]
      (reduce into #{} (vals (vaet ent-id)))))

(defn- remove-explored [pendings explored restruct-fn]
  (restruct-fn (remove #(contains? explored %) pendings)))

(defn- traverse [pendings explored exploring-fn ent-at restruct-fn]
    (let [cleaned-pendings (remove-explored pendings explored restruct-fn)
          item (first cleaned-pendings)
          all-next-items  (exploring-fn item)
          next-pends (reduce conj (restruct-fn (rest cleaned-pendings)) all-next-items)]
      (when item (cons  (ent-at item)
                                   (lazy-seq (traverse next-pends (conj explored item) exploring-fn ent-at restruct-fn))))))

(defn traverse-db
  ([start-ent-id db algo direction] (traverse-db start-ent-id db algo direction (:curr-time db)))
  ([start-ent-id db algo direction ts]
   (let [pend-struct (if (= :graph/bfs algo) vec list*)
          explore-fn (if (= :graph/outgoing direction) outgoing-refs incoming-refs)]
     (traverse [start-ent-id] #{}  (partial explore-fn db ts) (partial entity-at db ts) pend-struct))))
