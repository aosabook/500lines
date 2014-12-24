(ns fdb.graph
  (:use fdb.constructs))

(defn outgoing-refs [db ts ent-id & ref-names]
  (let [val-filter-fn (if ref-names #(vals (select-keys ref-names %)) vals)]
  (if-not ent-id []
    (->> (entity-at db ts ent-id)
          (:attrs) (val-filter-fn) (filter ref?) (mapcat :value)))))

(defn incoming-refs [db ts ent-id & ref-names]
  (let [vaet (ind-at db :VAET ts)
          all-attr-map (vaet ent-id)
          filtered-map (if ref-names (select-keys ref-names all-attr-map) all-attr-map)]
      (reduce into #{} (vals filtered-map))))

(defn- remove-explored [candidates explored structure-fn]
  (structure-fn (remove #(contains? explored %) candidates)))

(defn- traverse [pendings explored exploring-fn ent-at structure-fn]
    (let [cleaned-pendings (remove-explored pendings explored structure-fn)
          item (first cleaned-pendings)
          all-next-items  (exploring-fn item)
          next-pends (reduce conj (structure-fn (rest cleaned-pendings)) all-next-items)]
      (when item (cons  (ent-at item)
                                   (lazy-seq (traverse next-pends (conj explored item) exploring-fn ent-at structure-fn))))))

(defn traverse-db
  ([start-ent-id db algo direction] (traverse-db start-ent-id db algo direction (:curr-time db)))
  ([start-ent-id db algo direction ts]
   (let [structure-fn (if (= :graph/bfs algo) vec list*)
          explore-fn (if (= :graph/outgoing direction) outgoing-refs incoming-refs)]
     (traverse [start-ent-id] #{}  (partial explore-fn db ts) (partial entity-at db ts) structure-fn))))

(defn evolution-of
  "The sequence of the values of an entity's attribute, as changed through time"
  [db ent-id attr-name]
  (loop [res [] ts (:curr-time db)]
    (if (= -1 ts) (reverse res)
        (let [attr (attr-at db ent-id attr-name ts)]
          (recur (conj res {(:ts attr) (:value attr)})  (:prev-ts attr))))))
