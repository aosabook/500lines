(ns core.storage)

(defn initial-storage [] {})

(defn stored-entity [storage e-id] (e-id storage))

(defn update-storage [storage entity] (assoc storage (:id entity) entity))

(defn remove-entity-from-storage [storage entity] (dissoc storage (:id entity)))

