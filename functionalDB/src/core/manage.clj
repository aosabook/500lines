(ns tmpTst (:use core.fdb))

(def all-dbs (atom {}))

(defn _get-db [dbs db-name]
  (if (db-name dbs ) dbs (assoc dbs db-name (make-db))))

(defn get-db-conn [db-name]
  ((keyword db-name) (swap! all-dbs _get-db (keyword db-name))))

(defn db-from-conn [conn] @conn)
