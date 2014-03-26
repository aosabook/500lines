(ns core.manage
  "Management of db and db connections via this module it is possible to either acquire new db or reset an existing one, as well as get the db value from a connection"
  (:use core.fdb))

(def __ALL-DBS__ (atom {}))

(defn _get-db [dbs db-name]
  (if (db-name dbs)  dbs  (assoc dbs db-name (make-db))))

(defn _reset-db[dbs db-name] (dissoc dbs db-name))

(defn _as-db-name [db-name] (keyword db-name))

(defn get-db-conn [db-name]
  (let [stored-db-name (_as-db-name db-name)]
  (stored-db-name (swap! __ALL-DBS__ _get-db stored-db-name))))

(defn reset-db-conn [db-name]
  (let [stored-db-name (_as-db-name db-name)]
  (swap! __ALL-DBS__ _reset-db stored-db-name)) nil)

(defn db-from-conn [conn] @conn)
