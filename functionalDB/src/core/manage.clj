(ns core.manage
  "Management of db connections (from the user's perspective), internally creates/ / drops dbs."
  (:use core.constructs))

(def ^:private __ALL-DBS__ (atom {}))

(defn- force-get-db  [dbs db-name] (if (db-name dbs)  dbs  (assoc dbs db-name (make-db))))

(defn- drop-db [dbs db-name] (dissoc dbs db-name))

(defn- as-db-name [db-name] (keyword db-name))

(defn get-db-conn [db-name]
  (let [stored-db-name (as-db-name db-name)]
  (stored-db-name (swap! __ALL-DBS__ force-get-db stored-db-name))))

(defn drop-db-conn [db-name]
  (let [stored-db-name (as-db-name db-name)]
  (swap! __ALL-DBS__ drop-db stored-db-name)) nil)

(defn db-from-conn [conn] @conn)