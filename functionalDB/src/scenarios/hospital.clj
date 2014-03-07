(ns scenarios.hospital
  (:use core.fdb)
  [:require [core.manage :as M]])


(M/reset-db-conn "hospital-db")

(def hospital-db (M/get-db-conn "hospital-db"))


(def sis (make-entity :test/db-systolic))
(def dias (make-entity :test/db-diastolic))
(def temp (make-entity :test/temperature))
(def person (make-entity :person/patient))
(def doctor (make-entity :person/doctor))

;; (def pat1 (-> (make-entity :__pat1 )
;;                      (add-attr (make-attr :patient/kind :person/patient :db/ref ))
;;                      (add-attr (make-attr :patient/systolic 120 :number ))
;;                      (add-attr (make-attr :patient/diastolic 80 :number ))
;;                      (add-attr (make-attr :patient/temperature 37 :number ))
;;                      (add-attr (make-attr :patient/city "London":string :indexed true))
;;                      (add-attr (make-attr :patient/symptoms #{"fever" "cough"}:string :cardinality :db/multiple))
;;               ))

;;   (def pat2 (-> (make-entity :__pat2 )
;;                      (add-attr (make-attr :patient/kind :person/patient :db/ref ))
;;                      (add-attr (make-attr :patient/systolic 110 :number ))
;;                      (add-attr (make-attr :patient/diastolic 70 :number ))
;;                      (add-attr (make-attr :patient/temperature 38 :number ))
;;                      (add-attr (make-attr :patient/city "New York":string :indexd true))
;;               ))

;; world setup

(defn make-patient[id address symptoms]
  (-> (make-entity id)
        (add-attr (make-attr :patient/kind :person/patient :db/ref))
        (add-attr (make-attr :patient/city address :string :indexed true))
        (add-attr (make-attr :patient/tests [] :db/ref :indexed true :cardinality :db/multiple))
        (add-attr (make-attr :patient/symptoms (set symptoms) :string :cardinality :db/multiple))))

(defn add-patient [id address symptoms]
  (transact hospital-db (add-entity (make-patient id address symptoms))))


(transact hospital-db (add-entity sis)  (add-entity dias) (add-entity temp) (add-entity person) (add-entity doctor))
@hospital-db
(add-patient :pat1 "London" ["fever" "cough"] )
;(add-patient :pat2 "New York" ["Tremor" "dizziness"])

;(transact hospital-db (add-entity pat1) (add-entity pat2))
(transact hospital-db (update-datom :pat1 :patient/symptoms #{"cold sweat" "sneeze"} :db/add))
(evolution-of (M/db-from-conn hospital-db) :pat1 :patient/symptoms)


