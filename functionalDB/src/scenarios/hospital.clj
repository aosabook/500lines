(ns core.tmpTst
  [:use core.fdb]
  [:require [core.manage :as M]
             ])


(def hospital-db (M/get-db-conn "hospital-db"))


(def sis (make-entity :test/db-systolic))
(def dias (make-entity :test/db-diastolic))
(def temp (make-entity :test/temperature))
(def person (make-entity :person/patient))
(def doctor (make-entity :person/doctor))

(def pat1 (-> (make-entity :pat1 )
                     (add-attr (make-attr :patient/kind :person/patient :REF ))
                     (add-attr (make-attr :patient/systolic 120 :number ))
                     (add-attr (make-attr :patient/diastolic 80 :number ))
                     (add-attr (make-attr :patient/temperature 37 :number ))
                     (add-attr (make-attr :patient/city "London":string :db/single true))
              )

  (def pat2 (-> (make-entity :__pat2 )
                     (add-attr (make-attr :patient/kind :person/patient :REF ))
                     (add-attr (make-attr :patient/systolic 110 :number ))
                     (add-attr (make-attr :patient/diastolic 70 :number ))
                     (add-attr (make-attr :patient/temperature 38 :number ))
                     (add-attr (make-attr :patient/city "New York":string :db/single true))
              )

;; world setup
(transact hospital-db (add-entity sis)  (add-entity dias) (add-entity temp) (add-entity person) (add-entity doctor))
(transact hospital-db (add-entity pat1) (add-entity pat2))

