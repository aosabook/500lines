(ns scenarios.hospital
  (:use [core fdb query constructs] )
  [:require [core.manage :as M]
                [clojure.set     :as CS :only (union difference )]])

(def db-name "hos12")
(M/reset-db-conn db-name)

(def hospital-db (M/get-db-conn db-name))

(def basic-kinds [:test/bp-systolic :test/bp-diastolic :test/temperature :person/patient :person/doctor ] )

(defn make-patient[id address symptoms]
  (-> (make-entity id)
        (add-attr (make-attr :patient/kind :person/patient :db/ref))
        (add-attr (make-attr :patient/city address :string ))
        (add-attr (make-attr :patient/tests #{} :db/ref :indexed true :cardinality :db/multiple))
        (add-attr (make-attr :patient/symptoms (set symptoms) :string :cardinality :db/multiple))))

(defn make-test [t-id tests-map types ]
  (let [ent (make-entity t-id)]
           (reduce #(add-attr %1 (make-attr (first %2) ;attr-name
                                                               (second %2) ; attr value
                                                               (get types (first %2) :number) ; attr type
                                                                :indexed true ));indexed
                  ent tests-map)))

(defn add-patient [id address symptoms]
  (transact hospital-db (add-entity (make-patient id address symptoms))))

(defn add-test-results-to-patient [pat-id test-result]
  (let [test-id (:id test-result)
        a (transact hospital-db  (add-entity test-result))
        ]
   (transact hospital-db  (update-datom pat-id  :patient/tests #{test-id} :db/add))))

;; world setup
(transact hospital-db  (add-entities (map #(make-entity %) basic-kinds )))

(add-patient :pat1 "London" ["fever" "cough"] )

(add-patient :pat2 "London" ["fever" "cough"] )

(add-test-results-to-patient :pat1  (make-test :t2-pat1  {:test/bp-systolic 170 :test/bp-diastolic 80 :test/machine "XXX"} {:test/machine "string"} ))
(add-test-results-to-patient :pat2  (make-test :t4-pat1  {:test/bp-systolic 170 :test/bp-diastolic 90 :test/machine "XYY"} {:test/machine "string"} ))

(transact hospital-db (update-datom :pat1 :patient/symptoms #{"cold sweat" "sneeze"} :db/reset-to))
(transact hospital-db (update-datom :pat1 :patient/tests #{:t2-pat1} :db/remove))
 ;  (transact hospital-db (remove-entity :t2-pat1))

 (defn keep-on-equals [a b](if (= a b) a nil ))

;(def qq(map #(:db/variable (meta %)) (q-clauses [[ ?e :test/bp-systolic (> 150 ?b)][ ?e :test/bp-diastolic ?k]] )))
(ind-at @hospital-db :EAVT)
(q @hospital-db {:find [?e ?k] :where [[ ?e :test/bp-systolic (> 200 ?b)] [ ?e :test/bp-diastolic ?k]]} )

;(def ind (ind-at @hospital-db :VEAT))
;(def q-m
 (q @hospital-db {:find [?a ?b] :where [[ _  ?a (> 200 ?b)] ]})

 (q @hospital-db {:find [?e ?k ] :where [[ ?e :test/bp-systolic (> 180 ?b)][ ?e :test/bp-diastolic ?k] ]})
 ;)
 ;(map (partial vars-in-query-res st ) q-m)


;(vars-in-query-res q-m st)
 ;(def mc (mapcat (partial seqify-result-path ind)  q-res ))
 ;(map (comp(partial apply (:db-to-eav ind))(partial partition 2)) mc)
;(def aa1 (map #(->> %1 (partition 2)(apply (:db-to-eav ind))) mc) )
;(reduce #(assoc-in %1  (butlast %2) (last %2)) {} aa1)
 ;(merge-query-and-meta q-res ind)

(evolution-of (M/db-from-conn hospital-db) :pat1 :patient/symptoms)
(evolution-of (M/db-from-conn hospital-db) :pat1 :patient/tests)

