(ns fdb.query
   [:use [fdb constructs]
    [clojure.set :as CS :only (intersection)]])

(defn variable?
  "A predicate that accepts a string and checks whether it describes a datalog variable (either starts with ? or it is _)"
  ([x] (variable? x true))
  ([x accept_?]  ; intentionally accepts a string and implemented as function and not a macro so we'd be able to use it as a HOF
  (or (and accept_? (= x "_")) (= (first x) \?))))

(defmacro clause-term-meta
  "Finds the name of the variable at an item of a datalog clause element. If no variable, returning nil"
  [clause-term]
  (cond
   (coll? clause-term)  (first (filter #(variable? % false)  (map str clause-term))) ; the item is an s-expression, need to treat it as a coll, by going over it and returning the name of the variable
   (variable? (str clause-term) false) (str clause-term) ; the item is a simple variable
   :no-variable-in-clause nil)) ; the item is a value and not a variable

(defmacro clause-term-expr
  "Create a predicate for each element in the datalog clause"
  [clause-term]
  (cond
   (variable? (str clause-term)) #(= % %) ; simple one, e.g.,  ?a
   (not (coll? clause-term)) `#(= % ~clause-term) ; simple value given, e.g.,  :likes
   (= 2 (count clause-term)) `#(~(first clause-term) %) ; an unary predicate, e.g.,  (pos? ?a)
   (variable? (str (second clause-term))) `#(~(first clause-term) % ~(last clause-term)) ;a binary predicate, the variable is the first argument, e.g.,  (> ?a 42)
   (variable? (str (last clause-term))) `#(~(first clause-term) ~(second clause-term) %))) ; a binary predicate, the variable is the second argument, e.g.,  (> ?a 42)

(defmacro  pred-clause
  "Builds a predicate-clause from a query clause (a vector with three elements describing EAV). A predicate clause is a vector of predicates that would operate on
    an index, and set for that vector's metadata to be the names of the variables that the user assigned for each item in the clause"
  [clause]
  (loop [[trm# & rst-trm#] clause exprs# [] metas# [] ]
    (if  trm#
         (recur rst-trm# (conj exprs# `(clause-term-expr ~ trm#)) (conj metas#`(clause-term-meta ~ trm#)))
         (with-meta exprs# {:db/variable metas#}))))

 (defmacro q-clauses-to-pred-clauses
   "create a vector of predicate clauses to operate on indices, based on the given vector of clauses"
   [clauses]
    (loop [[frst# & rst#] clauses preds-vecs# []  ]
      (if-not frst#  preds-vecs#
        (recur rst# `(conj ~preds-vecs# (pred-clause ~frst#))) )))

(defn filter-index
  "Function that accepts an index and a path-predicate (which is a tripet of predicates to apply on paths in an index). For each path predicates it creates a result path (a triplet
  representing one path in the index) and returns a seq of result paths."
  [index predicate-clauses]
  (for [ pred-clause predicate-clauses
        :let [[lvl1-prd lvl2-prd lvl3-prd] (apply (from-eav index) pred-clause)] ; predicates for the first and second level of the index, also keeping the path to later use its meta
        [k1 l2map] index  ; keys and values of the first level
        :when (try (lvl1-prd k1) (catch Exception e false))  ; filtering to keep only the keys and the vals of the keys that passed the first level predicate
        [k2  l3-set] l2map  ; keys and values of the second level
        :when (try (lvl2-prd k2) (catch Exception e false)) ; filtering to keep only the keys and vals of keys that passed the second level predicate
        :let [res (set (filter lvl3-prd l3-set))] ]; keep from the set at the third level only the items that passed the predicate on them
          (with-meta [k1 k2  res] (meta pred-clause)))) ; constructed result clause, while keeping the meta of the query to use it later when extracting variables

 (defn items-that-answer-all-conditions
   "takes the sequence of all the items collection, each such collection answered one condition, we test here what are the items that answered all of the conditions
   i.e., what items are found at exactly 'num-of-conditions' of such collections "
   [items-seq num-of-conditions]
   (->> items-seq ; take the items-seq
           (map vec) ; make each collection (actually a set) into a vector
           (reduce into []) ;reduce all the vectors into one big vector
           (frequencies) ; count for each item in how many collections (sets) it was in originally
           (filter #(<= num-of-conditions (last %))) ; keep only the items that answered all of the conditions
           (map first) ; take from the duos the items themselves
           (set))) ; return it as set

(defn mask-path-leaf-with-items
  "Returning the path with only the items found in the intersection of that path's items and the relevant items"
  [relevant-items path]
    (update-in path [2] CS/intersection relevant-items ))

 (defn combine-path-and-meta
   "This function returns for a (result) path a seq of vectors, each vector is a path from the root of the result path to one of its items, each item
   is followed by its variable name as was inserted in the query (which was kept at the metadata of the (result) path."
   [from-eav-fn path]
   (let [expanded-path [(repeat (first path)) (repeat (second path)) (last path)] ; there may be several leaves in each path, so repeating the first and second elements
         meta-of-path (apply from-eav-fn (map repeat (:db/variable (meta path)))) ; re-ordering the path's meta to be in the order of the index
         combined-data-and-meta-path (interleave meta-of-path expanded-path)]
     (apply (partial map vector) combined-data-and-meta-path))) ; returning a seq of vectors, each one is a single result with its meta

(defn bind-variables-to-query
  "A function that receives the query results (result clauses) and transforms each of them into a binding structure.
   A binding structure is a map whose key is a binding pair of an entity-id, and the value is also a map, where its key is a binding pair
   of an attribute, and the value is a binding pair of that found attribute's value. The symbol name in each binding pair is extracted from the tripet's metadata"
  [q-res index]
  (let [seq-res-path (mapcat (partial combine-path-and-meta (from-eav index)) q-res) ; seq-ing a result to hold the meta
        res-path (map #(->> %1 (partition 2)(apply (to-eav index))) seq-res-path)] ; making binding pairs
    (reduce #(assoc-in %1  (butlast %2) (last %2)) {} res-path))) ; structuring the pairs into the wanted binding structure

(defn query-index
  "Querying an index based a seq of predicate clauses. A  predicate clause is composed of 3 predicates, each one to operate on a different level of the index. Querying an index with
  a specific clause-pred returns a result-clause. We then take all the result clauses, find within them the last-level-items that are found in all the result-clauses, and return the result clauses, each contains
  only the last-level-items that are part of all the result-clauses."
   [index pred-clauses]
   (let [result-clauses (filter-index index pred-clauses) ; the predicate clauses from the root of the index to the leaves (a leaf of an index is a set)
         relevant-items (items-that-answer-all-conditions (map last result-clauses) (count pred-clauses)) ; the set of elements, each answers all the pred-clauses
         cleaned-result-clauses (map (partial mask-path-leaf-with-items relevant-items)  result-clauses)] ; the result clauses, now their leaves are filtered to have only the items that fulfilled the predicates
     (filter #(not-empty (last %)) cleaned-result-clauses))) ; of these, we'll build a subset of the index that contains the clauses with the leaves (sets), and these leaves contain only the valid items

(defn single-index-query-plan
  "A query plan that is based on querying a single index"
  [query indx db]
     (let [q-res (query-index (indx-at db indx) query)]
           (bind-variables-to-query q-res  (indx-at db indx))))

 (defn index-of-joining-variable
  "A joining variable is the variable that is found on all of the query clauses"
  [query-clauses]
  (let [metas-seq  (map #(:db/variable (meta %)) query-clauses) ; all the metas (which are vectors) for the query
        collapsing-fn (fn [accV v] (map #(when (= %1 %2) %1) accV v)) ; going over the vectors, collapsing each onto another, term by term, keeping a term only if the two terms are equal
        collapsed (reduce collapsing-fn metas-seq)] ; using the above fn on the metas, eventually get a seq with one item who is not null, this is the joining variable
    (first (keep-indexed #(when (variable? %2 false) %1)  collapsed)))) ; returning the index of the first element that is a variable (there's only one)

  (defn build-query-plan
   "Upon receiving a database and query clauses, this function responsible to deduce on which index in the db it is best to perform the query clauses, and then return
   a query plan, which is a function that accepts a database and executes the plan on it."
   [query]
    (let [term-ind (index-of-joining-variable query)
          ind-to-use (case term-ind 0 :AVET 1 :VEAT 2 :EAVT)]
      (partial single-index-query-plan query ind-to-use)))

(defn resultify-bind-pair
  "A bind pair is composed of two elements - the variable name and its value. Resultifying means to check whether the variable is suppose to be part of the
  result, and if it does, adds it to the accumulated result"
  [vars-set accum pair]
  (let [[ var-name _] pair]
    (if (contains? vars-set var-name) (conj accum pair) accum)))

(defn resultify-av-pair
  "An av pair is a pair composed of two binding pairs, one for an attribute and one for the attribute's value"
  [vars-set accum-res av-pair]
   (reduce (partial resultify-bind-pair vars-set) accum-res  av-pair))

(defn locate-vars-in-query-res
  "this function would look for all the bindings found in the query result and return the binding that were requested by the user (captured at the vars-set)"
  [vars-set q-res]
  (let [[e-pair av-map]  q-res
        e-res (resultify-bind-pair vars-set [] e-pair )]
    (map (partial resultify-av-pair vars-set e-res)  av-map)))

(defn unify
  "Unifying the binded query results with variables to report"
  [binded-res-col needed-vars]
  (map (partial locate-vars-in-query-res needed-vars) binded-res-col))

(defmacro symbol-col-to-set [coll] (set (map str coll)))

(defmacro q
  "querying the database using datalog queries built in a map structure ({:find [variables*] :where [ [e a v]* ]}). (after the where there are clauses)
  At the moment support only filtering queries, no joins is also assumed."
  [db query]
  `(let [pred-clauses#  (q-clauses-to-pred-clauses ~(:where query)) ; transforming the clauses of the query to an internal representation structure called query-clauses
           needed-vars# (symbol-col-to-set  ~(:find query))  ; extracting from the query the variables that needs to be reported out as a set
           query-plan# (build-query-plan pred-clauses#) ; extracting a query plan based on the query-clauses
           query-internal-res# (query-plan# ~db)] ;executing the plan on the database
     (unify query-internal-res# needed-vars#)));unifying the query result with the needed variables to report out what the user asked for
