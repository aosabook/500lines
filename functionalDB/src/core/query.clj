(ns core.query
   [:use [core constructs]
    [clojure.set :as CS :only (intersection)]])

(defn ind-at
  "inspecting a specific index at a given time, defaults to current. The kind argument mayone of the index name (e.g., AVET)"
  ([db kind] 
   (ind-at db kind  (:curr-time db)))
  ([db kind ts]
   (kind ((:timestamped db) ts))))

(defn variable?
  "A predicate that accepts a string and checks whether it describes a datalog variable (either starts with ? or it is _)"
  ([x] (variable? x true))
  ([x accept_?]  ; intentionally accepts a string and implemented as function and not a macro so we'd be able to use it as a HOF
  (or (and accept_? (= x "_")) (= (first x) \?))))

(defmacro clause-item-meta
  "Finds the name of the variable at an item of a datalog clause element. If no variable, returning nil"
  [clause-item]
  (cond
   (coll? clause-item)  (first (filter variable?  (map str clause-item))) ; the item is an s-expression, need to treat it as a coll, by going over it and returning the name of the variable
   (variable? (str clause-item)) (str clause-item) ; the item is a simple variable
   :no-variable-in-clause nil)) ; the item is a value and not a variable

(defmacro clause-item-expr
  "Create a predicate for each element in the datalog clause"
  [clause-item]
  (cond
   (variable? (str clause-item)) #(= % %) ; simple one, was something like ?a
   (not (coll? clause-item)) `#(= % ~(identity clause-item)) ; simple value given, was something like :likes
   (= 2 (count clause-item)) `#(~(first clause-item) %) ; was something like (pos? ?a)
   (variable? (str (second clause-item))) `#(~(first clause-item) % ~(last clause-item)) ; was something like (> ?a 42)
   (variable? (str (last clause-item))) `#(~(first clause-item) ~(second clause-item) %))) ; was something like (>  42 ?a)

(defmacro  q-clause
  "Build from a datalog clause (a vector with three elements describing EAV) a vector of predicates that would operate on
    an index, and set for that vector's metadata to be the names of the variables that the user assiged for each item in the clause"
  [clause]
  (loop [[frst-itm# & rst-itm#] clause exprs# [] metas# [] ]
    (if  frst-itm#
         (recur rst-itm# (conj exprs# `(clause-item-expr ~ frst-itm#)) (conj metas#`(clause-item-meta ~ frst-itm#)))
         (with-meta exprs# {:db/variable metas#}))))

 (defmacro q-clauses
   "create a vector of queries to operate on indices, based on the given vector of clauses "
   [clauses]
    (loop [[frst# & rst#] clauses preds-vecs# []  ]
      (if-not frst#  preds-vecs#
        (recur rst# `(conj ~preds-vecs# (q-clause ~frst#))) )))

 (defn index-of-chaining-variable
  "A chaining variable is the variable that is found on all of the query clauses"
  [query-clauses]
  (let [metas-seq  (map #(:db/variable (meta %)) query-clauses)
        collapse-seqs (fn [s1 s2] (map #(when (= %1 %2) %1) s1 s2))
        collapsed (reduce collapse-seqs metas-seq)]
    (first (keep-indexed #(when (variable? %2 false) %1)  collapsed))))

  (defn choose-index
   "Upon receiving a database and query clauses, this function responsible to deduce on which index in the db it is best to perfom the query clauses, and then return
   a vector in which the first element is the decided index and the second element is a function that knows how to restore an EAV structure from that decided index path structure."
   [db query]
    (let [var-ind (index-of-chaining-variable query)
            ind-to-use (case var-ind 0 :AVET 1 :VEAT 2 :EAVT)]
      (ind-at db ind-to-use)))

(defn filter-index
  "Function that accepts an index and a path-predicate (which is a tripet of predicates to apply on paths in an index). For each path predicates it creates a result path (a triplet
  representing one path in the index) and returns a seq of result paths."
  [index path-preds]
  (for [ path-pred path-preds
        :let [[lvl1-prd lvl2-prd lvl3-prd] (apply (from-eav index) path-pred)] ; predicates for the first and second level of the index, also keeping the path to later use its meta
           [k1 l2map] index  ; keys and values of the first level
           :when (try (lvl1-prd k1) (catch Exception e false))  ; filtering to keep only the keys and the vals of the keys that passed the first level predicate
           [k2  l3-set] l2map  ; keys and values of the second level
           :when (try (lvl2-prd k2) (catch Exception e false)) ; filtering to keep only the keys and vals of keys that passed the second level predicate
           :let [res (set (filter lvl3-prd l3-set))] ]; keep from the set at the third level only the items that passed the predicate on them
         (with-meta [k1 k2 res] (meta path-pred)))) ; constructed to resemble the EAV structure, while keeping the meta of the query to use it later when extracting variables

 (defn items-that-answer-all-conditions
   "takes the sequence of all the items collection, each such collection answered one condition, we test here what are the items that answered all of the conditions
   i.e., what items are found at exactly 'num-of-conditions' of such collections "
   [items-seq num-of-conditions]
   (->> items-seq ; take the items-seq
           (map vec) ; make each collection (actually a set) into a vector
           (reduce into []) ;reduce all the vectors into one big vector
           (frequencies) ; count for each item in how many collections (sets) it was in originally
           (filter #(<= num-of-conditions (last %))) ; keep only the items that answered all of the conditions
           (map first) ; take from the duos the items themeselves
           (set))) ; return it as set

(defn mask-path-leaf-with-items
  "Returning the path with only the items found in the intersection of that path's items and the relevant items"
  [relevant-items path]
    (update-in path [2] CS/intersection relevant-items ))

 (defn seqify-index-path
   "A result-path is a path whose leaves are the items representing the items of the query chaining variable. This function
   returns for a result path a seq of vectors, each vector is a path from the root of the result path to one of its items, each item is followed
   by its variable name as was inserted in the query"
   [index path]
   (let [seq-path   [ (repeat (first path))  (repeat (second path)) (last path)]
         meta-path(apply (from-eav index) (map repeat (:db/variable (meta path)))) ; re-ordering the meta to be in the order of the index
         all-path (interleave meta-path seq-path)]
     (apply (partial map vector)  all-path)))

(defn bind-variables-to-query
  "A function that receives the query results and restructues them to be an binding structure which resembles in its shape to an entity.
   The binding structure is a map whose key is a binding pair of a found entity-id, and the value is also a map, where its key is the binding pair of a found
  attribute, and the value is the binding pair of that found attribute's value"
  [q-res index]
  (let [seq-res-path (mapcat (partial seqify-index-path index)  q-res) ; seq-ing a result to hold the meta
         res-path (map #(->> %1 (partition 2)(apply (to-eav index))) seq-res-path)] ; making binding pairs
    (reduce #(assoc-in %1  (butlast %2) (last %2)) {} res-path))) ; structuring the pairs into the wanted binding structure

(defn query-index
  "Quering an index based a seq of path predicates. A path predicate is composed of 3 predicates, each one to operate on a different level of the index. Querying an index with
  a specific path-pred returns a result-path. We then take all the result paths and find within them the items that passed all the path-preds, and eventually return the result path, each contains
  only the items that passed all the path predicates."
   [index path-preds]
   (let [result-paths (filter-index index path-preds) ; the paths (vectors) from the root of the index to the leaves (a leaf of an index is a set) where each path fulfils one predicate path
         relevant-items (items-that-answer-all-conditions (map last result-paths) (count path-preds)) ; the set of elements, each answers all the pred-paths
         cleaned-paths (map (partial mask-path-leaf-with-items relevant-items) result-paths)] ; the paths, now their leaves are filtered to have only the items that fulfilled the predicates
     (filter #(not-empty (last %)) cleaned-paths))) ; of these, we'll build a subset-path of the index that contains the paths to the leaves (sets), and these leaves contain only the valid items

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
  "this function would look for all the binding found in the query result and return the binding that were requested by the user (captured at the vars-set)"
  [vars-set q-res]
  (let [[e-pair av-map]  q-res
          e-res (resultify-bind-pair vars-set [] e-pair )]
    (reduce (partial resultify-av-pair vars-set) e-res  av-map)))

(defmacro settify [coll] (set (map str coll)))
