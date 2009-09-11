(ns clojai.economy
  (:use clojai.search))

(defn- clean-map-add
 "Merge changes into m (by adding values of the same key) removing any
  resulting pairs with a value of zero."
 [m changes]
 (reduce 
  (fn [m [k deltav]]
    (let [v* (+ (m k 0) deltav)]
      (when (neg? v*)
        (throw (Exception. (str "Update would have resulted in "
                                "negative resource " k))))
      (if (zero? v*) (dissoc m k) (assoc m k v*))))
  m changes))

(defn add-resources
  "Add some resources to the economy. Res should be a map or seq of
   key-value pairs, for example: [[:metal 200] [:energy -30]]."
  [ai res]
  (update-in ai [:resources] #(clean-map-add % res)))

(defn current-resources
  "Creates a resources list based on the current game state."
  [callback]
  (let [economy (.getEconomy callback)]
    (for [res (.getResources callback)]      
      [(-> res .getName .toLowerCase keyword)
       (.getCurrent economy res)])))

(defn create-resources
  "Prepares the initial resources map."
  [callback]
  (into {} (current-resources callback)))

;;;; Economic planning ;;;;

(defn goal-difference
  "Given a state and a goal work out what is necessary to achieve the goal."
  [state goal]
  (for [[res need] goal
        :let [have (state res 0)]
        :when (< have need)]
    [res (- need have)]))

(defn mapvals
  "Apply f to the values of m. Returns another map."
  [f m]
  (zipmap (keys m) (map f (vals m))))

(defmulti effect :action)
(defmethod effect :build [ai {unit-name :unit}]
  (let [unit ((ai :unit-table) unit-name)]
    (merge-with + (mapvals - (:build-costs unit))
                (zipmap (:tags unit) (repeat 1)))))

(defmulti preconditions :action)
(defmethod preconditions :build [{unit-name :unit, builder :builder}]
  {builder 1})

(defn affect
  "Apply some differences to a state."
  [state & changes]
  (let [state* (apply merge-with + state changes)]
    (apply dissoc state* (for [[k v] state* :when (zero? v)] k))))


(comment

  (-> ai :units-by-tag :builder)

  (def goal {:scout 1})

  (defn goal? [ai]
    (let [res (ai :resources)]
      (every? (fn [[k v]] (>= (res k 0) v)) goal)))

  (goal? ai) 

  (ai :resources)

  (breadth-first-search 
   {:successors (fn [state]
                  (when-not (integer? state)
                    (for [child state] [nil child])))
    :goal? #(= % 4)}
   (list (list 1 2) (list 3 4 5)))

  (breadth-first-search 
   {:successors (fn [state]
                  (when-not (integer? state)
                    (for [child state] [nil child])))
    :goal? #(= % 4)}
   (list (list 1 2) (list 3 4 5)))


(defn succ-rev [ai unit-table state]
  (for [[res delta] (goal-difference initial-state state)
        unit ((ai :units-by-tag) res)
        builder (:built-by ((ai :unit-table) unit))]
    (let [action {:action :build
                  :unit unit
                  :builder builder}]
      [action (affect state 
                      {res -1}
                      (preconditions action)
                      )])))





; (:team-units-by-tag ai)
; (def ai @(first (first @clojai.ClojaiFactory/ai-instances)))
; (def cb (second (first @clojai.ClojaiFactory/ai-instances)))
)
