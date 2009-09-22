(ns clojai.unit)

(defn unit-name
  "Returns the id of a spring unit."
  [u]
  (keyword (str (-> u .getDef .getName) "-" (.getUnitId u))))

(defn create-unit
  "Creates a Clojai unit object from a spring unit object."
  [u models]
  (let [model (keyword (-> u .getDef .getName))]
    (assoc (models model)
      :id (unit-name u)
      :spring-unit u
      :pos (.getPos u))))

(defn spring-units
  "Produces a ClojAI unit map from a function that returns a list of
  Spring unit objects."
  [f models]
  (ref #^{::lst-fn f} {}))

(defn create-units
  "Creates the unit lists."
  [cb models]
  {:team  (spring-units #(.getTeamUnits cb) models)
   :enemy (spring-units #(.getEnemyUnits cb) models)})

(defn update-unit
  [unit s-unit]
  (assoc unit :pos (.getPos s-unit)))

(defn update-units
  "Updates a list of units."
  [oldunits s-units models]
  (reduce
   (fn [units s-unit]
     (let [id (unit-name s-unit)
           unit (units id)]
       (assoc units id 
              (if (nil? unit)
                (create-unit s-unit models)
                (update-unit unit s-unit)))))
   oldunits s-units))

(defn update-enemies
  "Updates the list of enemy units."
  [cb enemy-units models]
  (dosync (alter enemy-units update-units (.getEnemyUnits cb)
                 models)))

(defn update-team
  "Updates the list of units controlled by the AI."
  [cb team-units models]
  (dosync (alter team-units update-units (.getTeamUnits cb) models)))
