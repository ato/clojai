(ns clojai.unit
  (:use clojai)
  (:import (com.springrts.ai.oo OOAICallback Unit)))

(defn unit-name
  "Returns the id of a spring unit."
  [#^Unit u]
  (keyword (str (-> u .getDef .getName) "-" (.getUnitId u))))

(defn create-unit
  "Creates a Clojai unit object from a spring unit object."
  [#^Unit u models]
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
  [#^OOAICallback cb models]
  {:team  (spring-units #(.getTeamUnits cb) models)
   :enemy (spring-units #(.getEnemyUnits cb) models)})

(defn update-unit
  [unit #^Unit s-unit]
  (assoc unit :pos (.getPos s-unit)))

(defn update-units
  "Updates a list of units."
  [oldunits s-units models]
  (persistent!
   (reduce
    (fn [units s-unit]
      (let [id (unit-name s-unit)
            unit (units id)]
        (assoc! units id 
                (if (nil? unit)
                  (create-unit s-unit models)
                  (update-unit unit s-unit)))))
    (transient oldunits) s-units)))

(defn update-enemies
  "Updates the list of enemy units."
  [#^OOAICallback cb enemy-units models]
  (dosync (alter enemy-units update-units (.getEnemyUnits cb)
                 models)))

(defn update-team
  "Updates the list of units controlled by the AI."
  [#^OOAICallback cb team-units models]
  (dosync (alter team-units update-units (.getTeamUnits cb) models)))

(defn units-by-tag
  "Convenience function for retunrning a seq of units with a
   particular tag."
  ([ai tag] (units-by-tag ai :team tag))
  ([ai kw tag]
     (filter #((% :tags) tag) (vals @((ai :units) kw)))))


