(ns clojai.unit-table
  (:import 
   (com.springrts.ai AICallback)
   (com.springrts.ai.oo UnitDef))
  (:use clojure.set))

(defn res-by-name [cb name]
  (first (filter #(= name (.getName %)) (.getResources cb))))

(defn- create-table-entry [#^AICallback cb #^UnitDef unit]
  (let [plant? (.contains (.getCategoryString unit) " PLANT ")
        metal (res-by-name cb "Metal")
        energy (res-by-name cb "Energy")]
    [(keyword (.getName unit))
     {:spring-unit-def unit
      :model (keyword (.getName unit))
      :human-name (.getHumanName unit)

      :speed  (.getSpeed unit)
      :health (.getHealth unit)
      :max-slope (.getMaxSlope unit)
      :weapon-range (.getMaxWeaponRange unit)

      :los-radius (.getLosRadius unit)
      :radar-radius (.getRadarRadius unit)
      :sonar-radius (.getSonarRadius unit)
      :jammer-radius (.getJammerRadius unit)
      :sonar-jammer-radius (.getSonarJamRadius unit)

      :build-time  (.getBuildTime unit)
      :build-speed (.getBuildSpeed unit)

      :build-costs {:metal  (.getCost unit metal)
                    :energy (.getCost unit energy)}
      :production  {:metal  (.getResourceMake unit metal)
                    :energy (.getResourceMake unit energy)}
      :upkeep      {:metal  (.getUpkeep unit metal)
                    :energy (.getUpkeep unit energy)}
      :storage     {:metal  (.getStorage unit metal)
                    :energy (.getStorage unit energy)}

      :build-options (into #{} (map #(keyword (.getName %)) 
                                    (.getBuildOptions unit)))

      :tags
      (disj 
       #{(keyword (.getName unit))
         (when (pos? (.getSpeed unit))   :mobile)
         (when (zero? (.getSpeed unit))  :building)
         (when (.isCommander unit)       :commander)

         (when (pos? (.getRadarRadius unit))    :radar)
         (when (pos? (.getSonarRadius unit))    :sonar)
         (when (pos? (.getJammerRadius unit))   :jammer)
         (when (pos? (.getSonarJamRadius unit)) :sonar-jammer)

         (when (neg? (.getMinWaterDepth unit))      :land)
         (when (.isAbleToFly unit)                  :air)


         (when (and (not (.isAbleToFly unit)) 
                    (>= (.getSpeed unit) 120)) :scout)

         ;; FIXME: there's various different levels of water
         ;; this tag is pretty inaccurate
         (when (or (pos? (.getMaxWaterDepth unit))
                   (pos? (.getMinWaterDepth unit))) :sea)

         (when (and (pos? (.getSpeed unit))
                    (not (.isNotTransportable unit))) :transportable)
         
         (when (and (.isBuilder unit)
                    (pos? (.getBuildSpeed unit))
                    (not (empty? (.getBuildOptions unit)))
                    (not (.isAirBase unit))) :builder)
         (when (and (.isAbleToRepair unit)
                    (pos? (.getRepairSpeed unit))
                    (not plant?)
                    (not (.isAirBase unit))) :repairer)
         (when (and (.isAbleToAttack unit)
                    (pos? (.getMaxWeaponRange unit))) :attacker)

         (when (and (.isAbleToReclaim unit)
                    (not (.isAirBase unit))
                    (not plant?)
                    (pos? (.getReclaimSpeed unit)))   :reclaimer)

         (when (and (.isAbleToAssist unit)
                    (not (.isAirBase unit))
                    (pos? (.getBuildSpeed unit))
                    (not plant?))                     :assister)
         (when (.isAbleToResurrect unit) :necromancer)
         (when (.isStealth unit)         :stealth)
         (when (.isReclaimable unit)     :reclaimable)
         (when (.isAbleToDGun unit)      :dgun)
         (when (.isOnOffable unit)       :toggle)
         (when plant? :plant)
         (when (.isAirBase unit) :airbase)

         (when (pos? (.getExtractsResource unit metal)) :mex)

         } nil)}]))


(defn- update-built-by
  "Takes a unit map and fills in the :built-by field with values from 
  the :build-options field of each unit's builder."
  [unit-map]
  (merge-with
   #(assoc %1 :built-by %2) unit-map
   (apply merge-with union 
          (map (fn [[n a]] 
                 (into {} (map #(vector % #{n}) (:build-options a))))
                                           unit-map))))
(defn create-unit-table
  [cb]
  (println "ClojAI: loading unit definitions...")
  (time (update-built-by 
         (reduce (fn [s u] (conj s (create-table-entry cb u))) 
                 {} (.getUnitDefs cb)))))

(defn tag-map
  "Creates a map of the form {tag #{unit-type}} for all tags in unit-def."
  [unit-def]
  (let [name (:model unit-def)]
    (into {} (map #(vector % #{name}) 
                  (conj (:tags unit-def) name)))))

(defn create-units-by-tag
  [unit-table]
  (println "Preparing units-by-tag...")

  (time
   (apply merge-with union
          (map (comp tag-map second) unit-table))))
