(ns clojai
  (:use clojure.set
        (clojai economy unit-table spring command)))

(defn create-ai
  "Creates the initial AI state."
  [callback]
  (let [unit-table (create-unit-table callback)
        ai {:resources (create-resources callback)
            :unit-table unit-table
            :units-by-tag (create-units-by-tag unit-table)
            :team-units {}
            :team-units-by-tag {}
            :command-queue (create-command-queue callback)}]
    (send-chat! ai "Today is a good day to die!")
    ai))

;;;;

(defn unit-name
  [u]
  (keyword (str (-> u .getDef .getName) "-" (.getUnitId u))))

; (use 'clojure.contrib.pprint)
; (pprint (ai :team-units-by-tag))
; (:team-units ai)

; (def aia (first (first @clojai.ClojaiFactory/ai-instances)))
; (def ai @(first (first @clojai.ClojaiFactory/ai-instances)))
; (def cb (second (first @clojai.ClojaiFactory/ai-instances)))
;
; (first (ai :team-units-by-tag))


(defn create-unit
  [ai u]
  (let [model (keyword (-> u .getDef .getName))]
    (assoc (-> ai :unit-table model)
      :id (unit-name u)
      :spring-id (.getUnitId u)
      :pos (.getPos u))))

;;;;;;;;;;;; EVENTS ;;;;;;;;;;;;;
  

(defn on-update [ai cb frame]
  (-> ai
      (update-in [:resources] #(into % (current-resources cb)))
      ))

(defn on-message [ai cb player message]
  (println player " says: " message)
  ai)

(defn on-unit-created [ai cb unit builder]
  (println "Unit created: " (.getHumanName (.getDef unit)))
  ai)

(defn on-unit-finished [ai cb u]
  (let [unit (create-unit ai u)
        id (unit :id)]
    (println "Unit finished: " id)

    (-> ai
        (update-in [:team-units] assoc id unit)

        (update-in [:team-units-by-tag]
                   #(reduce (fn [m tag]
                              (assoc m tag (conj (get m tag #{}) id)))
                            % (unit :tags))))))

; (swap! aia on-unit-finished cb (-> cb .getTeamUnits first))

(defn on-unit-idle [ai cb unit]
  (println "Unit idle: " (.getHumanName (.getDef unit)))
  ai)

(defn on-unit-move-failed [ai cb unit]
  (println "Unit move failed: " (.getHumanName (.getDef unit)))
  ai)

(defn on-unit-damaged [ai cb unit attacker damage dir weapon-def paralyzed?]
  (println "Unit damaged " (.getHumanName (.getDef unit)) " by "
           (.getHumanName (.getDef attacker)) " for " damage " dmg")
  ai)

(defn on-unit-destroyed [ai cb unit attacker]
  (println "Unit destroyed " (.getHumanName (.getDef unit)) " by "
           (.getHumanName (.getDef attacker)))
  ai)
