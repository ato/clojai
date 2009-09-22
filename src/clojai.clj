(ns clojai
  (:import (com.springrts.ai.oo OOAIFactory AbstractOOAI Unit))
  (:use clojure.set
        (clojai economy model unit spring command map task
                gui)))

(defn set-cheats!
  "Enable AI cheating mode.  Allows seeing all enemy unit locations
  etc."
  [cb on?]
  (-> cb .getCheats (.setEnabled on?)))

(defn repl-hooks []
  (eval '(def *ai (first (first @clojai.ClojaiFactory/ai-instances))))
  (eval '(def *cb (second (first @clojai.ClojaiFactory/ai-instances))))
  )

(defn create-ai
  "Creates the initial AI state."
  [cb]
  (set-cheats! cb true)
  (repl-hooks)
  (let [metal-spots (get-metal-spots cb)
        models (create-models cb)
        units (create-units cb models)]
    {:resources (create-resources cb)
     :models models
     :units units
     :grids (create-grids cb units)}))

(def on-unit-finished)
(defn reset-ai
  [cb]
  (let [ai (create-ai cb)]
    (reduce #(on-unit-finished %1 cb %2) ai (.getTeamUnits cb))))

(defn reset-ai! []
  (eval '(reset! *ai (reset-ai *cb)))
  nil)

; (reset-ai!)
; (count @(-> @*ai :units :team))
; (clojai.map/update-grids (@*ai :grids))
; (count @(-> @*ai :grids :team))

; (await (-> @*ai :grids :defence))
; (clear-agent-errors (-> @*ai :grids :defence))

;;;;;;;;;;;; EVENTS ;;;;;;;;;;;;;

(defn on-update [ai cb frame]

  (update-enemies cb (-> ai :units :enemy) (ai :models))
  (update-team cb (-> ai :units :team) (ai :models))

  (when (zero? (mod frame 8))

    (clojai.map/update-grids (@*ai :grids)))



  (comment
d   (-> 
    ai
    (update-enemies cb frame)
    (update-maps cb frame)
    (update-in [:resources] #(into % (current-resources cb)))))
  ai)

(defn on-message [ai cb player message]
  (println player " says: " message)
  ai)

(defn on-unit-finished [ai cb s-unit]

  (let [unit (create-unit s-unit (ai :models))]
    (println "Unit finished: " (unit :human-name) (unit :id))
    (dosync
     (alter (-> ai :units :team) assoc (unit :id) unit)))

  ai)

(defn on-unit-destroyed [ai cb s-unit attacker]

  (let [id (unit-name s-unit)]
    (println "Unit destroyed: " id)
    (dosync
     (alter (-> ai :units :team) dissoc id)))
  
  ai)


(defn on-unit-idle [ai cb s-unit]
  (comment
    (if-let [unit ((ai :team-units) (unit-name s-unit))]
      (do
        (println "Unit idle: " (:id unit))         
        (when-let [task (choose-task ai unit)]
          (execute! ai cb task))     
        ai)
      ai))
  ai)

(defn on-unit-move-failed [ai cb unit]
  (println "Unit move failed: " (.getHumanName (.getDef unit)))
  ai)

(defn on-unit-damaged [ai cb unit attacker damage dir weapon-def paralyzed?]
  (println "Unit damaged " (unit-name unit) " for " damage " dmg")
  ai)

(defn on-unit-created [ai cb s-unit builder]

  ai)




