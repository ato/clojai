(ns clojai
  (:import (com.springrts.ai.oo OOAIFactory AbstractOOAI Unit))
  (:use clojure.set
        (clojai economy unit-table spring command map task gui)))

(defn create-ai
  "Creates the initial AI state."
  [cb]
  (let [unit-table (create-unit-table cb)
        metal-spots (get-metal-spots cb) 

        ai {:resources (create-resources cb)
            :unit-table unit-table
            :units-by-tag (create-units-by-tag unit-table)
            :team-units {}
            :team-units-by-tag {}
            :command-queue (create-command-queue cb)
            :metal-spots metal-spots
            :avail-metal-spots metal-spots

            :map-size [(-> cb .getMap .getWidth) 
                       (-> cb .getMap .getHeight)]
            :slope-map  (-> cb .getMap .getSlopeMap vec)
            :height-map (-> cb .getMap .getHeightMap vec)
            :radar-map  (-> cb .getMap .getRadarMap vec)
            :jammer-map (-> cb .getMap .getJammerMap vec)
            :los-map (-> cb .getMap .getLosMap vec)
            }]
    ai))


;;;;

(defn unit-name
  [u]
  (keyword (str (-> u .getDef .getName) "-" (.getUnitId u))))

(defn get-model
  "Given a spring unit object returns our model map of it."
  [ai s-unit]
  (let [model-id (-> s-unit .getDef .getName keyword)]
    (-> ai :unit-table model-id)))

; (use 'clojure.contrib.pprint)
; (pprint (ai :team-units-by-tag))
; (:team-units ai)

; (:team-units-by-tag @aia)
; (def aia (first (first @clojai.ClojaiFactory/ai-instances)))
; (def ai @(first (first @clojai.ClojaiFactory/ai-instances)))
; (def cb (second (first @clojai.ClojaiFactory/ai-instances)))
;(:units-by-tag ai)
; (create-units-by-tag (ai :unit-table))
; (tag-map (second (first (ai :unit-table))))
; (first (ai :team-units-by-tag))
; (:team-units-by-tag ai)
; (first (create-units-by-tag (ai :unit-table)))
; (do (reset! aia (reset-ai cb)) nil)
; (first (:units-by-tag (create-ai cb)))
; (Math/sqrt (count (@aia :los-map)))
; (/ 512 128)
(defn create-unit
  [ai u]
  (let [model (keyword (-> u .getDef .getName))]
    (assoc (-> ai :unit-table model)
      :id (unit-name u)
      :spring-unit u
      :pos (.getPos u))))

(defn enrol-unit [ai unit]
  (-> ai
      (update-in [:team-units] assoc (unit :id) unit)

      (update-in [:team-units-by-tag]
                 #(reduce (fn [m tag]
                            (assoc m tag (conj (get m tag #{}) (unit :id))))
                          % (unit :tags)))))

(defn unenrol-unit [ai unit]
  (-> ai
      (update-in [:team-units] dissoc (unit :id))

      (update-in 
       [:team-units-by-tag]
       #(reduce (fn [m tag]
                  (let [newval (disj (get m tag #{}) (unit :id))]
                    (if (empty? newval)
                      (dissoc m tag)
                      (assoc m tag newval))))
                % (unit :tags)))))

(def on-unit-finished)
(defn reset-ai
  [cb]
  (let [ai (create-ai cb)]
    (reduce #(on-unit-finished %1 cb %2) ai (.getTeamUnits cb))))

;;;;;;;;;;;; EVENTS ;;;;;;;;;;;;;
  
(comment
  :slope-map  (-> cb .getMap .getSlopeMap vec)
  :height-map (-> cb .getMap .getHeightMap vec)
  :radar-map  (-> cb .getMap .getRadarMap vec)
  :jammer-map (-> cb .getMap .getJammerMap vec)
  :los-map) (-> cb .getMap .getLosMap vec)

(use 'clojai.gui)
(def gui (show-gui aia))

(defn update-maps [ai #^OOAIFactory cb frame]
  (if (zero? (mod frame 32))
    (do 
      (.repaint gui)
      (assoc ai :los-map (-> cb .getMap .getLosMap vec)))
    
    ai))



(defn on-update [ai cb frame]
  (-> 
   ai
   (update-maps cb frame)
   (update-in [:resources] #(into % (current-resources cb)))))

(defn on-message [ai cb player message]
  (println player " says: " message)
  ai)

(defn choose-task
  [ai unit]
  (cond
    ;--- Commander ---
    (-> unit :tags :commander)      
    (cond
      (< (count (-> ai :team-units-by-tag :mex)) 2)
      {:task :build
       :unit (unit :id)
       :to-build (first (filter 
                         (comp :mex :tags (ai :unit-table))
                         (unit :build-options)))
       :pos (closest-avail-metal-spot ai (unit :pos))}
      
      (< (count (-> ai :team-units-by-tag :armsolar)) 2)
      {:task :build
       :unit (unit :id)
       :to-build :armsolar}
      
      (< (count (-> ai :team-units-by-tag :armllt)) 1)
      {:task :build
       :unit (unit :id)
       :to-build :armllt}
      
      (< (count (-> ai :team-units-by-tag :armlab)) 1)
      {:task :build
       :unit (unit :id)
       :to-build :armlab})
    ;--- Lab ---
    (-> unit :tags :armlab)
    {:task :build
     :unit (unit :id)
     :to-build :armflea}))

(defn on-unit-finished [ai cb u]
  (let [unit (create-unit ai u)
        id (unit :id)]
    (println "Unit finished: " id)
    (let [ai* (-> ai (enrol-unit unit))]
      (when-let [task (choose-task ai* unit)] (execute! ai* cb task))
      ai*)))

; (swap! aia on-unit-finished cb (-> cb .getTeamUnits first))

(defn on-unit-idle [ai cb s-unit]
  (if-let [unit ((ai :team-units) (unit-name s-unit))]
    (do
      (println "Unit idle: " (:id unit))
     
     
      (when-let [task (choose-task ai unit)] (execute! ai cb task))
     
      ai)
    ai))

; (def unit (-> @aia :team-units :armcom-404))
; (-> ai :team-units-by-tag :commander)
; (-> @aia :team-units)
; (filter (comp :mex :tags (ai :unit-table)) 
;   (-> @aia :team-units :armcom-404 :build-options))
;
(defn on-unit-move-failed [ai cb unit]
  (println "Unit move failed: " (.getHumanName (.getDef unit)))
  ai)

(defn on-unit-damaged [ai cb unit attacker damage dir weapon-def paralyzed?]
  (println "Unit damaged " (.getHumanName (.getDef unit)) " by "
           (.getHumanName (.getDef attacker)) " for " damage " dmg")
  ai)


(defn on-unit-created [ai cb unit builder]
  (println "Unit created: " (.getHumanName (.getDef unit)))

  ;; update metal map
  (if (-> (get-model ai unit) :tags :mex)
    (add-mex ai (.getPos unit))
    ai))

(defn on-unit-destroyed [ai cb s-unit attacker]
  (if-let [unit ((ai :team-units) (unit-name s-unit))]
    (do
     (println "Unit destroyed: " (:id unit))

     ;; update metal map
     (->
      (if (-> unit :tags :mex)
        (del-mex ai (:pos unit))
        ai)
      (unenrol-unit unit)))
    ai))


