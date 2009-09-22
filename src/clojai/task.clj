(ns clojai.task
  (:use (clojai spring command map))
  (:import (com.springrts.ai.command SendTextMessageAICommand
                                     BuildUnitAICommand
                                     MoveUnitAICommand
                                     RepairUnitAICommand
                                     CreateLineFigureDrawerAICommand
                                     AddLineDrawAICommand
                                     RemovePointDrawAICommand)))


(defn find-build-site [cb pos s-unit-def]
  (.findClosestBuildSite (.getMap cb) s-unit-def
                         pos 1400.0 4 -1))

(defmulti to-commands
  "Converts a task into a sequence of engine commands."
  #(:task %3))

(defmethod to-commands :build 
  [ai cb {unit :unit to-build :to-build pos :pos}]
  (let [s-unit (-> ai :team-units unit :spring-unit)
        s-unit-def (-> ai :unit-table to-build :spring-unit-def)]
    (assert s-unit)
    (assert s-unit-def)

    (list (BuildUnitAICommand. 
           s-unit 0 [] 0 s-unit-def
           (find-build-site cb (or pos (.getPos s-unit)) s-unit-def) 0))))

(defmethod to-commands :chat
  [ai cb {text :text}]
  (assert text)
  (list (SendTextMessageAICommand. text 0)))

; (def ai (first (first @clojai.ClojaiFactory/ai-instances)))
; (def cb (second (first @clojai.ClojaiFactory/ai-instances)))

; (execute! ai cb {:task :chat :text "Hello"})
; (execute! ai cb {:task :build :unit :armcom-404 :to-build :armmex})
; (execute! ai cb {:task :build, :unit :armcom-404, :to-build :armmex})

(defn execute!
  "Immediately executes a task."
  [ai cb & tasks]
  (println "Executing: " tasks)
  (dorun 
   (for [task tasks
         cmd (to-commands ai cb task)]
     (execute-command! cb cmd))))

(defn choose-task
  "Initial hard-coded task chooser.  Will be updated later
  to something more sophisticated."
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
