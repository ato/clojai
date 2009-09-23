(ns clojai.task
  (:use clojai (clojai spring command map unit))
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
  (let [s-unit ((unit-by-id ai unit) :spring-unit)
        s-unit-def (-> ai :models to-build :spring-unit-def)]
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

; (execute! *ai* {:task :chat :text "Hello"})
; (execute! ai cb {:task :build :unit :armcom-404 :to-build :armmex})
; (execute! ai cb {:task :build, :unit :armcom-404, :to-build :armmex})

(defn execute!
  "Immediately executes a task."
  [ai & tasks]
  (println "Executing: " tasks)
  (dorun 
   (for [task tasks
         cmd (to-commands ai (ai :cb) task)]
     (execute-command! (ai :cb) cmd))))

(defn choose-task
  "Initial hard-coded task chooser.  Will be updated later
  to something more sophisticated."
  [ai unit]
  (cond
    ;--- Commander ---
    (-> unit :tags :commander)      
    (cond
      (< (count (units-by-tag ai :mex)) 2)
      {:task :build
       :unit (unit :id)
       :to-build (first (filter 
                         (comp :mex :tags (ai :models))
                         (unit :build-options)))
       :pos (closest (unit :pos) @(ai :avail-metal-spots))}
      
      (< (count (units-by-tag ai :armsolar)) 2)
      {:task :build
       :unit (unit :id)
       :to-build :armsolar}
      
      (< (count (units-by-tag ai :armllt)) 1)
      {:task :build
       :unit (unit :id)
       :to-build :armllt}
      
      (< (count (units-by-tag ai :armlab)) 1)
      {:task :build
       :unit (unit :id)
       :to-build :armlab})
    ;--- Lab ---
    (-> unit :tags :armlab)
    {:task :build
     :unit (unit :id)
     :to-build :armflea}))


; (execute! *ai* (choose-task *ai* (first (units-by-tag *ai* :commander))))
; (use 'clojure.contrib.pprint)
; (
