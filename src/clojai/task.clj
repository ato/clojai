(ns clojai.task
  (:use (clojai spring command))
  (:import (com.springrts.ai.command SendTextMessageAICommand
                                     BuildUnitAICommand
                                     MoveUnitAICommand
                                     RepairUnitAICommand
                                     CreateLineFigureDrawerAICommand
                                     AddLineDrawAICommand
                                     RemovePointDrawAICommand)))


(defn find-build-site [cb s-builder s-unit-def]
  (.findClosestBuildSite (.getMap cb) s-unit-def
                         (.getPos s-builder) 1400.0 4 -1))

(defmulti to-commands
  "Converts a task into a sequence of engine commands."
  #(:task %3))

(defmethod to-commands :build 
  [ai cb {unit :unit to-build :to-build}]
  (let [s-unit (-> ai :team-units unit :spring-unit)
        s-unit-def (-> ai :unit-table to-build :spring-unit-def)]
    (assert s-unit)
    (assert s-unit-def)
    (list (BuildUnitAICommand. s-unit 0 [] 0 s-unit-def
                               (find-build-site cb s-unit s-unit-def) 0))))

(defmethod to-commands :chat
  [ai cb {text :text}]
  (assert text)
  (list (SendTextMessageAICommand. text 0)))

; (def ai @(first (first @clojai.ClojaiFactory/ai-instances)))
; (def cb (second (first @clojai.ClojaiFactory/ai-instances)))

; (execute! ai cb {:task :chat :text "Hello"})

(defn execute!
  "Immediately executes a task."
  [ai cb & tasks]
  (for [task tasks
        cmd (to-commands ai cb task)]
    (execute-command! cb cmd)))


