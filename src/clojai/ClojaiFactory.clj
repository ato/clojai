(ns clojai.ClojaiFactory
  (:import (com.springrts.ai.oo OOAIFactory AbstractOOAI))
  (:use clojai swank.swank)
  (:gen-class
   :extends com.springrts.ai.oo.OOAIFactory))

(def swank-running? (atom nil))

(defn -createAI [this team-id cb]
  (println (str "ClojaiFactory/-createAI called for team " team-id 
                "  with cb " cb))

  ;;
  ;; so that we don't have to thread them through everywhere
  ;; we (abuse?) the binding macro to create some per-AI "globals"
  ;;
  (defn do-event [fn]
    (binding [*cb* cb
              *ai* {:callback cb}]
      (fn))
    0)                  ; return zero to say we've processed the event
    
  ;; we fire up swank with do-event so that the REPL
  ;; has access to our globals and can inspect the first AI
  (when-not @swank-running?
    (do-event #(do
                 (swank.swank/start-server "/dev/null" :port 4005)))
    (reset! swank-running? true))

  ;;
  ;; proxy all the events
  ;;
  (proxy [AbstractOOAI] []
    (update
     [frame]
     (do-event #(on-update frame)))
    (message
     [player message]
     (do-event #(on-message player message)))
    (unitCreated 
     [unit builder]
     (do-event #(on-unit-created unit builder)))
    (unitFinished
     [unit]
     (do-event #(on-unit-finished unit)))
    (unitIdle
     [unit]
     (do-event #(on-unit-idle unit)))
    (unitDamaged
     [unit attacker damage dir weapon-def paralyzed?]
     (do-event #(on-unit-damaged unit attacker damage dir weapon-def paralyzed?)))
    (unitDestroyed
     [unit attacker]
     (do-event #(on-unit-destroyed unit attacker)))
    ))
