(ns 
    #^{:doc "Implements the OOAIFactory interface to satisify the
Spring Java API.  Doesn't actually contain much logic itself, it just
proxies everything over over to clojai so that we can patch code at
runtime."}
  clojai.ClojaiFactory
  (:import (com.springrts.ai.oo OOAIFactory AbstractOOAI))
  (:use clojai swank.swank)
  (:require clojure.main)
  (:gen-class
   :extends com.springrts.ai.oo.OOAIFactory))

(def swank-running? (atom nil))

(def ai-instances (atom []))

(defn -createAI [this team-id cb]
  (println (str "ClojaiFactory/-createAI called for team " team-id 
                "  with cb " cb))

  ;; we fire up swank with do-event so that the REPL
  ;; has access to our globals and can inspect the first AI
  (when-not @swank-running?
    (clojure.main/with-bindings
      (swank.swank/ignore-protocol-version "2008-12-09")
      (swank.swank/start-server "/dev/null" :port 4005
                                :encoding "iso-latin-1-unix"))
    (reset! swank-running? true))

  (let [ai (atom (create-ai cb))]
    (swap! ai-instances conj [ai cb])

    ;;
    ;; proxy all the events
    ;;
    (proxy [AbstractOOAI] []
      (update
       [frame]
       (swap! ai on-update cb frame)
       (assert @ai)
       0)
      (message
       [player message]
       (swap! ai on-message cb player message)
       (assert @ai)
       0)
      (unitCreated 
       [unit builder]
       (swap! ai on-unit-created cb unit builder)
       (assert @ai)
       0)
      (unitFinished
       [unit]
       (swap! ai on-unit-finished cb unit)
       (assert @ai)
       0)
      (unitIdle
       [unit]
       (swap! ai on-unit-idle cb unit)
       (assert @ai)
       0)
      (unitDamaged
       [unit attacker damage dir weapon-def paralyzed?]
       (swap! ai on-unit-damaged cb unit attacker damage dir
              weapon-def paralyzed?)
       (assert @ai)
       0)
      (unitDestroyed
       [unit attacker]
       (swap! ai on-unit-destroyed cb unit attacker) 
       (assert @ai)
       0))))
