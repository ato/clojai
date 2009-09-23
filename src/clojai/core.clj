(set! *warn-on-reflection* true)

(ns clojai.core
  (:import (com.springrts.ai.oo OOAIFactory AbstractOOAI Unit OOAICallback))
  (:use clojai (clojai map unit model economy gui))
  (:require swank.swank))

(defn create-ai
  "Creates the initial AI state."
  [ai-proxy cb]
  (let [metal-spots (get-metal-spots cb)
        models (create-models cb)
        units (create-units cb models)
        ai {:proxy ai-proxy
            :cb cb
            :resources (create-resources cb)
            :models models
            :units units
            :grids (create-grids cb units)}]
    (assoc ai :gui (show-gui ai))))

(defn destroy-ai [ai]
  (destroy-gui (ai :gui)))

(defn on-update [ai frame]
  (let [cb (ai :cb)]
    (update-enemies cb (-> ai :units :enemy) (ai :models))
    (update-team cb    (-> ai :units :team)  (ai :models))))

(defn on-release [ai reason]
  (destroy-ai ai))

(defn start-swank []
  (let [swank-running? (atom false)]
    (when-not @swank-running?
      (swank.swank/ignore-protocol-version "2008-12-09")
      (swank.swank/start-server "/dev/null" :port 4005
                                :encoding "iso-latin-1-unix")
      (reset! swank-running? true))))

(defmacro event [f]
  `(fn [& args#]                   
     (apply ~f (cons ~'ai (rest args#))) 0))

(defn reset-ai-proxy [ai-proxy cb]
  (let [ai (create-ai ai-proxy cb)]
    (def-global-ai! ai)
    (update-proxy ai-proxy
                  {"update" (event on-update)
                   "release" (event on-release)})))

(defn reset-ai! 
  ([] (reset-ai! *ai*))
  ([ai]
     (destroy-ai ai)
     (reset-ai-proxy (ai :proxy) (ai :cb))))
; (reset-ai!)

(defn create-ai-proxy [team-idx cb]
  (let [ai-proxy (proxy [AbstractOOAI] [])]
    (start-swank)
    (reset-ai-proxy ai-proxy cb)
    ai-proxy))

(defn set-cheats!
  "Enable AI cheating mode.  Allows seeing all enemy unit locations
  etc."
  [#^OOAICallback cb on?]
  (-> cb .getCheats (.setEnabled on?)))

; (reset-ai!)
; (count @(-> @*ai :units :team))
; (clojai.map/update-grids (@*ai :grids))
; (count @(-> @*ai :grids :team))

; (await (-> @*ai :grids :defence))
; (clear-agent-errors (-> @*ai :grids :defence))

;;;;;;;;;;;; EVENTS ;;;;;;;;;;;;;

(comment 
  (defmacro bar [f]
    `(println ~f))

  (macroexpand '(bar "foo"))

  (defn on-update [ai cb frame]

    (update-enemies cb (-> ai :units :enemy) (ai :models))
    (update-team cb (-> ai :units :team) (ai :models))

    (when (zero? (mod frame 8))

      (clojai.map/update-grids (ai :grids)))
    ))
