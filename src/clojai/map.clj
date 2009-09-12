(ns clojai.map
  (:use clojai.unit-table))

(defn dist2d2
  "Return the square of the 2d distance between two positions."
  [p1 p2]
  (let [dx (- (.x p1) (.x p2))
        dz (- (.z p1) (.z p2))]
    (+ (* dx dx) (* dz dz))))

(defn dist2d
  "Return the distance between two positions."
  [p1 p2]
  (Math/sqrt (dist2d2 p1 p2)))

(defn dist2
  "Return the square of the distance between two positions."
  [p1 p2]
  (let [dx (- (.x p1) (.x p2))
        dy (- (.y p1) (.y p2))
        dz (- (.z p1) (.z p2))]
    (+ (* dx dx) (* dy dy) (* dz dz))))

(defn dist
  "Return the distance between two positions."
  [p1 p2]
  (Math/sqrt (dist2 p1 p2)))

(defn get-metal-spots
  "Returns a set of positions of resource spots on the map."
  [cb]
  (-> cb .getMap (.getResourceMapSpotsPositions 
                  (res-by-name cb "Metal")) set))

(defn closest-metal-spot
  "Returns the metal spot closest to pos."
  [ai pos]
  (apply min-key #(dist2d2 pos %) (ai :metal-spots)))

(defn closest-avail-metal-spot
  "Returns the available metal spot closest to pos."
  [ai pos]
  (apply min-key #(dist2d2 pos %) (ai :avail-metal-spots)))


(defn add-mex
  "Mark a metal extractor as being at pos."
  [ai pos]
  (println "+mex: " pos)
  (println (closest-metal-spot ai pos))
  (println (closest-avail-metal-spot ai pos))
  (update-in ai [:avail-metal-spots] 
             disj (closest-metal-spot ai pos)))

(defn del-mex
  "Mark a metal extractor as no longer being at pos."
  [ai pos]
  (println "-mex: " pos)
  (update-in ai [:avail-metal-spots] 
             conj (closest-metal-spot ai pos)))

;(def pt (first (clojai/ai :metal-spots)))
;(apply min-key #(dist pt %) (clojai/ai :metal-spots))
