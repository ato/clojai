(ns clojai.map
  (:use clojai (clojai model))
  (:import (com.springrts.ai.oo OOAIFactory AbstractOOAI)))

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

(defn closest
  "Chooses the closes spot to position."
  [pos spots]
  (when-not (empty? spots)
    (apply min-key #(dist2d2 pos %) spots)))


(defn add-mex
  "Mark a metal extractor as being at pos."
  [ai pos]
  (alter (ai :avail-metal-spots)
             disj (closest pos (ai :metal-spots))))

(defn del-mex
  "Mark a metal extractor as no longer being at pos."
  [ai pos]
  (alter (ai :avail-metal-spots) 
             conj (closest pos (ai :metal-spots))))

(defn circle-fill-seq
  "Returns a seq of all the (integer) points within a circle of radius
   r centred at (cx, cy) and optionally clipped to the rectangle
   specified by [xmin,ymin] and [xmax,ymax]."
  ([r] (circle-fill-seq 0 0 r (- r) (- r) r r))
  ([cx cy r]
     (circle-fill-seq cx cy r (- cx r) (- cy r) (+ cx r) (+ cy r)))
  ([cx cy r xmin ymin xmax ymax]
     (for [y (range (max ymin (- cy r))
                    (inc (min ymax (+ cy r))))
           :let [yy (- y cy)
                 dx (int (Math/sqrt (- (* r r) (* yy yy))))]
           x (range (max xmin (- cx dx))
                    (inc (min xmax (+ cx dx))))]
       [x y])))

(defn circle-add
  "Adds a value to the (w x h) map v in the shape of a circle centred
   at cx and cy with radius r. "
  [v w h cx cy r value]  
  (reduce 
   (fn [v [x y]] 
     (let [idx (+ x (* y w))]
       (assoc v idx (+ (v idx) value))))
   v (circle-fill-seq cx cy r 0 0 (dec w) (dec h))))

(defn build-threatmap
  "Given a list of units, the size of the original map (in unit
  position coordinates) and a downscaling factor (eg 8), creates a
  map of positions to a threat value."
  [units [mapw maph] scale]

  (println "(re-)building threatmap of" (count units) "units"
           "map-size:" mapw "x" maph " @" scale)

  (let [w (/ mapw scale)
        h (/ maph scale)]
    (reduce 
     (fn [tm unit]

       (let [pos (unit :pos)
             x (int (/ (.x pos) scale))
             z (int (/ (.z pos) scale))
             r (int (/ (unit :weapon-range) scale))]
         (println "Adding unit" (unit :id) "at" x "," z "radius" r )
         (circle-add tm w h x z r (unit :power))))
     (vec (repeat (* w h) 0.0))
     units))

; (clojai/reset-ai!)
  )

(defn update-threatmap
  [oldtm units map-size scale]  
  (build-threatmap units map-size scale))

(defn update-maps 
  ([ai cb]
;     (.repaint (ai :gui))
     (send (ai :threat-map) update-threatmap (ai :enemy-units) 
           (ai :map-size) (ai :threat-map-scale))
     (-> ai
         (assoc :los-map (-> cb .getMap .getLosMap vec))))
     
  ([ai #^OOAIFactory cb frame]
     (if (zero? (mod frame 32))
       (update-maps ai cb)
       ai)))

(defmulti update-grid #(^% ::type))
(defmethod update-grid :spring [grid]
  (with-meta
    (vec ((^grid ::list-fn))) 
    ^grid))

(defmethod update-grid :threat [grid]
  (with-meta
    (build-threatmap
     (vals @(^grid ::units))
     (^grid ::map-size)
     (^grid ::scale))
    ^grid))

(defn spring-grid
  "Constructs an influence grid from a spring list."
  [f [mapw maph]]
  (let [lst (f)
        scale (int (Math/sqrt (/ (* mapw maph) (count lst))))] 
    (-> #^{::type :spring
           ::scale scale
           ::size [(/ mapw scale) (/ maph scale)]
           ::list-fn f}
        []
        (agent)
        (send update-grid))))

(defn threat-grid
  [units [mapw maph] scale]
  (println "Building threatmap of" (count @units) "units")
  (-> #^{::type :threat
         ::scale scale
         ::map-size [mapw maph]
         ::size [(/ mapw scale) (/ maph scale)]
         ::units units}
      []
      (agent)
      (send update-grid)))

(defn create-grids
  "Sets up the initial grids."
  [cb units]

  (let [s-map (.getMap cb)
        mapw (* 8 (.getWidth s-map))
        maph (* 8 (.getHeight s-map))
        msz [mapw maph]]
    {:slope  (spring-grid #(-> cb .getMap .getSlopeMap)  msz)
     :height (spring-grid #(-> cb .getMap .getHeightMap) msz)
     :radar  (spring-grid #(-> cb .getMap .getRadarMap)  msz)
     :jammer (spring-grid #(-> cb .getMap .getJammerMap) msz)
     :los    (spring-grid #(-> cb .getMap .getLosMap)    msz)

     :defence (threat-grid (units :team) msz 64)
     :threat (threat-grid (units :enemy) msz 64)
     }))

(defn update-grids [grids]
  (assert (grids :defence))

  (doseq [grid-id [:defence :threat]]
    (let [grid (get grids grid-id)]
      (await grid)
      (send grid update-grid)))

;  (let [ gseq (seq [(grids :defence) (grids :threat)])]
;    (dorun 
;     (for [grid gseq]
;       (do (await grid)
;           (send grid (update-grid)))
;       )))
  true)

(defn grid-size [grid]
  (^grid ::size))

;; (clojai/reset-ai!)
; (-> @clojai/*ai :grids)
;
