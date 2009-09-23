(ns clojai.gui
  (:use clojai.map)
  (:import (javax.swing JFrame JLabel JTextField JButton JPanel)
           (java.awt.event ActionListener)
           (java.awt GridLayout Dimension Graphics Color Graphics2D)
           (java.awt.image BufferedImage WritableRaster)))

(defn vec-to-raster! [v #^WritableRaster raster f]
  (let [w (int (.getWidth raster))
        h (int (.getHeight raster))]
    (dotimes [y h]
      (dotimes [x w]
        (.setSample raster x y (int 0) (int (f (int (v (+ (* y w) x)))))
)))))

(defn create-grid-panel [grid f]
  "Create a panel with a customised render"
  (let [[w h] (grid-size @grid)
        image (BufferedImage. w h BufferedImage/TYPE_BYTE_GRAY)
        oldmap (atom nil)
        panel (proxy [JPanel] []
                (getPreferredSize [] (Dimension. w h))
                (getMaximumSize [] (Dimension. w h))
                (paintComponent 
                 [g]
                 (proxy-super paintComponent g)

                 (let [newmap @grid]
                   (when (not= @oldmap newmap)
                     (time (vec-to-raster! newmap
                                           (.getRaster image) f))
                     (reset! oldmap newmap)))

                 (.drawImage g image 0 0 nil)))]
    (println "Added" w "x" h "grid")
    (add-watch grid panel (fn [_ _ _ _] (.repaint panel)))
    panel))

(defn show-gui [ai]
  (let [frame (JFrame. "ClojAI")
        panel (JPanel.)]
    (doto panel
      (.add (create-grid-panel (-> ai :grids :height) identity))
      (.add (create-grid-panel (-> ai :grids :defence) identity))
      (.add (create-grid-panel (-> ai :grids :threat) identity)))

    (doto frame
      (.add panel)
      (.setSize 1000 1000)
      (.setVisible true)
      )))

(defn destroy-gui [frame]
  (.dispose frame))

;(show-gui @clojai/*ai)
;(apply + (-> @clojai/*ai :grids :defence (deref)))
;

;(send (-> @clojai/*ai :grids :defence) update-grid)



