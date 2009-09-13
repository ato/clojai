(ns clojai.gui
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

(defn create-map-panel [ai-atom mapkey w h f]
  "Create a panel with a customised render"
  (let [image (BufferedImage. w h BufferedImage/TYPE_BYTE_GRAY)
        oldmap (atom nil)
        panel (proxy [JPanel] []
                (getPreferredSize [] (Dimension. w h))
                (getMaximumSize [] (Dimension. w h))
                (paintComponent 
                 [g]
                 (proxy-super paintComponent g)

                 (let [newmap (@ai-atom mapkey)]
                   (when (not= @oldmap newmap)
                     (println mapkey)
                     (time (vec-to-raster! newmap
                                           (.getRaster image) f))
                     (reset! oldmap newmap)))

                 (.drawImage g image 0 0 nil)))]
    panel))

(defn show-gui [ai-atom]
  (let [frame (JFrame. "ClojAI")
        panel (JPanel.)
        [w h] (@ai-atom :map-size)]
    (doto panel
      (.add (create-map-panel ai-atom :height-map w h
                              identity))
      (.add (create-map-panel ai-atom :los-map (/ w 4) (/ h 4)
                              #(if (pos? %) 127 0)))
      (.add (create-map-panel ai-atom :radar-map (/ w 8) (/ h 8)
                              #(if (pos? %) 127 0)))
      (.add (create-map-panel ai-atom :jammer-map (/ w 8) (/ h 8)
                              #(if (pos? %) 127 0))))

    (doto frame
      (.add panel)
      (.setSize 640 400)
      (.setVisible true)
      )))
