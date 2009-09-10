(ns clojai)

(def *cb*)
(def *ai*)

(defn on-update [frame]
  nil)

(defn on-message [player message]
  (println player " xsays: " message " bar " *ai*))

(defn on-unit-created [unit builder]
  (println "Unit created: " (.getHumanName (.getDef unit))))

(defn on-unit-finished [unit]
  (println "Unit finished: " (.getHumanName (.getDef unit))))

(defn on-unit-idle [unit]
  (println "Unit idle: " (.getHumanName (.getDef unit))))

(defn on-unit-move-failed [unit]
  (println "Unit move failed: " (.getHumanName (.getDef unit))))

(defn on-unit-damaged [unit attacker damage dir weapon-def paralyzed?]
  (println "Unit damaged " (.getHumanName (.getDef unit)) " by "
           (.getHumanName (.getDef attacker)) " for " damage " dmg"))

(defn on-unit-destroyed [unit attacker]
  (println "Unit destroyed " (.getHumanName (.getDef unit)) " by "
           (.getHumanName (.getDef attacker))))
