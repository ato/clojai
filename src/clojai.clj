(ns clojai)

(declare *ai*)

(defn def-global-ai!
  "Sets up a global reference to an AI object.  This is *solely*
  for use from the repl."
  [ai]
  (def *ai* ai))

; (use '(clojure.contrib pprint duck-streams))

; (with-open [w (writer "/tmp/models2.dat")] (pprint (*ai* :models) w))
; (pprint (*ai* :models) (writer "/tmp/models.dat"))

; #<foo>
