(ns clojai.proto.planner
  (use (clojure.contrib pprint repl-utils duck-streams)
       (clojai search)))

(def models
     (with-open [r (java.io.PushbackReader. (reader "/home/ato/src/clojai/data/models.dat"))]
       (read r)))

(defn succesors
  [state])

(defn all-build-options
  [state]
  (for [[builder cnt] state,
        :when (and (not (#{:energy :metal} builder))
                   (pos? cnt))
        option ((models builder) :build-options)]
    option))

(def initial-state
  {:metal 1000
   :energy 1000
   :armcom 1
   :armlab 1})

(def actions
     {:start {:preconds #{}
              :effects #{}}
      :finish {:preconds #{:right-shoe-on
                          :left-shoe-on}
               :effects #{}}
      :right-shoe {:preconds #{:right-sock-on}
                   :effects #{:right-shoe-on}}
      :right-sock {:preconds #{}
                   :effects #{:right-sock-on}}
      :left-shoe {:preconds #{:left-sock-on}
                   :effects #{:left-shoe-on}}
      :left-sock {:preconds #{}
                   :effects #{:left-sock-on}}
      })

(def initial-plan
     {:actions #{:start :finish}
      :orderings #{[:start :finish]}
      :links #{}
      :open-preconds ((actions :finish) :preconds)})

(defn succ
  [plan]
  (let [need (first (plan :open-preconds))]
    (for [act (filter #((% :effects) need) actions)])))


(all-build-options initial-state)

(comment 
  (breadth-first-search 
   {:successors (fn [state]
                  (when-not (integer? state)
                    (for [child state] [nil child])))
    :goal? #(= % 4)}
   (list (list 1 2) (list 3 4 5))))
