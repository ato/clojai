(ns clojai.search
  (:import clojure.lang.PersistentQueue))

(def empty-queue PersistentQueue/EMPTY)

(defn- default-step-cost
  [node action result]
  1)

(defn- make-node
  [state]
  {:state state})

(defn- expand
  [node problem]
  (doall (for [[action result] ((:successors problem) (:state node))]
     {:parent node
      :action action
      :state result
      :path-cost (+ (:path-cost node 0)
                    ((:step-cost problem default-step-cost) 
                     node action result))
      :depth (inc (:depth ^node 0))})))

(defn- conj-all
  "Conj all the values in xs onto coll."
  [coll xs]
  (if-let [s (seq xs)]
    (apply conj coll s)
    coll))

(defn tree-search
  [problem fringe]
  (when-let [node (peek fringe)]
    (if ((:goal? problem) (:state node))
      node ; solution
      (recur problem (conj-all (pop fringe) (expand node problem))))))

(defn graph-search
  "Like tree-search but can cope with cycles at the expense of keeping
   all searched states in memory."
  ([problem fringe]
     (graph-search problem fringe #{}))
  ([problem fringe closed]
     (when-let [node (peek fringe)]       
       (if ((:goal? problem) (:state node))
         node                           ; solution
         (recur problem 
                (conj-all (pop fringe) (filter (comp (complement closed) 
                                                     :state)
                                           (expand node problem)))
                (conj closed (:state node)))))))

(defn breadth-first-search
  [problem initial-state]
  (tree-search problem (conj empty-queue (make-node initial-state))))

(defn depth-first-search
  [problem initial-state]
  (tree-search problem (list (make-node initial-state))))

(comment 
  (breadth-first-search 
   {:successors (fn [state]
                  (when-not (integer? state)
                    (for [child state] [nil child])))
    :goal? #(= % 4)}
   (list (list 1 2) (list 3 4 5))))
