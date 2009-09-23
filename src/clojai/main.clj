;;;;
;;;; This is a shim to compile the rest of Clojai with the right
;;;; bindings.  Be careful that any accesses to other clojai
;;;; code (include use and require) are inside a with-bindings block.
;;;;
(ns clojai.main
  (:require clojure.main))

(defn create-ai-proxy
  [& args]
  (clojure.main/with-bindings
   (require 'clojai.core)
   (apply (eval 'clojai.core/create-ai-proxy) args)))
