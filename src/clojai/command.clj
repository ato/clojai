(ns clojai.command
  (:import java.util.Queue
           java.util.concurrent.ConcurrentLinkedQueue
           com.springrts.ai.AICommandWrapper
           (com.springrts.ai.oo OOAICallback Unit)
           (com.springrts.ai.command SendTextMessageAICommand
                                     MoveUnitAICommand
                                     BuildUnitAICommand
                                     RepairUnitAICommand
                                     CreateLineFigureDrawerAICommand
                                     AddLineDrawAICommand
                                     RemovePointDrawAICommand)))

(def cmd-to-engine AICommandWrapper/COMMAND_TO_ID_ENGINE)

(defn execute-command! [#^OOAICallback cb cmd]
  "Send a command to the engine."
  (let [result (.handleCommand 
                (.getEngine cb)
                cmd-to-engine ; AFAICT this is ignored
                -1            ; so is this
                cmd)]
    (when (not (zero? result))
      (throw (Exception. (str "engine-cmd " cmd " returned " result))))
    true))

(defn push-command! [ai cmd]
  (.add #^Queue (ai :command-queue) cmd))

(defn send-chat! [ai text]
  "Sends a chat/text message to other players.
   This text will also end up in infolog.txt."
  (push-command! ai (SendTextMessageAICommand. text 0)))

(defn move-unit! [ai unit dest]
  "Order a unit to move to the specified destination."
  (push-command! ai (MoveUnitAICommand. unit 0 [] 0 dest)))

(defn execute-command-queue!
  "Execute any pending commands and clear the queue."
  [cb #^ConcurrentLinkedQueue q]
  (while (when-let [cmd (.poll q)]
           (execute-command! cb cmd))))

(defn create-command-queue
  "Creates an empty command queue."
  [cb]
  (java.util.concurrent.ConcurrentLinkedQueue.))

(comment

 (def ai { :callback clojai/*cb*})
 (send-chat ai "hello")
)
