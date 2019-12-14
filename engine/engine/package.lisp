(defpackage :lbge.engine.events
  (:use :cl :lbge.asset :lbge.hash)
  (:export
   :process-events
   :add-event-handler
   :add-event-handlers))

(defpackage :lbge.engine
  (:use :cl :lbge.asset :lbge.hash)
  (:export
   :engine
   :delete-engine
   :make-engine
   :get-engine
   :add-manager
   :get-manager
   :make-engine-options
   :start
   ))
