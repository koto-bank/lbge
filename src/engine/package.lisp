(defpackage :lbge.engine
  (:use :cl :lbge.asset :lbge.hash)
  (:local-nicknames (:beacon :lbge.beacon)
                    (:image :lbge.image)
                    (:asset :lbge.asset)
                    (:render :lbge.render)
                    (:render-back :lbge.render.backend)
                    (:fs :lbge.filesystem))
  (:export
   :engine
   :delete-engine
   :make-engine
   :get-engine
   :add-manager
   :get-manager
   :make-engine-options
   :init-engine
   :start

   ;; Engine beacons
   :get-beacon
   :link

   ;; Windows
   :make-window
   :get-main-window

   ;; Renderer
   :get-renderer
   :install-renderer))

(defpackage :lbge.engine.events
  (:use :cl :lbge.asset :lbge.hash)
  (:local-nicknames (:ax :alexandria)
                    (:r :lbge.render)
                    (:e :lbge.engine))
  (:export
   :process-events
   :add-event-handler
   :add-event-handlers
   :event-data))
