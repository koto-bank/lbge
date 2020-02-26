(defpackage :lbge.render.backend
  (:use :cl)
  (:export
   ;; Base backend class and middle API
   :backend
   :init
   :clear
   :render
   :present
   :deinit
   :resize-viewport
   ;; Shader handling
   :shader-list
   :make-shader
   :use-shader))

(defpackage :lbge.render.shader
  (:use :cl)
  (:shadow
   cl:compile
   cl:delete)
  (:export
   :shader
   :add-stage
   :compile-shader
   :get-status
   :get-compile-log
   :delete-shader))

(defpackage :lbge.render
  (:use :cl)
  (:local-nicknames (:b :lbge.render.backend)
                    (:m :lbge.math))
  (:export
   :renderer
   :make-renderer
   :render-objects

   ;; Render functions
   :render
   :resize-viewport
   :renderer-backend

   ;; Render objects
   :add-object
   :add-objects
   :backend-data
   :vertices
   :indices
   :make-render-object
   :make-render-batch
   :batches
   :add-batch
   :transform

   ;; Camera
   :camera
   :camera-projection-matrix
   :camera-view-matrix
   :current-camera
   :adjust-camera-new-aspect
   :renderer-current-camera
   :add-camera
   :set-current-camera
   :get-current-camera
   :make-ortho-camera

   ;; Primitives
   :make-rect
   :make-triangle
   :make-circle
   :make-ellipse
   :make-ring

   ;;Misc
   :gl-check-error))

;;; low-level stuff
(defpackage :lbge.render.gl
  (:use
   :cl)
  (:local-nicknames (:b :lbge.render.backend)
                    (:m :lbge.math)
                    (:r :lbge.render)
                    (:h :lbge.hash)
                    (:ax :alexandria)
                    (:s :lbge.render.shader))
  (:export
   :make-context
   :gl-backend
   :delete-context))
