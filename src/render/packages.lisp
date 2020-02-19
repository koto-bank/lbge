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
   ;; Shader handling
   :add-shader
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
   :get-backend

   ;; Render objects
   :add-object
   :add-objects
   :backend-data
   :vertices
   :indices
   :make-render-object
   :add-batch

   ;; Camera
   :camera
   :add-camera
   :set-current-camera
   :make-ortho-camera

   ;; Primitives
   :make-rect
   :make-triangle
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
