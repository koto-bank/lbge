(defpackage :lbge.render.backend
  (:use :cl)
  (:export
   ;; Base backend class and middle API
   :backend
   :init
   :clear
   :render
   :present
   :deinit))

(defpackage :lbge.render
  (:use :cl)
  (:local-nicknames (:b :lbge.render.backend)
                    (:m :lbge.math))
  (:export
   :renderer
   :make-renderer

   ;; Render functions
   :render
   :get-backend

   ;; Render objects
   :add-object
   :add-objects

   ;; Camera
   :camera
   :add-camera
   :set-current-camera
   :make-ortho-camera

   ;; Primitives
   :make-rect
   :make-triangle
   :make-ellipse
   :make-ring))

;;; low-level stuff
(defpackage :lbge.render.gl
  (:use
   :cl)
  (:local-nicknames (:b :lbge.render.backend)
                    (:r :lbge.render))
  (:export
   :make-context
   :gl-backend
   :delete-context))
