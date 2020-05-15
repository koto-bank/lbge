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
   :use-shader
   ;; Textures
   :make-texture
   :use-texture
   ;; Misc
   :print-statistics))

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
   :delete-shader
   :set-uniform))

(defpackage :lbge.render.texture
  (:use :cl)
  (:export
   :texture
   :target
   :image
   :format
   :texture-image
   :texture-format
   :texture-initialize                  ; initialize texture and necessary parameters
   :texture-load                        ; load texture to GPU
   :texture-release
   :texture-bind))

(defpackage :lbge.render
  (:use :cl)
  (:local-nicknames (:b :lbge.render.backend)
                    (:ax :alexandria)
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
   :textures
   :add-texture
   :make-render-object
   :make-render-batch
   :batches
   :add-batch
   :transform

   ;; buffer semantics
   :semantics
   :semantics=
   :make-semantics
   :stride
   :attributes-num
   :attribute-types
   :attribute-sizes
   :attribute-offsets

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
                    (:i :lbge.image)
                    (:r :lbge.render)
                    (:h :lbge.hash)
                    (:ax :alexandria)
                    (:s :lbge.render.shader)
                    (:t :lbge.render.texture))
  (:export
   :make-context
   :gl-backend
   :delete-context))
