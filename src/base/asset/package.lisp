(defpackage :lbge.asset
  (:use :cl :lbge.hash)
  (:local-nicknames (:f :lbge.filesystem)
                    (:u :lbge.utils)
                    (:r :lbge.render)
                    (:b :lbge.render.backend)
                    (:sh :lbge.render.shader)
                    (:t :lbge.render.texture)
                    (:mat :lbge.render.material)
                    (:s :lbge.serialization))
  (:export
   ;; Key
   :key
   :asset-key
   :make-asset-key
   :make-asset
   :key-type
   :path
   :options
   :find-path-by-path-key

   :asset
   :type
   :asset-type
   :define-asset
   :asset-deps
   :asset-state

   :make-asset-manager
   :asset-manager
   :define-asset-handler
   :asset-handler
   :add-handler
   :handler-get-type
   :handler-get-asset
   :find-asset-file-by-path

   :add-root
   :asset-roots
   :get-asset
   :save-asset
   :load-dependencies

   :sexp-handler
   :sexp
   :sexp-data

   :shader-source
   :shader-source-handler
   :source
   :backend-shader
   :build-shader

   :image-asset
   :image-asset-handler

   :texture-asset
   :texture-asset-handler
   :backend-texture
   :build-texture

   :material-asset
   :material-asset-handler
   :backend-material
   :build-material))
