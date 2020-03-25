(asdf:defsystem :lbge
  :license "BSD-2-Clause"
  :depends-on (:alexandria :sdl2
               :rove
               :cffi :cl-autowrap
               :lbge-render
               :log4cl
               :closer-mop :cl-ppcre)
  :components
  ((:module base
    :components
    ((:file "packages")
     (:file "hash-map")
     (:file "utils")
     (:file "beacon")
     (:file "timer")
     (:file "sparse-set")))
   (:module animation
    :components
    ((:file "packages")
     (:file "animation")))
   (:module application
    :components
    ((:file "package")
     (:file "misc")))
   (:module filesystem
    :components
    ((:file "package")
     (:file "misc")))
   (:module image-loader
    :components
    ((:file "packages")
     (:file "image")
     (:file "image-loader")
     (:file "tga")))
   (:module math
    :components
    ((:file "package")
     (:file "definitions")
     (:file "common")
     (:file "vector")
     (:file "matrix")
     (:file "quaternion")
     (:file "polynomial")
     (:file "transform")
     (:module "interpolations"
      :components
       ((:file "newton")
        (:file "linear")))))
   (:module assets
    :pathname "base/asset"
    :components
    ((:file "package")
     (:file "asset")
     (:file "asset-handler")
     (:file "asset-manager")
     (:file "sexp-asset-handler")
     (:file "glsl-asset-handler")))
   (:module engine
    :components
    ((:file "package")
     (:file "events")
     (:file "engine")))
   (:module ecs
    :components
    ((:file "package")
     (:file "component")
     (:file "entity")
     (:file "system")
     (:file "world")))
   (:module misc
    :components
    ((:file "rove-utils")))))
