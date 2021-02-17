(defpackage :lbge-system
  (:use :cl))

(in-package :lbge-system)

(asdf:defsystem :lbge
  :license "BSD-2-Clause"
  :depends-on (:alexandria :sdl2
               :rove
               :cffi :cl-autowrap
               :lbge-render
               :log4cl
               :png
               :closer-mop :cl-ppcre
               :objective-cl)
  :around-compile (lambda (next)
                    (uiop:symbol-call '#:objective-cl '#:enable)
                    (unwind-protect (funcall next)
                      (uiop:symbol-call '#:objective-cl '#:disable)))
  :serial t
  :components
  ((:module base
    :components
    ((:file "packages")
     (:file "hash-map")
     (:file "utils")
     (:file "beacon")
     (:file "timer")
     (:file "serialization")
     (:file "math-serialization")
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
    :serial t
    :components
    ((:file "package")
     (:file "utils")
     (:file "asset")
     (:file "asset-handler")
     (:file "asset-manager")
     (:file "sexp-asset-handler")
     (:file "render-asset-handlers")))
   (:module image-loader
    :components
    ((:file "packages")
     (:file "png")
     (:file "image")
     (:file "tga")
     (:file "image-loader")))
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
