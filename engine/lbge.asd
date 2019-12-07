(asdf:defsystem :lbge
  :license "BSD-2-Clause"
  :depends-on (:alexandria :sdl2 :cffi :cl-autowrap)
  :components
  ((:module base
    :components
    ((:file "hash-map")))
   (:module image-loader
    :components
    ((:file "packages")
     (:file "image")
     (:file "image-loader")
     (:file "tga")))
   (:module math
    :components
    ((:file "package")
     (:file "common")
     (:file "vector")
     (:file "matrix")))
   (:module assets
    :pathname "base/asset"
    :components
    ((:file "package")
     (:file "asset")
     (:file "asset-handler")
     (:file "asset-manager")))
   (:module engine
    :components
    ((:file "package")
     (:file "window")
     #+linux (:file "window.linux")
     (:file "events")
     (:file "engine")))
   (:module render
    :components
    ((:file "render")))
   (:module ecs
    :components
    ((:file "package")
     (:file "component")
     (:file "entity")
     (:file "system")
     (:file "world")))))
