(asdf:defsystem :lbge
  :license "BSD-2-Clause"
  :depends-on (:rove :alexandria)
  :components
  ((:module base
    :components
    ((:file "hash-map")))
   (:module image-loader
    :components
    ((:file "packages")
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
   (:module ecs
    :components
    ((:file "package")
     (:file "component")
     (:file "entity")
     (:file "system")
     (:file "world")))))
