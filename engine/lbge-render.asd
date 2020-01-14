(defpackage :lbge.render.system
  (:use :cl :asdf))

(in-package :lbge.render.system)

(asdf:defsystem :lbge-render
  :license "BSD-2-Clause"
  :depends-on (:alexandria :sdl2
               :cffi :cl-autowrap
               :cl-opengl)
  :components
  ((:module base
    :components
    ((:file "packages")
     (:file "hash-map")))
   (:module math
    :components
    ((:file "package")
     (:file "common")
     (:file "vector")
     (:file "matrix")
     (:file "quaternion")
     (:file "transform")))
   (:module render
    :components
    ((:file "packages")
     (:file "render")))))
