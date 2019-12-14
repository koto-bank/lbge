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
    ((:file "hash-map")))
   (:module math
    :components
    ((:file "package")
     (:file "common")
     (:file "vector")
     (:file "matrix")))
   (:module render
    :components
    ((:file "package")
     (:module window
      :components
      ((:file "window")
       #+linux (:file "window.linux")
       #+os-macosx (:file "window.macos")))
     (:file "render")))))
