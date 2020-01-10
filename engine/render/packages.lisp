(defpackage :lbge.render
  (:use :cl :lbge.hash)
  (:export
   :renderer
   :make-renderer

   ;;; Windows
   :make-window
   :window
   :initialize-backend))

;;; low-level stuff
(defpackage :lbge.render.gl
  (:use :cl :lbge.render)
  (:export
   :make-context
   :compile-effect))
