(defpackage :lbge.render
  (:use :cl :lbge.hash)
  (:export
   :renderer

   ;;; Windows
   :create-window
   :window
   :initialize-backend))
