(defpackage :lbge.animation
  (:use :cl)
  (:shadow :time)
  (:local-nicknames (:math :lbge.math))
  (:export
   :make
   :update
   :linear-interpolation))
