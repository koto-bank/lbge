(defpackage :lbge.math-serialization
  (:use :cl :lbge.serialization)
  (:local-nicknames (:m :lbge.math)))

(in-package :lbge.math-serialization)

(defmethod serialize ((vector m::floatn))
  (cons (alexandria:make-keyword (class-name (class-of vector)))
        (m:in-vec vector)))

(defmethod deserialize ((vector m::floatn) form &optional options)
  (setf [vector.m:in-vec]
        (apply #'vector form))
  vector)
