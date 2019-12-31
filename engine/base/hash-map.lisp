(defpackage :lbge.hash
  (:use :cl)
  (:export :make-hash
           :get-hash
           :set-hash))

(in-package :lbge.hash)

(defclass hash ()
  (internal))

(defun make-hash ()
  (let ((h (make-instance 'hash)))
    (setf (slot-value h 'internal)
          (make-hash-table))
    h))

(defun get-hash (hash key)
  (gethash key hash))

(defmacro set-hash (hash (key value))
  `(setf
    (gethash ,key (slot-value ,hash 'internal))
    ,value))
