(defpackage :lbge.serialization
  (:use :cl)
  (:export
   :serialize
   :deserialize))

(defgeneric serialize (object))

(defgeneric deserialize (object form &optional options))
