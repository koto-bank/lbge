(in-package :lbge.asset)

(defclass asset-handler ()
  ()
  (:documentation "Base class for all asset handlers"))

(defgeneric load-asset (handler key))
(defgeneric create-asset (handler key))
