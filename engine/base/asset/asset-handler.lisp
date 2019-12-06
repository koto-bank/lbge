(in-package :lbge.asset)

(defclass asset-handler ()
  ()
  (:documentation "Base class for all asset handlers"))

(defgeneric handler-load-asset (handler key))
(defgeneric handler-create-asset (handler key))
