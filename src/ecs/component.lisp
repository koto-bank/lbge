(in-package :lbge.ecs)

(defclass component ()
  ((entity
    :accessor comp-entity
    :documentation "Entity that the component belongs to"))
  (:documentation "Base class for all components"))

(defun get-sibling (component sibling-type))
