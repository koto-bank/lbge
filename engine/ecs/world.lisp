(in-package :lbge.ecs)

(defclass world ()
  ((world-entity :accessor :world-entity)
   (systems)
   (entity-component-map)))

(defun add-system (world system-type))

(defun get-components (entity)
  ())
