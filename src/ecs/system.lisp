(in-package :lbge.ecs)

(defclass system ()
  ((world
    :documentation "World the system belongs to"
    :initarg :world
    :reader get-world)
   (component-storages
    :accessor component-storages
    :initform (list)
    :documentation "List component storages for component types"))
  (:documentation "Base class for all systems"))

(defmacro bind-components (system-type &rest comp-types)
  `(eval-when (:compile-toplevel)
     (log:debug "Binding ~S to system ~S" (list ,@comp-types) ,system-type)
     (setf (get ,system-type :components)
           (list ,@comp-types))))

(defgeneric update (system dt)
  (:documentation
   "Updates the system.
Every system must implement this method"))

(defmethod update ((sys system) dt)
  (error "All systems must implement the update method! System ~A is not an exception" sys))
