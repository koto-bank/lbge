(in-package :lbge.ecs)

(defclass system ()
  ((component-storages
    :documentation "List of component storages")
   (world :documentation "World the system belongs to"
          :reader get-world)
   (owned-component-types
    :documentation "List of component types, which the system owns (and thus stores)"))
  (:documentation "Base class for all systems"))

(defgeneric update (system dt)
  (:documentation
   "Updates the system.
Every system must implement this method"))

(defmethod update ((sys system) dt)
  (error "All systems must implement the update method! System ~A is not an exception" sys))
