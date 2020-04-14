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

(defun add-component-types (system component-types)
  (with-slots (component-storages) system
    (loop
      :for type :in component-types
      :do (progn
            (assert (null (find type component-storages
                                :key #'storage-component-type))
                    nil "Component ~A already stored in ~A" type system)
            (push (make-instance 'component-storage :type type) component-storages)))))

(defmethod initialize-instance :after ((sys system) &key)
  (let ((comp-types (get (type-of sys) :components)))
    (log:debug "Adding component storages for ~S to system ~S"
               comp-types
               (type-of sys))
    (add-component-types sys comp-types)))

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
