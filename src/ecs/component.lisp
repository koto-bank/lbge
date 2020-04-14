(in-package :lbge.ecs)

(defclass component ()
  ((entity
    :accessor comp-entity
    :documentation "Entity that the component belongs to"))
  (:documentation "Base class for all components"))

(defclass component-storage ()
  ((sparse
    :initform (make-array '(0)
                          :adjustable t
                          :element-type '(unsigned-byte 64))
    :documentation "Sparse set of the entity storage")
   (packed
    :initform (make-array '(0)
                          :adjustable t
                          :fill-pointer 0
                          :element-type '(unsigned-byte 64))
    :documentation "Packed array of entities")
   (type
    :initform nil
    :initarg :type
    :accessor storage-component-type
    :documentation "Type of stored components")
   (data
    :initform nil
    :documentation "Packed array of components")))

(defmethod initialize-instance :after ((storage component-storage) &key)
  (with-slots (type data) storage
    (setf data (make-array '(0)
                           :adjustable t
                           :fill-pointer 0
                           :element-type type))))

(defun get-entities (storage)
  (slot-value storage 'packed))

(defun add-component (storage entity component)
  (assert (eq (type-of component) (storage-component-type storage))
          nil "Component type ~S doesn't match storage component type ~S"
          (type-of component) (storage-component-type storage))
  (with-slots (packed sparse data) storage
    (ss:insert entity sparse packed component data)))

(defun remove-component (storage entity)
  (assert (eq (type-of component) (storage-component-type storage))
          nil "Component type ~S doesn't match storage component type ~S"
          (type-of component) (storage-component-type storage))
  (with-slots (packed sparse data) storage
    (ss:remove entity sparse packed data)))

(defun storage-get-component (storage entity &optional comp-type)
  (when comp-type
    (assert (eq comp-type (storage-component-type storage))
            nil "Component type ~S doesn't match storage component type ~S"
            comp-type (storage-component-type storage)))
  (with-slots (packed sparse data) storage
    (ss:get entity sparse packed data)))

(defun storage-size (storage)
  (array-dimension (slot-value storage 'data) 0))
