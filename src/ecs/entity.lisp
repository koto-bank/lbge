(in-package :lbge.ecs)

(define-symbol-macro null-entity 0)

(defclass entity-storage ()
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
   (available
    :initform 0
    :documentation "Number of entities available for recycling")
   (create-next
    :initform 1
    :documentation "Next entity to be created")
   (recycle-next
    :initform null-entity
    :documentation "Next entity to be recycled")))

(defmacro is-null (entity)
  (= entity null-entity))

(defmethod initialize-instance :after ((storage entity-storage) &key)
  :documentation "Add default null entity"
  (with-slots (sparse packed) storage
    (ss:insert null-entity sparse packed)))

(defun storage-delete-entity (storage entity)
  "Delete the entity.

The algorithm:
1. Increase the gen part of the entity
2. Swap recycle-next with the entity in the packed array
3. Increase available counter"
  (with-slots (sparse packed
               available recycle-next)
      storage
    (unless (ss:existsp entity sparse packed)
      ;; Already deleted
      (return-from storage-delete-entity nil))
    (let ((new-recycle-next entity))
      (setf
       ;; Increase gen
       (ss:gen new-recycle-next)
       (1+ (ss:gen entity))

       ;; Set recycle-next to the current one
       (aref packed
             (aref sparse (ss:id entity)))
       recycle-next

       ;; Current one will be recycled next
       recycle-next
       new-recycle-next

       ;; Increase available counter
       available
       (1+ available)))))

(defun storage-create-entity (storage)
  "Create entity (or recycle one created earlier).

The recycle algorithm:
1. Restore previous recycle-next entity
2. Decrease available count
3. Restore original recycle-next as live and return it"
  (with-slots (create-next
               recycle-next
               available
               sparse packed)
      storage
    (when (zerop available)
      ;; Create brand new entity, nothing to recycle
      (ss:insert create-next sparse packed)
      (let ((ret create-next))
        (incf create-next)
        (return-from storage-create-entity ret)))

    (let ((ret recycle-next)
          (id-new (ss:id recycle-next)))
      (setf
       ;; Restore previous recycle-next
       recycle-next
       (aref packed
             (aref sparse id-new))

       ;; Decrease available count
       (available) (1- available)

       ;; Restore new entity
       (aref packed
             (aref sparse id-new))
       ret)
      ret)))

(defun entity-existsp (storage entity)
  (with-slots (sparse packed)
      storage
    (ss:existsp entity sparse packed)))
