(defpackage :lbge.serialization
  (:use :cl)
  (:local-nicknames (:u :lbge.utils))
  (:export
   :serializable-class
   :serializable

   :serialize
   :deserialize

   :serialize-stream
   :deserialize-stream

   :serialize-file
   :deserialize-file

   :serializable-direct-slot
   :serializable-effective-slot))

(in-package :lbge.serialization)

(defgeneric serialize (object))

(defgeneric deserialize (object form &optional options))

(defclass serializable-class (standard-class) ())

(defclass serializable-direct-slot (closer-mop:standard-direct-slot-definition)
  ((serializable
    :initarg :serialize
    :initform nil
    :accessor serializable-direct-slot-s)))

(defclass serializable-effective-slot (closer-mop:standard-effective-slot-definition)
  ((serializable
    :initform nil
    :accessor serializable-effective-slot-s)))

(defmethod closer-mop:direct-slot-definition-class ((class serializable-class) &rest initargs)
  (find-class 'serializable-direct-slot))

(defmethod closer-mop:effective-slot-definition-class ((class serializable-class) &rest initargs)
  (find-class 'serializable-effective-slot))

(defmethod closer-mop:compute-effective-slot-definition ((class serializable-class) name direct-slots)
  (let ((slot (call-next-method))
        (direct-slot (first direct-slots)))

    (when (closer-mop:subclassp (class-of direct-slot)
                                (find-class 'serializable-direct-slot))
      (setf (serializable-effective-slot-s slot)
            (serializable-direct-slot-s direct-slot)))
    slot))

(defmethod closer-mop:compute-slots ((class serializable-class))
  (let* ((all-slots (call-next-method))
         (serializable-slots (remove-if-not #'identity all-slots :key #'serializable-effective-slot-s))
         (slot-names (mapcar #'closer-mop:slot-definition-name serializable-slots)))
    (cons (make-instance 'serializable-effective-slot
                         :allocation :class
                         :allocation-class class
                         :name '%serializable-slots
                         :initfunction #'(lambda () slot-names)
                         :documentation "Slots to be [de]serializaed"
                         :initform slot-names)
          all-slots)))

(defmethod closer-mop:validate-superclass ((class serializable-class)
                                           (super standard-class))
  t)

(defclass serializable () () (:metaclass serializable-class))

(defmethod serialize ((object t))
  object)

(defmethod serialize ((object cons))
  (if (eq 'quote (car object))
    (serialize (cadr object))
    (cons :list
          (mapcar #'serialize object))))

(defmethod deserialize ((object null) form &optional options)
  ;; A special case: deserialize creating object
  (declare (ignore object))
  (when (atom form)
    (return-from deserialize form))
  (let ((head (car form))
        (tail (cdr form)))
    (cond
      ((keywordp head)
       (cond ((eq :list head)
              (mapcar (u:bind #'deserialize nil _)
                      tail))
             (t (assert nil nil "Unsupported deserialization key ~A" head))))
      ((symbolp head)
       (deserialize (make-instance head)
                    tail
                    options))
      (t (assert nil nil "Don't know how to deserialize ~A" form)))))

(defmethod serialize ((object serializable))
  (cons (type-of object)
        (loop
          for slot in [object.%serializable-slots]
          collect (list slot
                        (if (slot-boundp object slot)
                          (serialize (slot-value object slot))
                          nil)))))

(defmethod deserialize ((object serializable) form &optional options)
  (loop
    for slot in form do
      (let ((name (car slot))
            (value (cadr slot)))
        (setf (slot-value object name)
              (deserialize nil value options))))
  object)

(defun serialize-stream (object stream)
  (prin1 (serialize object) stream))

(defun deserialize-stream (object stream &optional options)
  (let ((form (read stream)))
    (if (null object)
      (deserialize object form options)
      (deserialize object (cdr form) options))))

(defun serialize-file (object-or-list file-path)
  (with-open-file (stream file-path
                          :direction :output
                          :if-exists :supersede)
    (if (listp object-or-list)
      (mapcar #'serialize-stream object-or-list)
      (serialize-stream object-or-list))))

(defun deserialize-file (object file-path &optional options)
  (with-open-file (stream file-path
                          :direction :input)
    (deserialize-stream object stream options)))
