(in-package :lbge.hash)

(defclass hash ()
  (internal))

(defun make-hash (&rest pairs)
  (let ((h (make-instance 'hash)))
    (setf (slot-value h 'internal) (make-hash-table))
    (apply #'hash-set (cons h pairs))
    h))

(defun get-internal (hash)
  (slot-value hash 'internal))

(defun hash-get (hash key &optional default)
  (gethash key (slot-value hash 'internal) default))

(defun hash-set (hash &rest pairs)
  (assert (evenp (length pairs)) nil "Odd number of hash-set arguments")
  (loop
    :for (key value) :on pairs :by #'cddr :do
      (setf (gethash key (slot-value hash 'internal)) value)))

(defun hash-equal (hash-1 hash-2)
  (equalp (slot-value hash-1 'internal)
          (slot-value hash-2 'internal)))
