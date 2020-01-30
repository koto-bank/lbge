(in-package :lbge.hash)

(defclass hash ()
  (internal))

(defun make-hash (&optional pairs)
  (let ((h (make-instance 'hash)))
    (setf (slot-value h 'internal) (make-hash-table))
    (hash-set h pairs)
    h))

(defun get-internal (hash)
  (slot-value hash 'internal))

(defun hash-get (hash key &optional default)
  (gethash key (slot-value hash 'internal) default))

(defun hash-set (hash &optional pairs)
  (dolist (pair pairs hash)
    (setf (gethash (first pair) (slot-value hash 'internal))
          (second pair))))

(defun hash-equal (hash-1 hash-2)
  (equalp (slot-value hash-1 'internal)
          (slot-value hash-2 'internal)))
