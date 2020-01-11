(in-package :lbge.hash)

(defclass hash ()
  (internal))

(defun make-hash (&optional pairs)
  (let ((h (make-instance 'hash)))
    (setf (slot-value h 'internal) (make-hash-table))
    (set-hash h pairs)
    h))

(defun get-hash (hash key)
  (gethash key (slot-value hash 'internal)))

(defun set-hash (hash &optional pairs)
  (dolist (pair pairs hash)
    (setf (gethash (first pair) (slot-value hash 'internal))
          (second pair))))

(defun equal-hash (hash-1 hash-2)
  (equalp (slot-value hash-1 'internal)
          (slot-value hash-2 'internal)))
