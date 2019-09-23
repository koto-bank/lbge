(in-package :lbge.engine)

(defclass engine ()
  ((managers
    :initform (make-hash)
    :documentation "Hash table containing various managers"))
  (:documentation "The engine"))

(defvar *engine* nil
  "Running engine instance.
Only one allowed per application.")

(defun delete-engine ()
  (setf *engine* nil))

(defun make-engine ()
  (assert (null *engine*) nil
          "Engine already created")
  (setf *engine* (make-instance 'engine)))

(defun get-engine ()
  "Return engine isntance.
Asserts that it have been craeted earlier."
  (assert *engine* nil "Engine is not created")
  *engine*)

(defun add-manager (engine manager-type)
  "Add new manager to engine"
  (let ((managers (slot-value engine 'managers)))
    (assert (null (get-hash managers manager-type)) nil
            "Manager ~S already registered" manager-type)))

(defun get-manager (engine manager-type)
  (let ((managers (slot-value engine 'managers)))
    (get-hash (managers manager-type))))
