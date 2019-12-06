(in-package :lbge.engine)

(defclass engine ()
  ((managers
    :initform (make-hash)
    :documentation "Hash table containing various managers")
   (state
    :initform nil
    :documentation "Current engine state. Possible values: nil :running
:stopping")
   (main-window
    :documentation "Engine main window"))
  (:documentation "The engine"))

(defvar *engine* nil
  "Running engine instance.
Only one allowed per application.")

(defun delete-engine ()
  (setf *engine* nil))

(defun start (engine)
  (setf (slot-value engine 'state)
        :running))

(defun set-main-window (engine window)
  (setf (slot-value engine 'main-window) window))

(defun make-engine ()
  (assert (null *engine*) nil
          "Engine already created")
  (setf *engine* (make-instance 'engine)))

(defun get-engine ()
  "Return engine isntance.
Asserts that it have been created earlier."
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

(defun start (engine)
  (setf (slot-value engine 'state) :running))

(defun engine-loop (engine)
  (sdl2:with-init (:everything)
    (sdl2:with-window (win  :flags '(:shown))
      (let ((quit nil))
        (sdl2:with-sdl-event (sdl-event)
          (loop :while (eq (slot-value engine 'state) :running)
                ;; process-events is defined in events.lisp
                :do (process-events engine sdl-event)))))))
