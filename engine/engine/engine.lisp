(in-package :lbge.engine)

(defclass engine ()
  ((managers
    :initform (make-hash)
    :documentation "Hash table containing various managers")
   (state
    :initform nil
    :documentation "Current engine state. Possible values: nil :running
:stopping")
   (options
    :documentation "Options passed at startup")
   (main-window
    :documentation "Engine main window"))
  (:documentation "The engine"))

(defstruct engine-options
  (window-title "LBGE Window")
  (window-w 800)
  (window-h 600))

(defvar *engine* nil
  "Running engine instance.
Only one allowed per application.")

(defun delete-engine ()
  (setf *engine* nil))

(defun set-main-window (window)
  (setf (slot-value *engine* 'main-window) window))

(defun make-engine (&rest options)
  (assert (null *engine*) nil
          "Engine already created")
  (setf *engine* (make-instance 'engine))
  (unless options
    (setf options (make-engine-options)))
  (setf (slot-value *engine* 'options) options))

(defun get-engine ()
  "Return engine isntance.
Asserts that it have been created earlier."
  (assert *engine* nil "Engine is not created")
  *engine*)

(defun add-manager (engine manager-type)
  "Add new manager to engine"
  (let ((managers (slot-value *engine* 'managers)))
    (assert (null (get-hash managers manager-type)) nil
            "Manager ~S already registered" manager-type)))

(defun get-manager (manager-type)
  (let ((managers (slot-value *engine* 'managers)))
    (get-hash (managers manager-type))))

(defun engine-loop ()
  (sdl2:with-init (:audio :video :timer :joystick :gamecontroller :noparachute)
    (sdl2:with-window (win :flags '(:shown))
      (sdl2:with-sdl-event (sdl-event)
        (loop :while (eq (slot-value *engine* 'state) :running)
              ;; process-events is defined in events.lisp
              :do (lbge.engine.events:process-events *engine* sdl-event))))))

(defun start ()
  (setf (slot-value *engine* 'state) :running)
  (sdl2:make-this-thread-main #'engine-loop))
