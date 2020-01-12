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
   (beacons
    :documentation "Available engine beacons. Current list:
:before-start - blinks just before the start of the main loop
:on-loop - blinks on every loop iteration"
    :initform (list (beacon:make :before-start)
                    (beacon:make :on-loop)))
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

(defun create-window (title width height)
  (sb-int:with-float-traps-masked (:overflow :invalid)
    (sdl2:create-window :x :centered :y :centered
                        :title title :w width :h height
                        :flags '(:shown))))

(defun get-main-window ()
  (slot-value *engine* 'main-window))

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
    (assert (null (hash-get managers manager-type)) nil
            "Manager ~S already registered" manager-type)))

(defun get-manager (manager-type)
  (let ((managers (slot-value *engine* 'managers)))
    (hash-get (managers manager-type))))

;;; Beacons
(defun link (name callback)
  (let ((beacon (get-beacon name)))
    (assert beacon nil
            "Beacon ~S not found int the engine" name)
    (beacon:link beacon callback)))

(defun get-beacon (name)
  (find name (slot-value *engine* 'beacons) :key #'beacon:name))

(defun blink (name)
  (beacon:blink (get-beacon name)))

(defun engine-loop ()
  (let ((options (slot-value *engine* 'options))
        (init-flags (autowrap:mask-apply
                     'sdl2::sdl-init-flags
                     '(:audio :video :timer :joystick :gamecontroller :noparachute))))
    (unwind-protect
         (progn
           (sb-int:with-float-traps-masked (:invalid :overflow)
             (sdl2::check-rc (sdl2-ffi.functions:sdl-init init-flags)))
           (let* ((title (engine-options-window-title options))
                  (h (engine-options-window-h options))
                  (w (engine-options-window-w options))
                  (win (create-window title w h)))
             (set-main-window win)
             (unwind-protect
                  (blink :before-start)
                  (sdl2:with-sdl-event (sdl-event)
                    (loop :while (eq (slot-value *engine* 'state) :running)
                          ;; process-events is defined in events.lisp
                          :do (progn (lbge.engine.events:process-events *engine* sdl-event)
                                     (blink :on-loop))))
               (sb-int:with-float-traps-masked (:invalid)
                 (sdl2:destroy-window win)))))
      (sdl2:sdl-quit))))

(defun start ()
  (setf (slot-value *engine* 'state) :running)
  (engine-loop))
