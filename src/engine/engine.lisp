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
    :documentation "Engine main window")
   (renderer :documentation "Current renderer"
             :initform nil
             :accessor engine-renderer))
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

(defun make-window (title width height)
  (sb-int:with-float-traps-masked (:overflow :invalid)
    (sdl2:create-window :x :centered :y :centered
                        :title title :w width :h height
                        :flags '(:shown :opengl))))

(defun get-main-window ()
  (slot-value *engine* 'main-window))

(defun make-engine (&optional options)
  (delete-engine)
  (setf *engine* (make-instance 'engine))
  (unless options
    (setf options (make-engine-options)))
  (setf (slot-value *engine* 'options) options))

(defun get-engine ()
  "Return engine isntance.
Asserts that it have been created earlier."
  (assert *engine* nil "Engine is not created")
  *engine*)

(defun add-manager (manager-type)
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

(defun blink (name &optional arg-list)
  (apply #'beacon:blink (cons (get-beacon name) arg-list)))

;;; Renderer
(defun install-renderer (renderer)
  (unless (null renderer)
    (assert (null (engine-renderer *engine*))
            nil "Renderer already installed"))
  (setf (engine-renderer *engine*) renderer))

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
                  (win (make-window title w h))
                  (ticks 0))
             (set-main-window win)
             (unwind-protect
                  (progn
                    (blink :before-start)
                    (sdl2:with-sdl-event (sdl-event)
                      (setf ticks (sdl2:get-ticks))
                      (loop :while (eq (slot-value *engine* 'state) :running)
                            ;; process-events is defined in events.lisp
                            :do (progn
                                  (let* ((current-ticks (sdl2:get-ticks))
                                         (delta (- current-ticks ticks)))
                                    (setf ticks current-ticks)
                                    (lbge.engine.events:process-events *engine* sdl-event)
                                    (blink :on-loop (list delta))))))
                    (sb-int:with-float-traps-masked (:invalid)
                      (sdl2:destroy-window win))))))
      (sdl2:sdl-quit))))

(defun start ()
  (setf (slot-value *engine* 'state) :running)
  (engine-loop))
