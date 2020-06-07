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
  (when *engine*
    (stop-engine)
    (setf *engine* nil)))

(defun set-main-window (window)
  (setf (slot-value *engine* 'main-window) window))

(defun make-window (title width height)
  (sb-int:with-float-traps-masked (:overflow :invalid)
    (sdl2:create-window :x :centered :y :centered
                        :title title :w width :h height
                        :flags '(:shown :opengl))))

(defun get-main-window ()
  (assert *engine* nil "Engine doesn't exist, can't get main window")
  (slot-value *engine* 'main-window))

(defun make-engine ()
  (delete-engine)
  (setf *engine* (make-instance 'engine)))

(defun get-engine ()
  "Return engine isntance.
Asserts that it have been created earlier."
  (assert *engine* nil "Engine is not created")
  *engine*)

(defun add-manager (manager-type)
  "Add new manager to engine"
  (let ((managers (slot-value *engine* 'managers)))
    (assert (null (hash-get managers manager-type)) nil
            "Manager ~S already registered" manager-type)
    (let ((manager (make-instance manager-type)))
      (hash-set managers manager-type manager)
      manager)))

(defun get-manager (manager-type)
  (let ((managers (slot-value *engine* 'managers)))
    (hash-get managers manager-type)))

(defun init-engine (&optional options)
  (unless options
    (setf options (make-engine-options)))
  (setf (slot-value *engine* 'options) options)

  (let ((init-flags (autowrap:mask-apply
                     'sdl2::sdl-init-flags
                     '(:audio :video :timer :joystick :gamecontroller :noparachute))))
    (handler-case
        (progn
          (sb-int:with-float-traps-masked (:invalid :overflow)
            (sdl2::check-rc (sdl2-ffi.functions:sdl-init init-flags)))
          (let* ((title (engine-options-window-title options))
                 (h (engine-options-window-h options))
                 (w (engine-options-window-w options))
                 (win (make-window title w h)))
            (set-main-window win)))
      (t ()
        (stop-engine))))
  (fs:set-app-root-to-system 'lbge-render-test)
  ;; Setup managers
  (let ((a (add-manager 'asset:asset-manager)))
    (asset:add-root a :root ".")
    (asset:add-handler a (make-instance 'asset:glsl-asset-handler))
    (asset:add-handler a (make-instance 'image:image-asset-handler)))
  (let* ((renderer (render:make-renderer :gl))
         (backend (render:renderer-backend renderer)))
    (render-back:init backend
                      (get-main-window)
                      '((:gl-version (4 . 1))))
    (format t "OpenGL version string: ~a~%" (gl:gl-version))
    (format t "GLSL version string: ~a~%" (gl:glsl-version))
    (gl:clear-color 0.02f0 0.05f0 0.05f0 1.0f0)
    (install-renderer renderer)
    (backend:resize-viewport backend renderer
                       1440 900)))

(defun stop-engine ()
  (let ((win (get-main-window)))
    (when win
      (sb-int:with-float-traps-masked (:invalid)
        (sdl2:destroy-window win)))
    (sdl2:sdl-quit)))

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

(defun get-renderer ()
  (engine-renderer *engine*))

(defun engine-loop ()
  (let ((ticks 0))
    (unwind-protect
                (sdl2:with-sdl-event (sdl-event)
           (blink :before-start)
           (setf ticks (sdl2:get-ticks))
           (loop :while (eq (slot-value *engine* 'state) :running)
                 ;; process-events is defined in events.lisp
                 :do (progn
                       (let* ((current-ticks (sdl2:get-ticks))
                              (delta (- current-ticks ticks)))
                         (setf ticks current-ticks)
                         (lbge.engine.events:process-events *engine* sdl-event)
                         (blink :on-loop (list delta))))))
      (stop-engine))))

(defun start ()
  (setf (slot-value *engine* 'state) :running)
  (engine-loop))
