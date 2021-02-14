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
  (window-w 1280)
  (window-h 800)
  (time-per-frame 16) ; ms per frame, for 62.5 fps
  (clear-color (m:make-float4 0.68f0 0.7f0 0.76f0 1.0f0))
  (opengl-version '(4 . 1)))

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
          (set-main-window (make-window (engine-options-window-title options)
                                        (engine-options-window-w options)
                                        (engine-options-window-h options))))
      (t ()
        (stop-engine))))
  ;; Setup managers
  (let ((a (add-manager 'asset:asset-manager)))
    (asset:add-root a :root "."))       ; default root
  (let* ((renderer (render:make-renderer :gl))
         (backend (render:renderer-backend renderer)))
    (render-back:init backend
                      (get-main-window)
                      `((:gl-version ,(engine-options-opengl-version options))))
    (log:info "OpenGL version string: ~a~%" (gl:gl-version))
    (log:info "GLSL version string: ~a~%" (gl:glsl-version))
    (let ((cc (engine-options-clear-color options)))
      (gl:clear-color (m:x cc) (m:y cc) (m:z cc) (m:w cc)))
    (install-renderer renderer)
    (render:resize-viewport renderer
                            (engine-options-window-w options)
                            (engine-options-window-h options))))

(defun stop-engine ()
  (log:info "Stopping engine...")
  (with-slots (main-window) *engine*
    (when (and (slot-boundp *engine* 'main-window) main-window)
      (log:info "Found SDL2 window, deleting...")
      (sb-int:with-float-traps-masked (:invalid)
        (sdl2:destroy-window main-window))
      (setf main-window nil))
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
  (let ((ticks 0)
        (time-per-frame (engine-options-time-per-frame [*engine*.options])))
    (unwind-protect
         (sdl2:with-sdl-event (sdl-event)
           (blink :before-start)
           (setf ticks (sdl2:get-ticks))
           (loop :while (eq (slot-value *engine* 'state) :running)
                 ;; process-events is defined in events.lisp
                 :do (progn
                       (let* ((current-ticks (sdl2:get-ticks))
                              (delta (- current-ticks ticks)))
                         (lbge.engine.events:process-events *engine* sdl-event)
                         (blink :on-loop (list delta))
                         (let ((loop-time (- (sdl2:get-ticks) current-ticks)))
                           (when (< loop-time time-per-frame)
                             (sdl2:delay (- time-per-frame loop-time))))
                         (setf ticks current-ticks)))))
      (stop-engine))))

(defun start ()
  (setf (slot-value *engine* 'state) :running)
  (engine-loop))
