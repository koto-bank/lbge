(defpackage :lbge-engine-test
  (:use :cl)
  (:export :run))

(in-package :lbge-engine-test)

(defun run ()
  (lbge.engine:delete-engine)
  (lbge.engine:make-engine)
  (lbge.engine.events:add-event-handlers
    (:keyup
     (:keysym keysym)
     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
       (sdl2:push-event :quit))
     (format t "Pressed ~S key~%" (sdl2:scancode keysym)))
    (:mousebuttonup
     (:button button)
     (format t "Pressed mouse button ~S~%" button)))
  (lbge.engine:start))
