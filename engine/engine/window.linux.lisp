(in-package :lbge.engine)

(defclass window-linux (window)
  ()
  (:documentation "Linux sdl window implementation"))

(defmethod initialize-backend ((window window-linux) backend)
  (assert (eq backend :opengl) nil "For now only OpenGL is supported"))

(defmethod finalize-window ((window window-linux))
  )
