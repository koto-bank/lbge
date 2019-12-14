(in-package :lbge.render)

(defun initialize-backend (window backend)
  (assert (eq backend :opengl) nil "For now only OpenGL is supported")
  ())
