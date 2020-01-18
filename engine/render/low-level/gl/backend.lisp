(in-package :lbge.render.gl)

(defclass gl-backend (b:backend)
  ((context :documentation "GL context" :initform nil)
   (window :documentation "SDL window" :initform nil)))

(defmethod b:init ((backend gl-backend) window)
  "Init GL backend"
  (with-slots (context (win window)) backend
    (assert (null context) nil
            "Context already initialized for renderer")
    (setf context (sdl2:gl-create-context window))
    (setf win window)))

(defmethod b:clear ((backend gl-backend))
  (gl:clear))

(defmethod b:render ((backend gl-backend) renderer))

(defmethod b:present ((backend gl-backend))
  (sdl2:gl-swap-window (slot-value backend 'window)))

(defmethod b:deinit ((backend gl-backend))
  (sdl2:gl-delete-context (slot-value backend 'context)))
