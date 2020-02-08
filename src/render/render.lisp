(in-package :lbge.render)

(defclass renderer ()
  ((backend-type :documentation "Keyword denoting backend:
:gl :vk :mtl :dx12" :initarg :backend)
   (backend :documentation "Rendering backend, providing actual drawing interface")
   (cameras :documentation "Available cameras" :initform (list))
   (current-camera :documentation "Current rendering camera")
   (render-objects :documentation "A list of all render objects"
                   :initform (list)))
  (:documentation "Renderer instance"))

(defun make-renderer (backend)
  (let ((renderer (make-instance 'renderer :backend backend)))
    (cond ((eq backend :gl)
           (setf (slot-value renderer 'backend)
                 (make-instance 'lbge.render.gl:gl-backend)))
          (t (assert nil nil "Backend ~S not supported" backend)))
    renderer))

(defun add-camera (renderer camera)
  "Adds a camera to the renderer"
  (push camera (slot-value renderer 'cameras)))

(defun set-current-camera (renderer camera)
  (assert (find camera (slot-value renderer 'cameras))
          nil "Cannot set camera as active, since it's not in camera list")
  (setf (slot-value renderer 'current-camera) camera))

(defun add-object (renderer obj)
  (push obj (slot-value renderer 'render-objects)))

(defun add-objects (renderer &rest objects)
  (with-slots (render-objects) renderer
    (mapcar (lambda (obj)
              (push obj render-objects))
            objects)))

(defun get-backend (renderer)
  (slot-value renderer 'backend))

(defun render (renderer)
  (b:render (slot-value renderer 'backend) renderer))
