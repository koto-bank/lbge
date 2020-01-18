(in-package :lbge.render)

(defclass camera ()
  ((matrix :documentation "Camera matrix")
   (transform :documentation "Camera transform"
              :initform (m:make-transform))))

(defun make-ortho-camera (&key left right top bottom near far)
  (let ((cam (make-instance 'camera)))
    (with-slots (matrix) cam
        (setf matrix (m:make-ortho-projection
                      :left left
                      :right right
                      :top top
                      :bottom bottom
                      :near near
                      :far far)))
    cam))
