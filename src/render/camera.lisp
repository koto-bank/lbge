(in-package :lbge.render)

(defclass camera ()
  ((matrix :documentation "Camera matrix"
           :initform nil
           :reader camera-projection-matrix)
   (view-matrix :documentation "Camera view matrix"
              :initform nil
              :accessor camera-view-matrix)))

(defun make-ortho-camera (&key left right top bottom near far view)
  (let ((cam (make-instance 'camera)))
    (with-slots (matrix view-matrix) cam
      (setf matrix (m:make-ortho-projection
                    :left left
                    :right right
                    :top top
                    :bottom bottom
                    :near near
                    :far far)
            view-matrix view))
    cam))
