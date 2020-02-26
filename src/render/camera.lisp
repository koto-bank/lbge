(in-package :lbge.render)

(defclass camera ()
  ((matrix :documentation "Camera matrix"
           :initform nil
           :reader camera-projection-matrix)
   (view-matrix :documentation "Camera view matrix"
              :initform nil
              :accessor camera-view-matrix)))

(defclass ortho-camera (camera)
  ((left)
   (right)
   (top)
   (bottom)
   (near)
   (far)))

(defun make-ortho-camera (&key left right top bottom near far view)
  (let ((cam (make-instance 'ortho-camera))
        (proj-matrix (m:make-ortho-projection
                    :left left
                    :right right
                    :top top
                    :bottom bottom
                    :near near
                    :far far)))
    (with-slots (matrix view-matrix
                 (c-left left) (c-right right)
                 (c-top top) (c-bottom bottom)
                 (c-near near) (c-far far))
        cam
      (setf matrix proj-matrix
            view-matrix view
            c-left left
            c-right right
            c-top top
            c-bottom bottom
            c-near near
            c-far far))
    cam))
