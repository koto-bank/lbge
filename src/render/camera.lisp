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

(defgeneric adjust-camera-new-aspect (camera aspect preserve-fov)
  (:documentation
   "Adjusts camera for new aspect ratio.
Preserve-fov is a keyword which tells which fov we should preserve, :x
or :y"))

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

(defmethod adjust-camera-new-aspect ((camera ortho-camera) aspect preserve-fov)
  (with-slots (left right top bottom near far matrix) camera
    (let*
        ((width (- right left))
         (height (- top bottom))

         (new-width/2 (if (eq preserve-fov :x)
                          (/ width 2)
                          (* (/ height 2) aspect)))
         (new-height/2 (if (eq preserve-fov :y)
                           (/ height 2)
                           (* (/ width 2) (/ 1 aspect))))

         (new-left (- new-width/2))
         (new-right new-width/2)
         (new-top new-height/2)
         (new-bottom (- new-height/2))
         (new-matrix (m:make-ortho-projection
                      :left new-left
                      :right new-right
                      :top new-top
                      :bottom new-bottom
                      :near near
                      :far far)))
      (setf matrix new-matrix
            left new-left
            right new-right
            top new-top
            bottom new-bottom))))
