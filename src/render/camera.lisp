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

(pub defun make-default-ortho-camera ()
   (make-ortho-camera
    :left -1.0f0 :right 1.0f0
    :top 0.75f0 :bottom -0.75f0
    :near -0.1f0 :far 3.0f0
    :view (m:make-look-at
           (m:make-float3 0.0 0.0 1.0)
           (m:make-float3 0.0 0.0 0.0)
           (m:make-float3 0.0 1.0 0.0))))

(defun make-ortho-camera (&key left right top bottom near far view)
  (let ((cam (make-instance 'ortho-camera))
        (proj-matrix (m:make-ortho-projection
                      :left left
                      :right right
                      :top top
                      :bottom bottom
                      :near near
                      :far far)))
    [cam.matrix setf proj-matrix
     cam.view-matrix view
     cam.left left
     cam.right right
     cam.top top
     cam.bottom bottom
     cam.near near
     cam.far far]
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
