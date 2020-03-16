(in-package :lbge.animation)

(defclass animation ()
  ((key-points :initarg :key-points
               :documentation "A list of animation key points.
                A key point is a cons pair (time . value)")
   (duration :accessor duration
             :initarg :duration
             :documentation "Total duraion of the animation")
   (setter :initarg :setter
           :documentation "The setter closure. Accepts one parameter -
           new value to be set")
   (passed-time :initform 0
                :accessor passed-time
                :documentation "Time that has passed sice animation start")
   (interpolation-function
    :initarg :interpolation-function
    :documentation "Interpolation function. Takes as argument key point list and current time"))
  (:documentation "Describes change of some parameter in time"))

(defun time (key-point)
  (car key-point))

(defun value (key-point)
  (cdr key-point))

(defun make (key-points setter interpolation-function)
  (assert (> (length key-points) 1)
          nil "Cannot have animation with only one key point: ~A" key-points)
  (assert (= 0 (time (first key-points)))
          nil
          "First animation keyframe time must be 0")
  (let ((duration (- (time (car (last key-points)))
                     (time (first key-points)))))
    (make-instance 'animation
                   :key-points key-points
                   :duration duration
                   :setter setter
                   :interpolation-function interpolation-function)))

(defun update (animation dt)
  (with-slots (passed-time
               setter
               key-points
               duration
               interpolation-function)
      animation
    (setf passed-time (+ dt passed-time))
    (when (> passed-time duration)
      (setf passed-time (- passed-time duration)))
    (funcall setter (funcall interpolation-function key-points passed-time))))

;;; Interpolation functions
(defun linear-interpolation (key-points passed-time)
  (assert (<= passed-time (- (time (car (last key-points)))
                             (time (first key-points))))
          nil "Wrong time for linear interpolation!")
  (loop
    :for (k1 k2) :on key-points
    :do
       (when (and (>= passed-time (time k1))
                  (<= passed-time (time k2)))
         (return (math:lerp passed-time (time k1) (time k2)
                            (value k1) (value k2))))))
