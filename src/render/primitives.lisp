(in-package :lbge.render)

(defun make-rect (&key w h (transform (m:make-transform)))
  (assert (> w 0.0f0) nil "Width must be greater than zero, current value: ~S" w)
  (assert (> h 0.0f0) nil "Height must be greater than zero, current value: ~S" h)
  (let* ((b (make-instance 'batch))
         (w/2 (/ w 2.0f0))
         (h/2 (/ h 2.0f0))
         (-w/2 (- w/2))
         (-h/2 (- h/2)))
    (with-slots (vertices indices) b
      (setf vertices (vector (m:make-float3 -w/2 h/2 0.0f0)
                             (m:make-float3 w/2 h/2 0.0f0)
                             (m:make-float3 w/2 -h/2 0.0f0)
                             (m:make-float3 -w/2 h/2 0.0f0))
            indices (vector 0 1 2 2 3 1)))
    (make-render-object (list b))))

(defun make-triangle (&key size (transform (m:make-transform)))
  (assert (> size 0.0f0) nil "Triangle size must be positive, current value: ~S" size)
  (let* ((b (make-instance 'render-batch))
         (size/2 (/ size 2.0f0))
         (-size/2 (- size/2))
         (r-circ (/ size (sqrt 3.0f0)))
         (-r-insc (- (/ r-circ 2))))
    (with-slots (vertices indices) b
      (setf vertices (vector (m:make-float3 -size/2 -r-insc 0.0f0)
                             (m:make-float3 0.0f0 r-circ 0.0f0)
                             (m:make-float3 size/2 -r-insc 0.0f0))
            indices (vector 0 1 2)))
    (make-render-object (list b))))

(defun make-ellipse (&key w h (transform (m:make-transform)))
  ;; Temp!
  (make-rect :w w :h h :transform transform))

(defun make-ring (&key w h thickness (transform (m:make-transform)))
  ;; Temp!
  (make-triangle :size w :transform transform))
