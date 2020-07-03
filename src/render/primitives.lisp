(in-package :lbge.render)

(defun assemble-vertex-array (attributes)
  (let ((attr-len (length (car attributes))))
    (assert (loop
              :for a :in attributes
              :always (= (length a) attr-len))
            nil "All attribute lists must have equal lenght")
    (let ((result (make-array (* (length attributes)
                                 attr-len)))
          (i 0))
      (apply (ax:curry #'mapcar
                       (lambda (&rest vals)
                         (mapcar (lambda (value)
                                   (setf (aref result i)
                                         value)
                                   (incf i))
                                 vals)))
             attributes)
      result)))

(defun unzip-attribute-list (attribute-list)
  (loop
    :for (a v) :on attribute-list :by #'cddr
    :collect a :into attributes
    :collect v :into values
    :finally (return (values attributes values))))

(defun assemble-render-object (batch vertices indices transform additional-attributes)
  (multiple-value-bind (attributes values)
      (unzip-attribute-list additional-attributes)
    (with-slots ((verts vertices) (inds indices)) batch
      (setf verts
            (assemble-vertex-array
             (nconc (list vertices) values))
            inds indices))
    (make-render-object (list batch)
                        (make-semantics
                         (nconc (list '(:vertex :float 4))
                                attributes))
                        transform)))

(defun make-rect (&key w h (transform (m:make-transform))
                    additional-attributes)
  "Create a rectangle primitive.
If additional-attributes list is provided, it should have the folloving form, e.g.:
 ((:color :float 3) ((1.0 0.0 0.0) (0.0 1.0 0.0) (0.0 0.0 1.0) (0.0 1.0 1.0))
  (:texcoord :float 2) ((1.0 0.0) (0.0 0.0) (0.0 1.0) (1.0 1.0)))"
  (assert (> w 0.0f0) nil "Width must be greater than zero, current value: ~S" w)
  (assert (> h 0.0f0) nil "Height must be greater than zero, current value: ~S" h)
  (let* ((b (make-instance 'batch))
         (w/2 (/ w 2.0f0))
         (h/2 (/ h 2.0f0))
         (-w/2 (- w/2))
         (-h/2 (- h/2))
         (verts (list (m:make-float4 -w/2 h/2 0.0f0 1.0f0)
                      (m:make-float4 w/2 h/2 0.0f0 1.0f0)
                      (m:make-float4 w/2 -h/2 0.0f0 1.0f0)
                      (m:make-float4 -w/2 -h/2 0.0f0 1.0f0))))
    (assemble-render-object b verts (vector 0 2 1 0 3 2) transform
                            (append (list '(:texcoord :float 2)
                                          (list (m:make-float2 0.0 1.0)
                                                (m:make-float2 1.0 1.0)
                                                (m:make-float2 1.0 0.0)
                                                (m:make-float2 0.0 0.0))))
                            additional-attributes)))

(defun make-triangle (&key size (transform (m:make-transform))
                        additional-attributes)
  (assert (> size 0.0f0) nil "Triangle size must be positive, current value: ~S" size)
  (let* ((b (make-instance 'batch))
         (size/2 (/ size 2.0f0))
         (-size/2 (- size/2))
         (r-circ (/ size (sqrt 3.0f0)))
         (-r-insc (- (/ r-circ 2)))
         (verts (list (m:make-float4 -size/2 -r-insc 0.0f0 1.0f0)
                      (m:make-float4 0.0f0 r-circ 0.0f0 1.0f0)
                      (m:make-float4 size/2 -r-insc 0.0f0 1.0f0))))
    (assemble-render-object b verts (vector 0 2 1) transform additional-attributes)))

(defun make-circle (&key radius (vert-num 32) (transform (m:make-transform))
                      additional-attributes)
  (make-ellipse :r-x radius :r-y radius :vert-num vert-num
                :transform transform
                :additional-attributes additional-attributes))

(defun make-ellipse (&key r-x r-y (vert-num 32) (transform (m:make-transform))
                       additional-attributes)
  (assert (> vert-num 2) nil "Cant make an ellipse with ~A vertices" vert-num)
  (let ((step (/ (* 2 pi) vert-num))
        (inds (make-array  (list (* 3 vert-num))))
        (verts (make-array (list (+ 1 vert-num))))
        (base-ind 0))
    (dotimes (i vert-num)
      (let ((angle (* i step)))
        (setf (aref verts i) (m:make-float4 (* r-x (cos angle)) (* r-y (sin angle)) 0.0f0 1.0f0)
              (aref inds base-ind) i
              (aref inds (+ base-ind 1)) (+ i 1)
              (aref inds (+ base-ind 2)) vert-num
              base-ind (+ 3 base-ind))))

    (setf (aref verts vert-num) (m:make-float4 0.0f0 0.0f0 0.0f0 1.0f0)
          (aref inds (- (* 3 vert-num) 2)) (aref inds 0))
    (log:debug "Ellipse vertices: ~A" verts)
    (log:debug "Ellipse indices: ~A" inds)
    (make-render-object (list (make-instance 'batch :indices inds :vertices verts))
                        (make-semantics ((:vertex :float 4)))
                        transform)))

(defun make-ring (&key in-r out-r (vert-num 32) (transform (m:make-transform))
                    additional-attributes)
  (assert (> vert-num 2) nil "Cant make an ring with ~A vertices" vert-num)
  (let ((step (/ (* 2 pi) vert-num))
        (inds (make-array  (list (* 6 vert-num))))
        (verts (make-array (list (* 2 vert-num))))
        (base-ind 0))
    (dotimes (i vert-num)
      (let ((angle (* i step))
            (base-vert (* 2 i)))
        (setf (aref verts base-vert) (m:make-float4 (* out-r (cos angle)) (* out-r (sin angle)) 0.0f0 1.0f0)
              (aref verts (1+ base-vert)) (m:make-float4 (* in-r (cos angle)) (* in-r (sin angle)) 0.0f0 1.0f0)

              (aref inds base-ind) base-vert
              (aref inds (+ base-ind 1)) (+ base-vert 3)
              (aref inds (+ base-ind 2)) (+ base-vert 1)
              (aref inds (+ base-ind 3)) base-vert
              (aref inds (+ base-ind 4)) (+ base-vert 2)
              (aref inds (+ base-ind 5)) (+ base-vert 3)
              base-ind (+ 6 base-ind))))
    (let ((last-index (1- (* 6 vert-num))))
      (setf (aref inds last-index) 1)
      (setf (aref inds (- last-index 1)) 0)
      (setf (aref inds (- last-index 4)) 1))
    (make-render-object (list (make-instance 'batch :indices inds :vertices verts))
                        (make-semantics ((:vertex :float 4)))
                        transform)))
