(in-package :lbge.render)

(defclass batch ()
  ((vertices :documentation "Vertex array. A simple-vector"
             :initform (vector))
   (indices :documentation "Index array. A simple-vector"
            :initform (vector))))

(defclass render-object ()
  ((batches :documentation "Adjustable vector of batches"
            :initform (make-arrray 0 :adjustable t
                                     :fill-pointer 0)
            :initarg :batches)
   (transform :documentation "Transform matrix"
              :initarg :transform)
   (backend-data :documentation "Backend-dependent data. Opaque"))
  (:documentation "Render object. The thing that contains backend-independent info for rendering"))

(defun make-render-object (batches &optional (transform (m:make-transform)))
  (make-instance 'render-object
                 :batches (make-array (length batches)
                                      :initial-contents batches
                                      :adjustable t
                                      :fill-pointer (length batches))
                 :transform transform))

(defun add-batch (object batch)
  (vector-push-extend batch (slot-value 'batches object)))
