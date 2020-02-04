(in-package :lbge.render)

(defclass batch ()
  ((vertices :documentation "Vertex array. A simple-vector"
             :initform (vector))
   (indices :documentation "Index array. A simple-vector"
            :initform (vector))))

(defclass render-object ()
  ((batches :documentation "Adjustable vector of batches"
            :initform (make-arrray 0 :adjustable t)
            :initarg :batches)
   (backend-data :documentation "Backend-dependent data. Opaque"))
  (:documentation "Render object. The thing that contains backend-independent info for rendering"))

(defun make-render-object (batches)
  (make-instance 'render-object
                 :batches (make-array 0 :initial-contents batches
                                        :adjustable t)))

(defun add-batch (object batch)
  (vector-push batch (slot-value 'batches object)))
