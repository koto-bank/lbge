(in-package :lbge.render)

(defclass render-object ()
  ((vertices :documentation "Vertex array. A simple-vector")
   (indices :documentation "Index array. A simple-vector")
   (backend-data :documentation "Backend-dependent data. Opaque"))
  (:documentation "Render object. The thing that contains backend-independent info for rendering"))
