(in-package :lbge.render.gl)

(defclass gl-shader (b:shader)
  ((shader-map :documentation "Map from shader-type to shader asset"
               :initform (h:make-hash))
   (status :initform :unknown))
  (:documentation "OpenGL shader program"))

(defmethod get-status ((s gl-shader))
  (slot-value s 'status))

(defmethod add-stage ((s gl-shader) stage)
  (with-slots (shader-map) s
    (assert (null (h:hash-get shader-map :)))))

(defmethod compile ((s gl-shader)))

(defmethod link ((s gl-shader)))

(defmethod get-errors ((s gl-shader)))

(defmethod delete ((s gl-shader)))
