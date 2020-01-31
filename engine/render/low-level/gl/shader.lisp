(in-package :lbge.render.gl)

(defclass gl-shader (s:shader)
  ((shader-map :documentation "Map from shader-type to shader sources"
               :initform (h:make-hash))
   (status :initform :unknown)
   (handle :documentation "Internal shader program handle")
   (log :documentation "Error logs storage" :initform nil))
  (:documentation "OpenGL shader program"))

(defmethod s:get-status ((s gl-shader))
  (slot-value s 'status))

(defmethod s:add-stage ((s gl-shader) stages)
  (with-slots (shader-map) s
    (do ((s stages (cddr s)))
        ((null s))
      (let ((stage (car s))
            (source (cadr s)))
        (assert (null (h:hash-get shader-map stage)))
        (h:hash-set shader-map (list (list stage source)))))))

(defun get-compile-errors (id)
  (gl:get-shader-info-log id))

(defun get-link-errors (handle)
  (gl:get-program-info-log handle))

(defun compile-shader-part (shader type source)
  (let ((id (gl:create-shader type))
        (handle (slot-value shader 'handle)))
    (gl:attach-shader handle id)
    (gl:shader-source id source)
    (gl:compile-shader id)
    (let ((errors (get-compile-errors id)))
      (if (> (length errors) 0)
          (progn
            (setf (slot-value shader 'status :error)
                  (slot-value shader 'log errors))
            :error)
          id))))

(defmethod s:compile-shader ((s gl-shader))
  (let ((type-to-gl-type
          '((:vertex :vertex-shader)
            (:fragment :fragment-shader)
            (:geometry :geometry-shader)
            (:compute :compute-shader)))
        (attached-shaders (list)))
    (with-slots ((sources shader-map)
                 handle
                 status)
        s
      (loop
        :for type :being :the :hash-keys :in (h:get-internal sources)
          :using (hash-value source)
        :do (let ((res (compile-shader-part
                        s
                        (cadr (assoc type type-to-gl-type))
                        source)))
              (when (eq res :error)
                (return))
              (push res attached-shaders)))
      (when (eq :unknown                ; not error
                status)
        (gl:link-program handle)
        (let ((errors (get-link-errors handle)))
          (if (> (length errors) 0)
            (setf status :error
                  (slot-value s 'log) errors)
            (setf status :compiled))))
      (loop
        :for id :in attached-shaders
        :do (gl:detach-shader handle id)
            (gl:delete-shader id)))))

(defmethod s:get-errors ((s gl-shader))
  (slot-value s 'log))

(defmethod s:delete-shader ((s gl-shader)))
