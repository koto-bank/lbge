(in-package :lbge.render.gl)

(defclass gl-shader (s:shader)
  ((shader-map :documentation "Map from shader-type to shader sources"
               :initform (h:make-hash))
   (status :initform :unknown)
   (handle :documentation "Internal shader program handle")
   (model-view-uniform :documentation "Model-view matrix uniform location in the shader" :initform nil)
   (log :documentation "Compilation log" :initform ""))
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
        (h:hash-set shader-map stage source)))))

(defun get-compile-errors (id)
  (gl:get-shader-info-log id))

(defun get-link-errors (handle)
  (gl:get-program-info-log handle))

(defun add-to-log (shader lines)
  (with-slots (log) shader
    (setf log
          (concatenate 'string log lines))))

(defun compile-shader-part (shader type source)
  (let ((id (gl:create-shader type))
        (handle (slot-value shader 'handle)))
    (gl:attach-shader handle id)
    (gl:shader-source id source)
    (gl:compile-shader id)
    (add-to-log shader (get-compile-errors id))
    (let ((status (gl:get-shader id :compile-status)))
      (if (not status)
          (setf (slot-value shader 'status) :error)
          id))))

(defun set-default-uniforms (shader)
  (with-slots (model-view-uniform handle) shader
    (let ((location (gl:get-uniform-location handle "model_view")))
      (setf model-view-uniform location)
      (assert (not (= location -1)) nil
              "Invalid model_view location in shader ~A. Maybe it was optimized out?"
              handle))))

(defmethod s:compile-shader ((s gl-shader))
  (let ((type-to-gl-type
          '((:vertex :vertex-shader)
            (:fragment :fragment-shader)
            (:geometry :geometry-shader)
            (:compute :compute-shader)))
        (attached-shaders (list)))
    (with-slots ((sources shader-map)
                 handle
                 log
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
      (unless (eq :error status)
        (gl:link-program handle)
        (let ((link-status (gl:get-program handle :link-status)))
          (add-to-log s (get-link-errors handle))
          (if (not link-status)
            (setf status :error)
            (setf status :compiled))))
      (loop
        :for id :in attached-shaders
        :do (gl:detach-shader handle id)
            (gl:delete-shader id))
      (set-default-uniforms s))))

(defmethod s:get-compile-log ((s gl-shader))
  (slot-value s 'log))

(defmethod s:delete-shader ((s gl-shader)))
