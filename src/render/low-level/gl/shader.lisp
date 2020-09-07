(in-package :lbge.render.gl)

(defclass gl-shader (s:shader)
  ((shader-map :documentation "Map from shader-type to shader sources"
               :initform (h:make-hash))
   (status :initform :unknown)
   (handle :documentation "Internal shader program handle")
   (model-view-uniform :documentation "Model-view matrix uniform location in the shader" :initform nil)
   (projection-uniform :documentation "Projection matrix uniform location in the shader" :initform nil)
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

(defun to-glsl-name (name)
  (substitute #\_ #\- (string-downcase (string name))))

(defmacro gl-uniform (fun shader name x y z w)
  `(cond
     ,@(loop
         :for v :in (list w z y t)
         :for args :on (list w z y x)
         :collect `(,v (funcall ,fun (gl:get-uniform-location
                                      (slot-value ,shader 'handle) ,name)
                                ,@(reverse args))))))

(defmacro set-vector (x y &optional z w)
  `(setf ,@(loop
             :for var :in (remove nil (list w z y x))
             :append `(,var (,(find-symbol (string var) "LBGE.MATH") ,x)))))

(defmethod s:set-uniform ((shader gl-shader) name x &optional y z w)
  (let ((n (to-glsl-name name))
        (function #'gl:uniformi))
    (typecase x
      (m:float2 (set-vector x y))
      (m:float3 (set-vector x y z))
      (m:float4 (set-vector x y z w)))
    (when (member (class-of x)
                  (mapcar #'find-class
                          '(m:float2
                            m:float3
                            m:float4
                            float))
                  :test #'closer-mop:subtypep)
      (setf function #'gl:uniformf))
    (gl-uniform function shader n x y z w)))

(defun get-uniform-location (shader name)
  (ax:switch ((string-upcase (string name)) :test #'string=)
    ("MODEL-VIEW" (slot-value shader 'model-view-uniform))
    ("PROJECTION" (slot-value shader 'projection-uniform))
    (t (gl:get-uniform-location (slot-value shader 'handle)
                                (to-glsl-name name)))))

(defmethod s:set-uniform-matrix ((shader gl-shader) name matrix)
  (let ((uniform-location (get-uniform-location shader name)))
    (gl:uniform-matrix uniform-location
                       (m:mat-size matrix)
                       (vector (m:in-vec matrix)))))

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
  (with-slots (model-view-uniform projection-uniform handle) shader
    (setf model-view-uniform (gl:get-uniform-location handle "model_view"))
    (assert (not (= model-view-uniform -1)) nil
            "Invalid model_view location in shader ~A. Maybe it was optimized out?"
            handle)
    (setf projection-uniform (gl:get-uniform-location handle "projection"))
    (assert (not (= projection-uniform -1)) nil
            "Invalid projection-uniform location in shader ~A. Maybe it was optimized out?"
            handle)))

(defmethod s:set-texture ((shader gl-shader) name (texture gl-texture) num)
  (gl:active-texture num)
  (gl:bind-texture (slot-value texture 't:target)
                   (slot-value texture 'handle))
  (s:set-uniform shader name num))

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
            (setf status :ready))))
      (loop
        :for id :in attached-shaders
        :do (gl:detach-shader handle id)
            (gl:delete-shader id))
      (when (eq status :error)
        (log:error "Shader compilation failed")
        (log:error log))
      (when (eq status :ready)
        (log:info "Shader successfully compiled and linked!")
        (set-default-uniforms s))
      (when (> (length log) 0)
        (log:info "Compilation log: ~A" log)))))

(defmethod s:get-compile-log ((s gl-shader))
  (slot-value s 'log))

(defmethod s:delete-shader ((s gl-shader)))

(objective-cl:enable)
(defmethod s:get-uniform ((s gl-shader) name)
  (/= -1 (gl:get-uniform-location [s.handle] (to-glsl-name name))))
(objective-cl:disable)
