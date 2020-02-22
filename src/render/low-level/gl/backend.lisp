(in-package :lbge.render.gl)

(defclass gl-backend (b:backend)
  ((context :documentation "GL context" :initform nil)
   (shader-map :documentation "Map of all shaders. Key is shader name"
               :initform (h:make-hash))
   (active-shader :documentation "Current active shader")
   (window :documentation "SDL window" :initform nil)

   ;; TODO: move to separate structure, supprot multiple layouts
   (vao :documentation "VAO" :initform nil)
   (vertex-bo :documentation "VBO" :initform 0)
   (last-vertex-index :initform 0 :documentation "Index of last vertex in VBO")
   (last-index-index :initform 0 :documentation "Index of last index in IBO")
   (index-bo :documentation "IBO" :initform 0)
   (total-vertex-size :initform 0 :documentation "Total number of bytes in VBO")
   (total-index-size :initform 0 :documentation "Total number of bytes in IBO")))

(defmethod b:init ((backend gl-backend) window &optional info)
  "Init GL backend.
info is an alist, which may contain following keys: :gl-version - a
cons pair of maj . min context version"
  (with-slots (context (win window) vao-vector) backend
    (assert (null context) nil
            "Context already initialized for renderer")
    (sb-int:with-float-traps-masked (:invalid)
      (let ((version (cadr (assoc :gl-version info))))
        (when version
          (sdl2:gl-set-attr :context-major-version (car version))
          (sdl2:gl-set-attr :context-minor-version (cdr version))
          (sdl2:gl-set-attr sdl2-ffi:+sdl-gl-context-profile-mask+
                            sdl2-ffi:+sdl-gl-context-profile-core+)))
      (setf context (sdl2:gl-create-context window)))
    (setf win window)
    (gl:enable :depth-test)
    (gl:enable :cull-face)
    (ensure-vao backend)))

(defun ensure-vao (backend)
  ;; Temporary implementaion! We should support VAO creation
  ;; for any new buffer semantics and other state options.
  ;; For now create only one vao if its not present
  (with-slots (vao vertex-bo index-bo) backend
    (let ((new-vao (gl:gen-vertex-array))
          (vertex-buf (gl:gen-buffer))
          (index-buf (gl:gen-buffer)))

      (setf vao new-vao
            vertex-bo vertex-buf
            index-bo index-buf)
      (gl:bind-vertex-array new-vao)
      (gl:enable-vertex-attrib-array 0) ; vertices

      ;; Create vertex buffer
      (gl:bind-buffer :array-buffer vertex-buf)
      (gl:vertex-attrib-pointer 0 4 :float nil 0 0)
      (gl:with-gl-array (a :float :count #xFFFF) ; max 65535 vertices
        (gl:buffer-data :array-buffer :static-draw a))

      ;; Create index buffer
      (gl:bind-buffer :element-array-buffer index-buf)
      (gl:with-gl-array (a :unsigned-short :count #xFFFF) ; max 65535 indices
        (gl:buffer-data :element-array-buffer :static-draw a))

      (gl:bind-buffer :array-buffer 0)
      (gl:bind-buffer :element-array-buffer 0)
      (gl:bind-vertex-array 0))))

(defmethod b:clear ((backend gl-backend))
  (gl:clear :color-buffer-bit :depth-buffer-bit))

(defmethod b:render ((backend gl-backend) renderer)
  (with-slots ((objs r:render-objects)
               (camera r:current-camera))
      renderer
    (with-slots (vao active-shader) backend
      (gl:bind-vertex-array vao)
      (gl:use-program (slot-value active-shader 'handle))
      (mapcar (ax:curry #'ensure-buffer-data backend) objs)
      (mapcar (ax:curry #'draw-object camera backend) objs)
      (gl:use-program 0)
      (gl:bind-vertex-array 0))))

(defun add-shader (backend name shader)
  (with-slots (shader-map) backend
    (assert (null (h:hash-get shader-map name))
            nil "Shader with name ~A already exists" name)
    (h:hash-set shader-map name shader)))

(defmethod b:make-shader ((backend gl-backend) shader-name)
  (let ((shader (make-instance 'gl-shader)))
    (setf (slot-value shader 'handle)
          (gl:create-program))
    (add-shader backend shader-name shader)
    shader))

(defmethod b:use-shader ((backend gl-backend) (shader gl-shader))
  (with-slots (status handle) shader
    (assert (eq :compiled status) nil
            "Shader must be compiled in order to be used")
    (setf (slot-value backend 'active-shader) shader)
    (set-default-uniforms shader)))

(defmethod b:present ((backend gl-backend))
  (sb-int:with-float-traps-masked (:invalid)
    (sdl2:gl-swap-window (slot-value backend 'window))))

(defmethod b:deinit ((backend gl-backend))
  (sdl2:gl-delete-context (slot-value backend 'context)))
