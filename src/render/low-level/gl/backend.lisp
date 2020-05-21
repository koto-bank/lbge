(in-package :lbge.render.gl)

(defclass buffer-storage ()
  ((vao :documentation "VAO" :initform nil)
   (semantics :documentation "Specifies semantics of stored vertex buffer"
              :initform (assert nil nil "Must provide semantics on buffer storage creation")
              :initarg :semantics
              :accessor buffer-storage-semantics)
   (vertex-bo :documentation "VBO" :initform 0)
   (last-vertex-index :initform 0 :documentation "Index of last vertex in VBO")
   (last-index-index :initform 0 :documentation "Index of last index in IBO")
   (index-bo :documentation "IBO" :initform 0)
   (total-vertex-size :initform 0 :documentation "Total number of bytes in VBO")
   (total-index-size :initform 0 :documentation "Total number of bytes in IBO")))

(defclass gl-backend (b:backend)
  ((context :documentation "GL context" :initform nil)
   ;; TODO way to check if shader is comatible with buffer-storage?
   (shader-map :documentation "Map of all shaders. Key is shader name"
               :initform (h:make-hash))
   (buffer-storages :documentation "List of buffer storages each containing its objects"
                    :initform (list))
   ;; Todo: Move to material both textures and shaders
   (active-shader :documentation "Current active shader")
   (active-texture :documentation "Current texture")
   (window :documentation "SDL window" :initform nil)))

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
    (gl:enable :cull-face)))

(defun init-buffer-storage (new-buffer-storage)
  (with-slots (vao vertex-bo index-bo semantics) new-buffer-storage
    (let ((new-vao (gl:gen-vertex-array))
          (vertex-buf (gl:gen-buffer))
          (index-buf (gl:gen-buffer)))

      (setf vao new-vao
            vertex-bo vertex-buf
            index-bo index-buf)
      (gl:bind-vertex-array new-vao)
      (gl:bind-buffer :array-buffer vertex-buf)

      (with-slots (r:stride r:attributes-num
                   r:attribute-types r:attribute-sizes r:attribute-offsets)
          semantics

        (dotimes (i r:attributes-num)
          (let ((size (nth i r:attribute-sizes))
                (offset (nth i r:attribute-offsets))
                (type (nth i r:attribute-types)))
            (log:debug "Providing vertex attrib pointer ~S type ~S size ~S offset ~S stride ~S" i type size offset r:stride)
            (gl:enable-vertex-attrib-array i)
            (gl:vertex-attrib-pointer i size type nil r:stride (cffi:inc-pointer
                                                                (cffi:null-pointer) offset)))))
      ;; Fill vertex buffer
      (gl:with-gl-array (a :float :count #xFFFF) ; max 65535 vertices
        (gl:buffer-data :array-buffer :static-draw a))

      ;; Create index buffer
      (gl:bind-buffer :element-array-buffer index-buf)
      (gl:with-gl-array (a :unsigned-short :count #xFFFF) ; max 65535 indices
        (gl:buffer-data :element-array-buffer :static-draw a))

      (gl:bind-buffer :array-buffer 0)
      (gl:bind-buffer :element-array-buffer 0)
      (gl:bind-vertex-array 0)))
  new-buffer-storage)

(defun ensure-buffer-storage (backend render-object)
  "Returns buffer storage for render object, otherwise creates it"
  (with-slots (buffer-storages) backend
    (with-slots (r:semantics) render-object
      (let ((maybe-storage
              (find r:semantics buffer-storages
                    :test (lambda (r-sem buffer-storage)
                            (r:semantics= r-sem
                                          (buffer-storage-semantics
                                           buffer-storage))))))
        (when maybe-storage
          (log:debug "Found buffer storage for semantics ~S" r:semantics)
          (return-from ensure-buffer-storage maybe-storage)))
      (car (push (init-buffer-storage
                  (make-instance 'buffer-storage :semantics r:semantics))
                 buffer-storages)))))

(defmethod b:clear ((backend gl-backend))
  (gl:clear :color-buffer-bit :depth-buffer-bit))

(defmethod b:render ((backend gl-backend) renderer)
  (with-slots ((objs r:render-objects)
               (camera r:current-camera))
      renderer
    (with-slots (active-shader active-texture) backend
      (gl:use-program (slot-value active-shader 'handle))

      ;; Texture setting
      ;; TODO: move to material handling
      (gl:active-texture :texture0)
      (gl:bind-texture :texture-2d (slot-value active-texture 'handle))
      (s:set-uniform active-shader "sampler0" 0)

      (dolist (obj objs)
        (ensure-buffer-data backend obj))
      (dolist (obj objs)
        (draw-object camera backend obj))
      (gl:use-program 0))))

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

(defmethod b:resize-viewport ((backend gl-backend) renderer width height)
  (gl:viewport 0 0 width height)
  (r:adjust-camera-new-aspect (r:renderer-current-camera renderer)
                              (/ width height)
                              :y))

;;; Textures
(defmethod b:make-texture ((backend gl-backend) &rest args)
  (let ((texture (apply #'make-instance 'gl-texture args)))
    (t:texture-initialize texture)
    texture))

(defmethod b:use-texture ((backend gl-backend) (shader gl-shader) (texture gl-texture))
  (set-uniform shader "sampler0" (slot-value texture 'handle)))

(defmethod b:present ((backend gl-backend))
  (sb-int:with-float-traps-masked (:invalid)
    (sdl2:gl-swap-window (slot-value backend 'window))))

(defmethod b:deinit ((backend gl-backend))
  (sdl2:gl-delete-context (slot-value backend 'context)))

;;; Misc
(defun print-buffer-storage (buffer-storage stream)
  (with-slots (vao semantics last-vertex-index) buffer-storage
    (format stream "~
VAO:        ~A~%~
Semantics:  ~A~%~
Vertices:   ~A~%---------------~%"
            vao semantics (1+ last-vertex-index))))

(defmethod b:print-statistics ((backend gl-backend) &optional (stream t) args)
  (with-slots (buffer-storages shader-map) backend
    (loop
      :for storage :in buffer-storages :do
        (print-buffer-storage storage stream))))
