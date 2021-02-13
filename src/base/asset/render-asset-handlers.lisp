(in-package :lbge.asset)

;;; Shader source
(define-asset shader-source ()
  ((source :initarg :source)))

(define-asset-handler shader-source (manager key)
  (let ((file-path (find-path-by-path-key manager key)))
    (log:debug "File path ~S" file-path)
    (log:debug "Asset key ~S" key)
    (unless file-path
      (return (make-asset key
                          :state :error)))
    (let ((shader-lines nil))
      (with-open-file (shader-file
                       file-path
                       :direction :input
                       :if-does-not-exist :error)
        (do ((line (read-line shader-file nil)
                   (read-line shader-file nil)))
            ((null line))
          (unless (string= "" (u:trim-newlines line))
            (setf shader-lines (cons (format nil "~a~%" line)
                                     shader-lines)))))
      (make-asset key
                  :source (reverse shader-lines)
                  :state :loaded))))

;;; Image
(define-asset image-asset (lbge.image:image) ())

(define-asset-handler image-asset (manager key)
  (let ((file-path (lbge.asset:find-path-by-path-key manager key))
        (image-asset  (make-asset key :state :error)))
    (log:debug "File path ~S" file-path)
    (log:debug "Asset key ~S" key)
    (unless file-path
      (return image-asset))
    (let ((image (lbge.image:load-image file-path)))
      (when image
        (lbge.image:copy-image image-asset image)
        [image-asset.state setf :loaded])
      image-asset)))

;;; Texture
(define-asset texture-asset ()
  ((image-asset :type image-asset :dep t)
   (backend-texture
    :accessor backend-texture
    :documentation "Texture object created and managed by rendering
    backend")))

(define-asset-handler texture-asset (manager key)
  (let ((file-path (find-path-by-path-key manager key))
        (texture (make-asset key :state :error)))
    (unless file-path
      (return texture))
    (s:deserialize-file texture file-path)
    [texture.state setf :loaded]
    texture))

;;; Material
(define-asset material-asset ()
  ((vertex-shader-source :dep t :type shader-source)
   (fragment-shader-source :dep t :type shader-source)
   (backend-material
    :accessor backend-material
    :documentation "Material created and managed by the rendering backend")))

(define-asset-handler material-asset (manager key)
  (let ((file-path (find-path-by-path-key manager key))
        (material (make-asset key :state :error)))
    (unless file-path
      (return material))
    (s:deserialize-file material file-path)
    [material.state setf :loaded]
    material))

(defun build-shader (backend sources)
  ;; Sources is a list in format
  ;; (:vertex <vertex-source>
  ;;  :fragment <fragment-source>)
  (let ((shader (b:make-shader backend)))
    (sh:add-stage shader sources)
    (sh:compile-shader shader)
    shader))

(defun make-sampler-keyword (num)
  (alexandria:make-keyword (format nil "SAMPLER~A" num)))

(defun build-material (backend material textures uniforms)
  (let ((shader (build-shader
                 backend
                 (list :vertex [material.vertex-shader-source.source]
                       :fragment [material.fragment-shader-source.source])))
        (backend-material (make-instance 'mat:material)))
    (setf [material.backend-material] backend-material
          [backend-material.mat:shader] shader)
    (loop
      for (name slot-name) on uniforms by #'cddr
      do (mat:add-uniform backend-material name
                          (slot-value material slot-name)))
    (loop
      for texture-slot in textures
      for i from 0
      do (let ((texture (slot-value material texture-slot)))
           (setf [texture.backend-texture]
                 (b:make-texture backend
                                 :image [texture.image-asset]
                                 :target :texture-2d
                                 :format :rgba8))
           (mat:add-texture backend-material (make-sampler-keyword i)
                            [texture.backend-texture])))
    material))
