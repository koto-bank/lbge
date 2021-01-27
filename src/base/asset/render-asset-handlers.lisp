(in-package :lbge.asset)

;;; Shader source
(define-asset shader-source ()
  ((source :initarg :source :reader shader-source)))

(define-asset-handler shader-source (manager key)
  (let ((file-path (find-path-by-path-key manager key)))
    (log:debug "File path ~S" file-path)
    (log:debug "Asset key ~S" key)
    (unless file-path
      (return (make-asset 'shader-source
                          key
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
      (make-asset 'shader-source key
                  :source (reverse shader-lines)
                  :state :loaded))))

;;; Image
(define-asset image-asset (lbge.image:image) ())

(define-asset-handler image-asset (manager key)
  (let ((file-path (lbge.asset:find-path-by-path-key manager key))
        (image-asset  (make-asset 'image-asset key :state :error)))
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
(define-asset texture-asset (lbge.render.texture:texture)
  ((image :type image-asset :dep t)))

(define-asset-handler texture-asset (manager key)
  (let ((file-path (find-path-by-path-key manager key))
        (texture (make-asset 'texture-asset key)))
    (unless file-path
      (return texture))
    (deserialize texture file-path)
    (setf [texture.image]
          (load-asset manager [texture.image.key]))))

;;; Material
(define-asset material-asset (lbge.render.material:material) ())

(define-asset-handler material-asset (manager key) ())
