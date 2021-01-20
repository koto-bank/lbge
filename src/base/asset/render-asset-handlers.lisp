(in-package :lbge.asset)

;;; Shader source
(define-asset shader-source ()
  ((source :initarg :source :reader shader-source)))

(define-asset-handler shader-source (manager key)
  (let ((file-path (find-path-by-path-key manager key)))
    (log:debug "File path ~S" file-path)
    (log:debug "Asset key ~S" key)
    (unless file-path
      (return (make-instance 'asset :state :error)))
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
      (make-instance 'shader-source :source (reverse shader-lines)
                                    :state :loaded))))

;;; Image
(define-asset image-asset (lbge.image:image) ())

(define-asset-handler image-asset (manager key)
  (let ((file-path (lbge.asset:find-path-by-path-key manager key))
        (image-asset  (make-instance 'image-asset :state :error)))
    (log:debug "File path ~S" file-path)
    (log:debug "Asset key ~S" key)
    (unless file-path
      (return image-asset))
    (let ((image (lbge.image:load-image file-path)))
      (when image
        (lbge.image:copy-image image-asset image)
        [image-asset.state setf :loaded])
      image-asset)))

;;; Material
(define-asset material-asset (lbge.render.material:material) ())

(define-asset-handler material-asset (manager key) ())
