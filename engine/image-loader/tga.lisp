(in-package :lbge.image-loader.tga)

(defun tga (path)
  "Returns the `lbge.image-loader.image::image` object for `path` file."
  (let ((raw (alexandria:read-file-into-byte-vector path)))
    (make-image :width (get-width raw)
                :height (get-height raw)
                :channels (get-channels raw)
                :data raw)))

(defun get-width (raw)
  (+ (ash (aref raw 13) 1) (aref raw 12)))

(defun get-height (raw)
  (+ (ash (aref raw 15) 1) (aref raw 14)))

(defun get-channels (raw)
  (format nil "rgba~a" (aref raw 16)))
