(defpackage lbge.image-loader.tga
  (:use :cl :lbge.image-loader)
  (:export tga))
(in-package lbge.image-loader.tga)

(defun tga (path)
  "Returns the `lbge.image-loader:image` object for `path` file."
  (let ((raw (alexandria:read-file-into-byte-vector path))
    (make-image :width    (get-width raw)
                :height   (get-height raw)
                :channels (get-channels raw)
                :data raw))))

(defun get-width (raw)
  (+ (aref raw 12) (aref raw 13)))

(defun get-height (raw)
  (+ (aref raw 14) (aref raw 15)))

(defun get-channels (raw)
  (format nil "rgb~a" (aref raw 16)))
