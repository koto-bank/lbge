(defpackage lbge.image-loader.tga
  (:use :cl :lbge.image-loader)
  (:export tga))
(in-package lbge.image-loader.tga)

(defun tga (path)
  "Returns the `lbge.image-loader:image` object for `path` file."
  (let ((raw (alexandria:read-file-into-string path))
    (make-image :width    (get-width raw)
                :height   (get-height raw)
                :channels (get-channels raw)
                :data raw))))

(defun get-width (raw)
  (parse-integer (concatenate (subseq raw 13 14)
                              (subseq raw 12 13)
                 :radix 16)))

(defun get-height (raw)
  (parse-integer (concatenate (subseq raw 15 16)
                              (subseq raw 14 15)
                 :radix 16)))

(defun get-channels (raw)
  (format nil "rgb~a" (parse-integer (subseq raw 16 17)
                                     :radix 16))
