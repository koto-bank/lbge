(in-package :lbge.image-loader.tga)

(defun tga (path)
  "Returns the `lbge.image-loader.image::image` object for `path` file."
  (let* ((raw (alexandria:read-file-into-byte-vector path))
         (width (get-width raw))
         (height (get-height raw)))
    (make-image :width width
                :height height
                :channels (get-channels raw)
                :data (get-data (* width height) raw))))

(defun get-width (raw)
  (+ (* (aref raw 13) 256)
     (aref raw 12)))

(defun get-height (raw)
  (+ (* (aref raw 15) 256)
     (aref raw 14)))

(defun get-channels (raw)
  (format nil "rgba~a" (aref raw 16)))

(defun get-data (image-lenght raw)
  (let* ((header-lenght 17)
         (image-id-length (aref raw 0))
         (color-map-included? (aref raw 1))
         (first-data-byte
           (+ header-length
              image-id-length
              (unless (= color-map-included? 0)
                (* (+ (* (aref raw 6) 256)
                      (aref raw 5))
                   (aref raw 7))))))
    (subseq raw first-data-byte (+ first-data-byte (- image-lenght 1)))))
