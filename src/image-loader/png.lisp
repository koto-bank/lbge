(in-package :lbge.image)

(defun png-channels (image)
  (let ((channels (alexandria:switch ((png:image-channels image))
                    (1 "R")
                    (3 "RGB")
                    (4 "RGBA")))
        (bits (png:image-bit-depth image)))
    (alexandria:make-keyword (format nil "~A~A" channels bits))))

(defun png-data (image)
  (let* ((w (png:image-width image))
         (h (png:image-height image))
         (channels-per-pixel (png:image-channels image))
         (data (make-array (* w h channels-per-pixel)
                           :element-type (list 'unsigned-byte
                                               (png:image-bit-depth image)))))
    (loop for i below h do
      (loop for j below w do
        (loop for c below channels-per-pixel do
          (setf (aref data (+ c (* j channels-per-pixel) (* i w channels-per-pixel)))
                (aref image (- h i 1) j c)))))
    data))

(defun load-png (path)
  (with-open-file (png path
                       :direction :input
                       :if-does-not-exist :error
                       :element-type '(unsigned-byte 8))
    (let ((image (png:decode png :preserve-alpha t)))
      (make-image :width (png:image-width image)
                  :height (png:image-height image)
                  :channels (png-channels image)
                  :data (png-data image)))))
