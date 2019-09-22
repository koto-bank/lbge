(in-package :lbge.image-loader.image)

(defclass image ()
  ((width :reader width
          :initarg :width
          :type integer)
   (height :reader height
           :initarg :height
           :type integer)
   (channels :reader channels
             :initarg :channels
             :type string)
   (data :reader data
         :initarg :data
         :type (vector (unsigned-byte 8)))))

(defun make-image (&key width height channels data)
  "Wrapper arround `image` class for making image objects
   in subpackages."
  (make-instance 'image
                 :width width
                 :height height
                 :channels channels
                 :data data))
