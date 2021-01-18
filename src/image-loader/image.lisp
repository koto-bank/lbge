(in-package :lbge.image)

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

(defun copy-image (image-1 image-2)
  [image-1.width setf image-2.width
   image-1.height image-2.height
   image-1.channels image-2.channels
   image-1.data image-2.data])

(defun make-image (&key width height channels data)
  "Wrapper arround `image` class for making image objects
   in subpackages."
  (make-instance 'image
                 :width width
                 :height height
                 :channels channels
                 :data data))
