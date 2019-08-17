(in-package :lbge.image-loader)

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

(defun load-image (path)
  "Takes path to an image and returns `image` structure.
   If the image format is unsupported throws error."
  (ccase (intern (pathname-type path))
    ('|tga| (lbge.image-loader.tga:tga path))))
