(defpackage lbge.image-loader
  (:use :cl)
  (:export load-image make-image))
(in-package lbge.image-loader)

(defclass image ()
  ((width :reader  width
          :initarg :width
          :initform nil
          :type integer)
   (height :reader  height
           :initarg :height
           :initform nil
           :type integer)
   (channels :reader  channels
             :initarg :channels
             :initform nil
             :type (vector (unsigned-byte 8)))
   (data :reader  data
         :initarg :data
         :initform nil
         :type string)))

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
  (ccase (pathname-type path)
    ("tga" (lbge.image-loader:tga path))))
