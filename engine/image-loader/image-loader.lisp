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
             :type string)
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
   If image format unsupported throws error."
  (cond-extension path
    (".tga" (lbge.image-loader:tga path))
    (t (error "Unsupported image format"))))

(defmacro cond-extension (path &rest clouses)
  "Takes path and list of clouses. Each clouse has the form:
   (%file-extension-str% %result-expr%).
   If `path` has %file-extensuin-str% then %result-expr% will returned."
  (when (and path clouses)
    `(if (search ,(caar clouses) path :from-end t)
         ,(cadar clouses)
         (cond-extension ,path ,(cadr clouses)))))
