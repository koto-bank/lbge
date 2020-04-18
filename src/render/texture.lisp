(in-package :lbge.render.texture)

(defclass texture ()
  ((image :accessor texture-image
          :initarg :image
          :documentation "An instance of lbge.image, raw data to be loaded")
   (target :documentation "Texutre target (currently can be only :texture-2d)"
           :initarg :target)
   (format :accessor texture-format
           :initarg :format
           :documentation "Target texture format: :r8, :rg8, :rgb8, :rgba8")
   (release-image
    :initform t
    :initarg :release-image
    :documentation "Flag that says if the original image should be released after texture
have been loaded to the GPU"))
  (:documentation "Base class for backend-dependent textures"))

(defgeneric texture-initialize (texture)
  (:documentation "Initialize texture"))

(defgeneric texture-load (texture)
  (:documentation "Load texture to the GPU"))

(defmethod texture-load :after ((tex texture))
  (:documentation "Default method to automatically release image data")
  (when (slot-value tex 'release-image)
    (setf (slot-value tex 'image) nil)))
