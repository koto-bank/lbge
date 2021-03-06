(in-package :lbge.render.backend)

(defclass backend () ()
  (:documentation "Protocol class for backends.
Each backend should implement the methods for this protocol,
renderer interacts with backend through this protocol"))

(defgeneric init (backend window &optional info)
  (:documentation "Render intialization.
info is custom info, what can be placed there, depends on backend"))
(defgeneric clear (backend))
(defgeneric render (backend renderer))
(defgeneric resize-viewport (backend renderer width height))
(defgeneric present (backend))
(defgeneric deinit (backend))

;;; Shaders
(defgeneric make-shader (backend))
(defgeneric shader-list (backend))

;;; Textures
(defgeneric make-texture (backend &rest args))

;;; Misc
(defgeneric print-statistics (backend &optional stream args))
(defgeneric get-total-frames (backend))
