(in-package :lbge.render.backend)

(defclass backend () ()
  (:documentation "Protocol class for backends.
Each backend should implement the methods for this protocol,
renderer interacts with backend through this protocol"))

(defgeneric init (backend window))
(defgeneric clear (backend))
(defgeneric render (backend renderer))
(defgeneric present (backend))
(defgeneric deinit (backend))
(defgeneric add-shader (backend shader-key)
  (:documentation "Shader-key is a keyword"))
(defgeneric shader-list (backend))
(defgeneric use-shader (backend shader-key))
