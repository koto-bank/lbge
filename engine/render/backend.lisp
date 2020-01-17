(in-package :lbge.render.backend)

(defclass backend ()
  ()
  (:documentation "Base class for all backends"))

(defgeneric init (backend window))
(defgeneric clear (backend))
(defgeneric render (backend renderer))
(defgeneric present (backend))
(defgeneric deinit (backend))
