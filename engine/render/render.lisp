(in-package :lbge.render)

(defclass renderer ()
  ((windows :documentation "List of windows")
   (main-window :documentation "Active window"))
  (:documentation "Renderer instance"))
