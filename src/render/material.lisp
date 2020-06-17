(in-package :lbge.render.material)

(defclass material ()
  ((parameters :documentation "Material uniform list"
               :accessor parameters)
   (samplers :documentation "Material texture samplers"
             :accessor samplers)
   (shader :documentation "Material shader"
           :accessor shader))
  (:documentation "Graphics material"))
