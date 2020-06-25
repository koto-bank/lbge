(in-package :lbge.render.material)

(defclass material ()
  ((uniforms :documentation "Material uniform list"
               :accessor uniforms)
   (textures :documentation "Material textures list. Maximum number of textures is 16. Uniform names in shaders are: sampler0, sampler1, etc."
             :accessor textures)
   (shader :documentation "Material shader"
           :accessor shader))
  (:documentation "Graphics material"))
