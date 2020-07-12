(in-package :lbge.render.material)

(defclass material ()
  ((uniforms :documentation "Material uniform list"
               :accessor uniforms)
   (textures :documentation "Material textures list. Maximum number of textures is 16. Uniform names in shaders are: sampler0, sampler1, etc."
             :accessor textures)
   (shader :documentation "Material shader"
           :accessor shader))
  (:documentation "Graphics material"))

(defun add-uniform (material name slot-name)
  "Name is name uniform name in the shader.
Slot-name is the name of corresponding slot in the shader"
  (push (cons name slot-name)
        (uniforms material)))

(defun set-uniforms (material)
  "Set all uniform parameters"
  (with-slots (shader uniforms) material
    (loop :for (name . slot) :in uniforms :do
      (s:set-uniform shader name (slot-value material slot)))))
