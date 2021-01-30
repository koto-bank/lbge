(in-package :lbge.render.material)

(defclass material ()
  ((uniforms :documentation "A list of keywords denoting available ~
uniforms in the shader"
             :initform (list)
             :accessor uniforms
             :initarg :uniforms)
   (textures :documentation "A list of pairs, where car is the texture uniform ~
name inside the shader, and cdr is the texture handle"
             :accessor textures
             :initform (list)
             :initarg :textures)
   (hooks :documentation "Material hooks code source"
          :initarg :hooks)
   (shader :documentation "Material shader"
           :accessor shader))
  (:documentation "Graphics material"))

(defun add-uniform (material name slot-name)
  "Name is name uniform name in the shader.
Slot-name is the name of corresponding slot in the material"
  (push (cons name slot-name)
        (uniforms material)))

(defun add-texture (material name slot-name)
  "Name is uniform name in the shader.
Slot-name is the name of texture in the material"
  (push (cons name slot-name)
        (textures material)))

(defun set-uniforms (material)
  "Set all uniform parameters"
  (with-slots (shader uniforms) material
    (loop :for (name . slot) :in uniforms :do
      (s:set-uniform shader name (slot-value material slot)))))

(defun set-textures (material)
  "Setup all texture uniforms in the shader"
  (with-slots (shader textures) material
    (loop
      :for (name . slot) :in textures
      :for n :from 0
      :do (s:set-texture shader
                         name
                         (slot-value material slot)
                         n))))

(defun check-material-consistency (material)
  "Check that material shader has all names needed
 for setting uniforms and textures"
  (every (lambda (location)
           (s:get-uniform [material.shader] (car location)))
         (append [material.textures] [material.uniforms])))
