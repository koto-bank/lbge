(in-package :lbge.render.material)

(defclass material ()
  ((uniforms
    :documentation
    "A list of pairs, where car is a keyword, the uniform name, ~
and the cdr is the value to be passed to the shader.~
When cdr is a symbol, it is treated as the shader slot name"
             :initform (list)
             :accessor uniforms
             :initarg :uniforms)
   (textures :documentation "A list of pairs, where car is the texture uniform ~
name inside the shader, and cdr is the texture"
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

(defun add-texture (material name texture)
  "Name is uniform name in the shader.
Texture is the texture object"
  (push (cons name texture)
        (textures material)))

(defun set-uniforms (material)
  "Set all uniform parameters"
  (with-slots (shader uniforms) material
    (loop :for (name . uniform) :in uniforms :do
      (s:set-uniform shader name (if (symbolp uniform)
                                   (slot-value material uniform)
                                   uniform)))))

(defun set-textures (material)
  "Setup all texture uniforms in the shader"
  (with-slots (shader textures) material
    (loop
      :for (name . texture) :in textures
      :for n :from 0
      :do (s:set-texture shader
                         name
                         texture
                         n))))

(defun check-material-consistency (material)
  "Check that material shader has all names needed
 for setting uniforms and textures"
  (every (lambda (location)
           (s:get-uniform [material.shader] (car location)))
         (append [material.textures] [material.uniforms])))
