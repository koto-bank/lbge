(in-package :lbge.render.shader)

(defclass shader () ()
  (:documentation "Protocol class for individual shaders"))

(defgeneric get-status (shader)
  (:documentation "Return shader state:
:unknown - default state
:error - error occured
:loaded - some sources are loaded and not compiled
:ready - program successfully compiled, linked and ready for use"))

(defgeneric add-stage (shader stages)
  (:documentation "Stage is backend pipeline step: fragment, hull,
 tesselation, etc. Specified by alist, e.g. (:fragment frag-src :vertex vert-src"))

(defgeneric compile-shader (shader)
  (:documentation "Compile and link shader"))

(defgeneric get-compile-log (shader)
  (:documentation "Return shader compilation/link log"))

(defgeneric delete-shader (shader))

(defgeneric set-uniform (shader name x &optional y z w))
(defgeneric set-uniform-matrix (shader name mat))
(defgeneric set-texture (shader name texture texture-num))
