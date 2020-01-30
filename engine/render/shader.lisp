(in-package :lbge.render.shader)

(defclass shader () ()
  (:documentation "Protocol class for individual shaders"))

(defgeneric get-status (shader)
  (:documentation "Return shader state:
:unknown - default state
:error - error occured
:loaded - some sources are loaded and not compiled
:compiled - program successfully compiled and linked"))

(defgeneric add-stage (shader stages)
  (:documentation "Stage is backend pipeline step: fragment, hull,
 tesselation, etc. Specified by alist, e.g. (:fragment frag-asset :vertex vert-asset"))

(defgeneric compile (shader)
  (:documentation "Compile and link shader"))

(defgeneric get-errors (shader)
  (:documentation "Return compilation/link error descriptions"))

(defgeneric delete (shader))
