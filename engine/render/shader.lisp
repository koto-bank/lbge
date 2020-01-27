(in-package :lbge.render.shader)

(defclass shader () ()
  (:documentation "Protocol class for individual shaders"))

(defgeneric get-status (shader)
  (:documentation "Return shader state: :error, :compiled, :unknown"))

(defgeneric add-stage (shader stages)
  (:documentation "Stage is backend pipeline step: fragment, hull,
 tesselation, etc. Specified by alist, e.g. (:fragment frag-asset :vertex vert-asset"))
(defgeneric compile (shader))
(defgeneric link (shader))
(defgeneric get-errors (shader)
  (:documentation "Return compilation/link error descriptions"))

(defgeneric delete (shader))
