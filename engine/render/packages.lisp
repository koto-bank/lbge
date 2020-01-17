(defpackage :lbge.render.backend
  (:use :cl)
  (:nicknames :b)
  (:export
   ;; Base backend class and middle API
   :backend
   :init
   :clear
   :render
   :present
   :deinit))
(defpackage :lbge.render
  (:use :cl :lbge.hash)
  (:export
   :renderer
   :make-renderer))

;;; low-level stuff
(defpackage :lbge.render.gl
  (:use :cl :lbge.render)
  (:export
   :make-context
   :compile-effect))
