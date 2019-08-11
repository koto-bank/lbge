(in-package :cl)

(defpackage :lbge.image-loader
  (:use :cl)
  (:export :load-image :make-image))

(defpackage :lbge.image-loader.tga
  (:use :cl :lbge.image-loader)
  (:export :tga))
