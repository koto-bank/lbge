(in-package :cl)

(defpackage :lbge.image-loader
  (:use :cl)
  (:export :load-image)
  (:import-from :alexandria
                :switch))

(defpackage :lbge.image-loader.image
  (:use :cl)
  (:export :make-image :width :height :channels :data))

(defpackage :lbge.image-loader.tga
  (:use :cl :lbge.image-loader.image)
  (:export :tga))
