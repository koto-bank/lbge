(in-package :cl)

(defpackage :lbge.image
  (:use :cl)
  (:local-nicknames
   (:a :lbge.asset)
   (:ax :alexandria))
  (:export
   ;; Loader
   :load-image

   ;; Image
   :image
   :make-image
   :width
   :height
   :channels
   :data

   ;; Asset
   :image-asset-handler))
