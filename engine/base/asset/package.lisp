(defpackage :lbge.asset
  (:use :cl :lbge.hash)
  (:local-nicknames (:f :lbge.filesystem))
  (:export
   :asset-key
   :make-asset-key

   :asset
   :asset-data
   :asset-state

   :make-asset-manager
   :asset-manager
   :asset-handler
   :add-handler

   :add-root
   :asset-roots
   :get-asset

   :sexp-asset-handler
   :glsl-asset-handler))
