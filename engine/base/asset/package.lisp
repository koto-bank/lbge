(defpackage :lbge.asset
  (:use :cl :lbge.hash :lbge.filesystem)
  (:export
   :asset-key
   :make-asset-key

   :asset
   :asset-data
   :asset-state

   :asset-manager
   :asset-handler
   :add-handler

   :add-root
   :asset-roots
   :get-asset

   :sexp-asset-handler))
