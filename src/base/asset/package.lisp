(defpackage :lbge.asset
  (:use :cl :lbge.hash)
  (:local-nicknames (:f :lbge.filesystem)
                    (:u :lbge.utils))
  (:export
   ;; Key
   :asset-key
   :make-asset-key
   :asset-type
   :key-type
   :path
   :options

   :asset
   :asset-data
   :asset-state

   :make-asset-manager
   :asset-manager
   :asset-handler
   :add-handler
   :handler-get-type
   :handler-get-asset
   :find-asset-file-by-path

   :add-root
   :asset-roots
   :get-asset

   :find-asset-by-path

   :sexp-asset-handler
   :glsl-asset-handler))
