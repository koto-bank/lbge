(defpackage :lbge.asset
  (:use :cl :lbge.hash)
  (:local-nicknames (:f :lbge.filesystem)
                    (:u :lbge.utils))
  (:export
   ;; Key
   :asset-key
   :make-asset-key
   :key-type
   :path
   :options
   :find-path-by-path-key

   :asset
   :type
   :asset-type
   :define-asset
   :asset-state

   :make-asset-manager
   :asset-manager
   :define-asset-handler
   :asset-handler
   :add-handler
   :handler-get-type
   :handler-get-asset
   :find-asset-file-by-path

   :add-root
   :asset-roots
   :get-asset

   :sexp-asset-handler
   :sexp
   :sexp-data
   :shader-source
   :shader-source-asset-handler
   :source

   :image-asset
   :image-asset-asset-handler))
