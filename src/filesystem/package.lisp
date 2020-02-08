(defpackage :lbge.filesystem
  (:use :cl)
  (:export
   :get-app-root
   :set-app-root
   :set-app-root-to-system
   :is-exist
   :is-file
   :is-dir
   :parse-asset-path
   :merge-paths
   :path-eq
   :make-path
   :root-relative-path))
