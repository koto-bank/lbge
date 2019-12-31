(in-package :lbge.filesystem)

(defun get-app-root ()
  (let ((app-root-arg
          (lbge.application:get-arg "--app-root")))
    (if (null app-root-arg)
      (asdf:system-source-directory 'lbge)
      (make-pathname :directory app-root-arg))))

(defun is-exist (path)
  (or (is-file path)
      (is-dir path)))

(defun is-file (path)
  "Check if file/dir exists"
  (uiop:file-exists-p path))

(defun is-dir (path)
  "Check if path exists and is directory"
  (uiop:directory-exists-p path))

;;; Asset path defined by root and relative path,
;;; e.g. ":textures/sprites/whatever"
(defun parse-asset-path (asset-path)
  "Parse asset path and return pair (:root-name . #P\"rel-path\")"
  (cl-ppcre:register-groups-bind (root path)
      ("^(.*?)/(.*)$" asset-path)
    (cons (read-from-string root) (uiop:make-pathname* :name path))))

(defun merge-paths (path-1 path-2)
  (uiop:merge-pathnames* path-2 (uiop:ensure-directory-pathname path-1)))

(defun path-eq (path-1 path-2)
  (uiop:pathname-equal
   (uiop:truenamize path-1)
   (uiop:truenamize path-2)))

(defun make-path (&key name dir)
  (uiop:make-pathname* :name name :directory dir))

(defun root-relative-path (path)
  (merge-paths (get-app-root) path))
