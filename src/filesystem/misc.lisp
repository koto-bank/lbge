(in-package :lbge.filesystem)

(defvar *app-root* (asdf:system-source-directory 'lbge)
  "Absolute path to current system")

(defun set-app-root (new-root)
  (setf *app-root* new-root))

(defun set-app-root-to-system (system-designator)
  (setf *app-root* (asdf:system-source-directory system-designator)))

(defun get-app-root ()
  ;; TODO: Move to engine initialization, set
  ;; *app-root* from the arg instead!
  (let ((app-root-arg
          (lbge.application:get-arg "--app-root")))
    (if app-root-arg
      (make-pathname :directory app-root-arg)
      *app-root*)))

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
      (log:debug "Asset path: ~S root: ~S path: ~S" asset-path root path)
      (cons (read-from-string root) (parse-namestring path))))

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
