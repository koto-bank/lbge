(in-package :lbge.image)

(defparameter *loaders* (list))

(defmacro register-loader (extension loader-fn)
  `(setf *loaders*
         (acons (string-upcase ,extension) ,loader-fn *loaders*)))

(defun load-image (path)
  "Takes path to an image and returns `image` structure.
   If the image format is unsupported throws error."
  (let ((extension (string-upcase (pathname-type path))))
    (assert (assoc extension *loaders* :test #'string=) nil
            "Failed to find loader for file type ~S (file ~S)" extension path)
    (funcall (cdr (assoc extension *loaders* :test #'string=)) path)))

(register-loader "TGA" #'load-tga)
(register-loader "PNG" #'load-png)
