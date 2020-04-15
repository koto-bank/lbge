(in-package :lbge.image)

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *loaders* (list)))

(defmacro register-loader (extension loader-fn)
  `(eval-when (:compile-toplevel :load-toplevel)
     (setf *loaders*
           (acons (string-upcase ,extension) ,loader-fn *loaders*))))

(defun load-image (path)
  "Takes path to an image and returns `image` structure.
   If the image format is unsupported throws error."
  (let ((extension (string-upcase (pathname-type path))))
    (assert (assoc extension *loaders* :test #'string=) nil
            "Failed to find loader for file type ~S (file ~S)" extension path)
    (funcall (cdr (assoc extension *loaders* :test #'string=)) path)))

(register-loader "TGA" #'load-tga)
