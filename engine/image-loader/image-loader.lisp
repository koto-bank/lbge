(in-package :lbge.image-loader)

(defun load-image (path)
  "Takes path to an image and returns `image` structure.
   If the image format is unsupported throws error."
  (switch ((pathname-type path) :test #'string=)
    ("tga" (lbge.image-loader.tga:tga path))))
