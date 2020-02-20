;;; Special files for adding functionality missing
;;; from dependencies. Pull-request merges and auiklisp
;;; updates have significant lag, so this file should
;;; contain everything needed while changes are en route
;;; to the repos and the quicklisp.

(in-package :cl-opengl)

(definline draw-elements-base-vertex (mode array base-vertex &key
                                           (count (gl-array-size array))
                                           (offset 0))
  (%gl:draw-elements-base-vertex mode count
                                 (cffi-type-to-gl (gl-array-type array))
                                 (gl-array-pointer-offset array offset)
                                 base-vertex))

(definline draw-elements-instanced-base-vertex
    (mode array primcount base-vertex
          &key
          (count (gl-array-size array))
          (offset 0))
  (%gl:draw-elements-instanced-base-vertex mode count
                                           (cffi-type-to-gl (gl-array-type array))
                                           (gl-array-pointer-offset array offset)
                                           primcount
                                           base-vertex))

(export 'draw-elements-instanced-base-vertex)
(export 'draw-elements-base-vertex)
