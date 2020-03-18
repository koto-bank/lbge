(in-package :lbge.math)

(defmethod lerp (x t1 t2 (val1 real) (val2 real))
  (lerp-num x t1 t2 val1 val2))

(defmethod lerp (x t1 t2 (val1 float4) (val2 float4))
  (make-float4 (lerp-num x t1 t2 (float4-x val1) (float4-x val2))
               (lerp-num x t1 t2 (float4-y val1) (float4-y val2))
               (lerp-num x t1 t2 (float4-z val1) (float4-z val2))
               (lerp-num x t1 t2 (float4-w val1) (float4-w val2))))

(defmethod lerp (x t1 t2 (val1 float2) (val2 float2))
  (make-float2 (lerp-num x t1 t2 (float2-x val1) (float2-x val2))
               (lerp-num x t1 t2 (float2-y val1) (float2-y val2))))

(defun lerp-num (x x1 x2 y1 y2)
  "Get y for x between points (x1, y1) and (x2, y2)"
  (assert (and (>= x x1)
               (<= x x2))
          nil
          "Can't interpolate ~A between ~A and ~A"
          x x1 x2)
  (+ y1 (* (- y2 y1)
           (/ (- x x1)
              (- x2 x1)))))
