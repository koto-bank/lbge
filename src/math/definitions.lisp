(in-package :lbge.math)

(defgeneric size (datum)
  (:documentation "Return number of vector or total matrix components"))

(defgeneric width (matrix)
  (:documentation "Return the width of its argument"))

(defgeneric height (matrix)
  (:documentation "Return the height of its argument"))

(defgeneric add (value1 value2)
  (:documentation "Perform addition of value1 to value2 that makes sense for the type"))

(defgeneric sub (value1 value2)
  (:documentation "Perform subtraction of value1 from value2 that makes sense for the type"))

(defgeneric mul (value1 value2)
  (:documentation "Perform multiplication of value1 by value2 that makes sense for the type"))

(defgeneric div (value1 value2)
  (:documentation "Perform division of value1 by value2 that makes sense for the type"))

(defgeneric norm2 (value)
  (:documentation "Get the squared euclidean norm of the value"))

(defgeneric norm (value)
  (:documentation "Get the euclidean norm of the value"))

(defgeneric negg (vector)
  (:documentation "Perform element-wise negation"))

(defgeneric absg (vector)
  (:documentation "Perform element-wise absolute value"))

(defgeneric eqg (value1 value2 &key eps &allow-other-keys)
  (:documentation "Generic equality predicate for values of this math library")
  (:method ((value1 t) (value2 t) &key (eps *epsilon*) &allow-other-keys)
    (declare (ignore value1 value2 eps))
    nil))

(defgeneric neqg (value1 value2 &key eps &allow-other-keys)
  (:documentation "The inverse of #'eqg")
  (:method ((value1 t) (value2 t) &key (eps *epsilon*) &allow-other-keys)
    (not (eqg value1 value2 :eps eps))))

(defgeneric det (matrix)
  (:documentation "Compute the determinant of a matrix"))

(defgeneric transpose (matrix)
  (:documentation "Compute the transpose a matrix"))

(defgeneric conj (quat)
  (:documentation "Get the conjugate quaternion of q"))

(defgeneric inv (quat)
  (:documentation "Get the inverse quaternion of q"))

(defgeneric versor (quat)
  (:documentation "Return the versor (unit quaterion) of q"))

(defgeneric expq (quat)
  (:documentation "Raise e to the power of q"))

(defgeneric logq (quat)
  (:documentation "Get the natural logarithm of q"))

(defgeneric exptq (quat1 quat2)
  (:documentation "Raise q1 to the power of q2"))

(defgeneric lerp (x t1 t2 val1 val2))

(defgeneric degree (poly)
  (:documentation "Get the degree of a polynomial"))

(defgeneric call-at (poly x)
  (:documentation "Evaluate polynomial at point x"))

(defgeneric nadd-point (interp x y)
  (:documentation "Add a point into an existing Newton interpolation polynomial"))

(defgeneric add-point (interp x y)
  (:documentation "Add a point into a Newton interpolation polynomial and return the new copy"))
