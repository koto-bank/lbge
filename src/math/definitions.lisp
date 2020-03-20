(in-package :lbge.math)

(defgeneric get-size (vector)
  (:documentation "Return number of vector components"))

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

(defgeneric negv (vector)
  (:documentation "Perform element-wise negation on a vector"))

(defgeneric absv (vector)
  (:documentation "Perform element-wise absolute value on a vector"))

(defgeneric det (matrix)
  (:documentation "Compute the determinant of a matrix"))

(defgeneric transpose (matrix)
  (:documentation "Compute the transpose a matrix"))

(defgeneric absm (matrix)
  (:documentation "Perform element-wise absolute value on a matrix"))

(defgeneric negm (matrix)
  (:documentation "Perform element-wise negation on a matrix"))

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

(defgeneric absq (quat)
  (:documentation "Perform element-wise absolute value on a quarternion"))

(defgeneric negq (quat)
  (:documentation "Perform element-wise negation on a quaternion"))

(defgeneric lerp (x t1 t2 val1 val2))

(defgeneric absp (poly)
  (:documentation "Perform element-wise absolute value on a polynomial"))

(defgeneric negp (poly)
  (:documentation "Perform element-wise negation on a polynomial"))
