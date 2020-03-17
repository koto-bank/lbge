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

(defgeneric norm (valie)
  (:documentation "Get the euclidean norm of the value")))

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

(defgeneric conj (q)
  (:documentation "Get the conjugate quaternion of q"))

(defgeneric inv (q)
  (:documentation "Get the inverse quaternion of q"))

(defgeneric versor (q)
  (:documentation "Return the versor (unit quaterion) of q"))

(defgeneric expq (q)
  (:documentation "Raise e to the power of q"))

(defgeneric logq (q)
  (:documentation "Get the natural logarithm of q"))

(defgeneric exptq (q1 q2)
  (:documentation "Raise q1 to the power of q2"))

(defgeneric absq (quat)
  (:documentation "Perform element-wise absolute value on a quarternion"))

(defgeneric negq (quat)
  (:documentation "Perform element-wise negation on a quaternion"))
