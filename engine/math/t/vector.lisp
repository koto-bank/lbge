(defpackage :lbge.test.vector
  (:use :cl :lbge.math :rove))

(in-package :lbge.test.vector)

(deftest add-test
  (testing "2d vector addition"
    (ok (eq (add (make-float2 1 2)
		 (make-float2 3 4))
	    (make-float2 4 6))))

  (testing "3d vector addition"
    (ok (eq (add (make-float3 1 2 3)
		 (make-float3 4 5 6))
	    (make-float3 5 7 9))))

  (testing "4d vector addition"
    (ok (eq (add (make-float4 1 2 3 4)
		 (make-float4 5 6 7 8))
	    (make-float4 6 8 10 12)))))


(deftest sub-test
  (testing "2d vector subtraction"
    (ok (eq (sub (make-float2 1 2)
		 (make-float2 3 4))
	    (make-float2 -2 -2))))

  (testing "3d vector subtraction"
    (ok (eq (sub (make-float3 1 2 3)
		 (make-float3 4 5 6))
	    (make-float3 -3 -3 -3))))

  (testing "4d vector subtraction"
    (ok (eq (sub (make-float4 1 2 3 4)
		 (make-float4 5 6 7 8))
	    (make-float4 -4 -4 -4 -4)))))


(deftest mul-test
  (testing "2d vector multiplication"
    (ok (eq (mul (make-float2 1 2) 2)
	    (make-float2 2 3))))

  (testing "3d vector multiplication"
    (ok (eq (mul (make-float3 1 2 3) 2)
	    (make-float3 2 4 6))))

  (testing "4d vector multiplication"
    (ok (eq (mul (make-float4 1 2 3 4) 2)
	    (make-float4 2 4 6 8)))))


(deftest div-test
  (testing "2d vector division"
    (ok (eq (div (make-float2 1 2) 2)
	    (make-float2 (/ 1 2) 1))))

  (testing "3d vector division"
    (ok (eq (div (make-float3 1 2 3) 2)
	    (make-float3 (/ 1 2) 1 (/ 3 2)))))

  (testing "4d vector division"
    (ok (eq (div (make-float4 1 2 3 4) 2)
	    (make-float4 (/ 1 2) 1 (/ 3 2) 2)))))


(deftest dot-test
  (testing "2d dot product"
    (ok (= (dot (make-float2 1 2)
		(make-float2 3 4))
	   11)))

  (testing "3d dot product"
    (ok (= (dot (make-float3 1 2 3)
		(make-float3 4 5 6))
	   32)))

  (testing "4d dot product"
    (ok (= (dot (make-float4 1 2 3 4)
		(make-float4 5 6 7 8))
	   70))))


(deftest cross-test
  (testing "Cross product"
    (ok (eq (cross (make-float3 1 2 3)
		   (make-float3 4 5 6))
	    (make-float3 -3 6 -3)))))


(deftest norm-test
  (testing "2d vector norm"
    (ok (= (norm (make-float2 1 2))
           (sqrt 5))))

  (testing "3d vector norm"
    (ok (= (norm (make-float3 1 2 3))
           (sqrt 14))))

  (testing "4d vector norm"
    (ok (= (norm (make-float4 1 2 3 4))
           (sqrt 30)))))


(deftest angle-test
  (testing "Angle between 2 2d vectors"
    (ok (= (angle (make-float2 1 2)
                  (make-float2 3 4))
           (acos (/ 11 (* 5 (sqrt 5)))))))

  (testing "Angle between 2 3d vectors"
    (ok (= (angle (make-float3 1 2 3)
                  (make-float3 4 5 6))
           (acos (/ (* 16 (sqrt (/ 2 11))) 7 )))))

  (testing "Angle between 2 4d vectors"
    (ok (= (angle (make-float4 1 2 3 4)
                  (make-float4 5 6 7 8))
           (acos (/ (* 7 (sqrt (/ 5 29))) 3))))))


(deftest negate-test
  (testing "2d vector negation"
    (ok (eq (negate (make-float2 1 2))
            (make-float2 -1 -2))))

  (testing "3d vector negation"
    (ok (eq (negate (make-float3 1 2 3))
            (make-float3 -1 -2 -3))))

  (testing "4d vector negation"
    (ok (eq (negate (make-float4 1 2 3 4))
            (make-float4 -1 -2 -3 -4)))))


(deftest project-test
  (testing "2d vector projection"
    (ok (eq (project (make-float2 1 2)
                     (make-float2 3 4))
            (make-float2 (/ 33 25) (/ 44 25)))))

  (testing "3d vector projection"
    (ok (eq (project (make-float3 1 2 3)
                     (make-float3 4 5 6))
            (make-float3 (/ 128 77) (/ 160 77) (/ 192 77)))))

  (testing "4d vector projection"
    (ok (eq (project (make-float4 1 2 3 4)
                     (make-float4 5 6 7 8))
            (make-float4 (/ 175 87) (/ 70 29) (/ 245 87) (/ 280 87))))))
