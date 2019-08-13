(in-package :lbge-test)


(deftest add-test
  (testing "2x2 matrix addition"
	   (ok (eqm (add (make-float2x2 #(1 2
					  3 4))
			 (make-float2x2 #(5 6
					  7 8)))
		    (make-float2x2 #(6 8
				     10 12)))))

  (testing "3x3 matrix addition"
	   (ok (eqm (add (make-float3x3 #(1 2 3
					  4 5 6
					  7 8 9))
			 (make-float3x3 #(10 11 12
					  13 14 15
					  16 17 18)))
		    (make-float3x3 #(11 13 15
				     17 19 21
				     23 25 27)))))

  (testing "4x4 matrix addition"
	   (ok (eqv (add (make-float4x4 #(1 2 3 4
					  5 6 7 8
					  9 10 11 12
					  13 14 15 16))
			 (make-float4x4 #(17 18 19 20
					  21 22 23 24
					  25 26 27 28
					  29 30 31 32)))
		    (make-float4x4 #(18 20 22 24
				     26 28 30 32
				     34 36 38 40
				     42 44 46 48))))))
(deftest sub-test
  (testing "2x2 matrix subtraction"
	   (ok (eqm (sub (make-float2x2 #(1 2
					  3 4))
			 (make-float2x2 #(5 6
					  7 8)))
		    (make-float2x2 #(-4 -4
				     -4 -4)))))

  (testing "3x3 matrix subtraction"
	   (ok (eqm (add (make-float3x3 #(1 2 3
					  4 5 6
					  7 8 9))
			 (make-float3x3 #(10 11 12
					  13 14 15
					  16 17 18)))
		    (make-float3x3 #(-9 -9 -9
				     -9 -9 -9
				     -9 -9 -9)))))

  (testing "4x4 matrix subtraction"
	   (ok (eqm (add (make-float4x4 #(1 2 3 4
					  5 6 7 8
					  9 10 11 12
					  13 14 15 16))
			 (make-float4x4 #(17 18 19 20
					  21 22 23 24
					  25 26 27 28
					  29 30 31 32)))
		    (make-float4x4 #(-16 -16 -16 -16
				     -16 -16 -16 -16
				     -16 -16 -16 -16
				     -16 -16 -16 -16))))))


(deftest mul-test
  (testing "2x2 matrix multiplication"
	   (ok (eqm (mul (make-float2x2 #(1 2
					  3 4))
			 2)
		    (make-float2x2 #(2 4
				     6 8)))))

  (testing "3x3 matrix multiplication"
	   (ok (eqm (mul (make-float3x3 #(1 2 3
					  4 5 6
					  7 8 9))
			 2)
		    (make-float3x3 #(2 4 6
				     8 10 12
				     14 16 18)))))

  (testing "4x4 matrix multiplication"
	   (ok (eqm (mul (make-float4 #(1 2 3 4
					5 6 7 8
					9 10 11 12
					13 14 15 16))
			 2)
		    (make-float4x4 #(2 4 6 8
				     10 12 14 16
				     18 20 22 24
				     26 28 30 32)))))

  (testing "2x2 by 2x2 matrix multiplication"
	   (ok (eqm (mul (make-float2x2 #(1 2
					  3 4))
			 (make-float2x2 #(5 6
					  7 8)))
		    (make-float2x2 #(19 22
				     43 50)))))
  
  (testing "3x3 by 3x3 matrix multiplication"
	   (ok (eqm (mul (make-float3x3 #(1 2 3
					  4 5 6
					  7 8 9))
			 (make-float3x3 #(10 11 12
					  13 14 15
					  16 17 18)))
		    (make-float3x3 #(84 90 96
				     201 216 231
				     318 342 366)))))
  
  (testing "4x4 by 4x4 matrix multiplication"
	   (ok (eqm (mul (make-float4x4 #(1 2 3 4
					  5 6 7 8
					  9 10 11 12
					  13 14 15 16))
			 (make-float4x4 #(17 18 19 20
					  21 22 23 24
					  25 26 27 28
					  29 30 31 32)))
		    (make-float4x4 #(250 260 270 280
				     618 644 670 696
				     986 1028 1070 1112
				     1354 1412 1470 1528)))))

  (testing "2x2 matrix by 2d vector multiplication"
	   (ok (eqv (mul (make-float2x2 #(1 2
					  3 4))
			 (make-float2 5 6))
		    (make-float2 17 39))))

  (testing "3x3 matrix by 3d vector multiplication"
	   (ok (eqv (mul (make-float3x3 #(1 2 3
					  4 5 6
					  7 8 9))
			 (make-float3 10 11 12))
		    (make-float3 68 167 266))))

  (testing "4x4 matrix multiplication"
	   (ok (eqv (mul (make-float4 #(1 2 3 4
					5 6 7 8
					9 10 11 12
					13 14 15 16))
			 (make-float4 17 18 19 20))
		    (make-float4 190 486 782 1078)))))


(deftest div-test
  (testing "2x2 matrix division"
	   (ok (eqm (div (make-float2x2 #(2 4
					  6 8))
			 2)
		    (make-float2x2 #(1 2
				     3 4)))))

  (testing "3x3 matrix division"
	   (ok (eqm (div (make-float3 #(2 4 6
					8 10 12
					14 16 18))
			 2)
		    (make-float3x3 #(1 2 3
				     4 5 6
				     7 8 9)))))

  (testing "4x4 matrix division"
	   (ok (eqm (div (make-float4x4 #(2 4 6 8
					  10 12 14 16
					  18 20 22 24
					  26 28 30 32))
			 2)
		    (make-float4x4 #(1 2 3 4
				     5 6 7 8
				     9 10 11 12
				     13 14 15 16))))))


(deftest absm-test
  (testing "2x2 matrix abs"
	   (ok (eqm (absm (make-float2x2 #(-1 -2
					   -3 -4)))
		    (make-float2x2 #(1 2
				     3 4)))))

  (testing "3x3 matrix abs"
	   (ok (eqm (absm (make-float3x3 #(-1 -2 -3
					   -4 -5 -6
					   -7 -8 -9)))
		    (make-float3x3 #(1 2 3
				     4 5 6
				     7 8 9)))))

  (testing "4x4 matrix abs"
	   (ok (eqm (absm (make-float4x4 #(-1 -2 -3 -4
					   -5 -6 -7 -8
					   -9 -10 -11 12
					   -13 -14 -15 -16)))
		    (make-float4x4 #(1 2 3 4
				     5 6 7 8
				     9 10 11 12
				     13 14 15 16))))))


(deftest negm-test
  (testing "2x2 matrix negation"
	   (ok (eqm (negm (make-float2x2 #(1 2
					   3 4)))
		    (make-float2x2 #(-1 -2
				     -3 -4)))))

  (testing "3x3 matrix negation"
	   (ok (eqm (negm (make-float3x3 #(1 2 3
					   4 5 6
					   7 8 9)))
		    (make-float3x3 #(-1 -2 -3
				     -4 -5 -6
				     -7 -8 -9)))))

  (testing "4x4 matrix negation"
	   (ok (eqm (negm (make-float4x4 #(1 2 3 4
					   5 6 7 8
					   9 10 11 12
					   13 14 15 16)))
		    (make-float4x4 #(-1 -2 -3 -4
				     -5 -6 -7 -8
				     -9 -10 -11 -12
				     -13 -14 -15 -16))))))


(deftest transpose-test
  (testing "2x2 matrix transposition"
	   (ok (eqm (transpose (make-float2x2 #(1 2
						3 4)))
		    (make-float2 #(1 3
				   2 4)))))

  (testing "3x3 matrix transposition"
	   (ok (eqm (transpose (make-float3x3 #(1 2 3
						4 5 6
						7 8 9)))
		    (make-float3x3 #(1 4 7
				     2 5 8
				     3 6 9)))))

  (testing "4x4 matrix transposition"
	   (ok (eqm (transpose (make-float4x4 #(1 2 3 4
						5 6 7 8
						9 10 11 12
						13 14 15 16)))
		    (make-float4x4 #(1 5 9 13
				     2 6 10 14
				     3 7 11 15
				     4 8 12 16))))))
