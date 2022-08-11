(define-test matrix-add-test
    (:lbge.math)
  (testing "2x2 matrix addition"
    (ok (eqg (add (make-float2x2 1 2
                                 3 4)
                  (make-float2x2 5 6
                                 7 8))
             (make-float2x2 6 8
                            10 12))))

  (testing "3x3 matrix addition"
    (ok (eqg (add (make-float3x3 1 2 3
                                 4 5 6
                                 7 8 9)
                  (make-float3x3 10 11 12
                                 13 14 15
                                 16 17 18))
             (make-float3x3 11 13 15
                            17 19 21
                            23 25 27))))

  (testing "4x4 matrix addition"
    (ok (eqg (add (make-float4x4 1 2 3 4
                                 5 6 7 8
                                 9 10 11 12
                                 13 14 15 16)
                  (make-float4x4 17 18 19 20
                                 21 22 23 24
                                 25 26 27 28
                                 29 30 31 32))
             (make-float4x4 18 20 22 24
                            26 28 30 32
                            34 36 38 40
                            42 44 46 48)))))
(define-test matrix-sub-test
    (:lbge.math)
  (testing "2x2 matrix subtraction"
    (ok (eqg (sub (make-float2x2 1 2
                                 3 4)
                  (make-float2x2 5 6
                                 7 8))
             (make-float2x2 -4 -4
                            -4 -4))))

  (testing "3x3 matrix subtraction"
    (ok (eqg (sub (make-float3x3 1 2 3
                                 4 5 6
                                 7 8 9)
                  (make-float3x3 10 11 12
                                 13 14 15
                                 16 17 18))
             (make-float3x3 -9 -9 -9
                            -9 -9 -9
                            -9 -9 -9))))

  (testing "4x4 matrix subtraction"
    (ok (eqg (sub (make-float4x4 1 2 3 4
                                 5 6 7 8
                                 9 10 11 12
                                 13 14 15 16)
                  (make-float4x4 17 18 19 20
                                 21 22 23 24
                                 25 26 27 28
                                 29 30 31 32))
             (make-float4x4 -16 -16 -16 -16
                            -16 -16 -16 -16
                            -16 -16 -16 -16
                            -16 -16 -16 -16)))))


(define-test matrix-mul-test
    (:lbge.math)
  (testing "2x2 matrix multiplication"
    (ok (eqg (mul (make-float2x2 1 2
                                 3 4)
                  2)
             (make-float2x2 2 4
                            6 8))))

  (testing "3x3 matrix multiplication"
    (ok (eqg (mul (make-float3x3 1 2 3
                                 4 5 6
                                 7 8 9)
                  2)
             (make-float3x3 2 4 6
                            8 10 12
                            14 16 18))))

  (testing "4x4 matrix multiplication"
    (ok (eqg (mul (make-float4x4 1 2 3 4
                                 5 6 7 8
                                 9 10 11 12
                                 13 14 15 16)
                  2)
             (make-float4x4 2 4 6 8
                            10 12 14 16
                            18 20 22 24
                            26 28 30 32))))

  (testing "2x2 by 2x2 matrix multiplication"
    (ok (eqg (mul (make-float2x2 5 8
                                 3 8)
                  (make-float2x2 3 8
                                 8 9))
             (make-float2x2 79 112
                            73 96))))

  (testing "3x3 by 3x3 matrix multiplication"
    (ok (eqg (mul (make-float3x3 10 20 10
                                 4 5 6
                                 2 3 5)
                  (make-float3x3 3 2 4
                                 3 3 9
                                 4 4 2))
             (make-float3x3 130 120 240
                            51 47 73
                            35 33 45))))

  (testing "4x4 by 4x4 matrix multiplication"
    (ok (eqg (mul (make-float4x4 5 7 9 10
                                 2 3 3 8
                                 8 10 2 3
                                 3 3 4 8)
                  (make-float4x4 3 10 12 18
                                 12 1 4 9
                                 9 10 12 2
                                 3 12 4 10))
             (make-float4x4 210 267 236 271
                            93 149 104 149
                            171 146 172 268
                            105 169 128 169))))

  (testing "2x2 matrix by 2d vector multiplication"
    (ok (eqg (mul (make-float2x2 1 2
                                 3 4)
                  (make-float2 1 2))
             (make-float2 3 14))))

  (testing "3x3 matrix by 3d vector multiplication"
    (ok (eqg (mul (make-float3x3 1 2 3
                                 4 5 6
                                 7 8 9)
                  (make-float3 1 2 3))
             (make-float3 6 30 72))))

  (testing "4x4 matrix multiplication"
    (ok (eqg (mul (make-float4x4 1 2 3 4
                                 5 6 7 8
                                 9 10 11 12
                                 13 14 15 16)
                  (make-float4 1 2 3 4))
             (make-float4 10 52 126 232)))))


(define-test matrix-div-test
    (:lbge.math)
  (testing "2x2 matrix division"
    (ok (eqg (div (make-float2x2 2 4
                                 6 8)
                  2)
             (make-float2x2 1 2
                            3 4))))

  (testing "3x3 matrix division"
    (ok (eqg (div (make-float3x3 2 4 6
                                 8 10 12
                                 14 16 18)
                  2)
             (make-float3x3 1 2 3
                            4 5 6
                            7 8 9))))

  (testing "4x4 matrix division"
    (ok (eqg (div (make-float4x4 2 4 6 8
                                 10 12 14 16
                                 18 20 22 24
                                 26 28 30 32)
                  2)
             (make-float4x4 1 2 3 4
                            5 6 7 8
                            9 10 11 12
                            13 14 15 16)))))


(define-test absg-test
    (:lbge.math)
  (testing "2x2 matrix abs"
    (ok (eqg (absg (make-float2x2 -1 -2
                                  -3 -4))
             (make-float2x2 1 2
                            3 4))))

  (testing "3x3 matrix abs"
    (ok (eqg (absg (make-float3x3 -1 -2 -3
                                  -4 -5 -6
                                  -7 -8 -9))
             (make-float3x3 1 2 3
                            4 5 6
                            7 8 9))))

  (testing "4x4 matrix abs"
    (ok (eqg (absg (make-float4x4 -1 -2 -3 -4
                                  -5 -6 -7 -8
                                  -9 -10 -11 12
                                  -13 -14 -15 -16))
             (make-float4x4 1 2 3 4
                            5 6 7 8
                            9 10 11 12
                            13 14 15 16)))))


(define-test negg-test
    (:lbge.math)
  (testing "2x2 matrix negation"
    (ok (eqg (negg (make-float2x2 1 2
                                  3 4))
             (make-float2x2 -1 -2
                            -3 -4))))

  (testing "3x3 matrix negation"
    (ok (eqg (negg (make-float3x3 1 2 3
                                  4 5 6
                                  7 8 9))
             (make-float3x3 -1 -2 -3
                            -4 -5 -6
                            -7 -8 -9))))

  (testing "4x4 matrix negation"
    (ok (eqg (negg (make-float4x4 1 2 3 4
                                  5 6 7 8
                                  9 10 11 12
                                  13 14 15 16))
             (make-float4x4 -1 -2 -3 -4
                            -5 -6 -7 -8
                            -9 -10 -11 -12
                            -13 -14 -15 -16)))))


(define-test transpose-test
    (:lbge.math)
  (testing "2x2 matrix transposition"
    (ok (eqg (transpose (make-float2x2 1 2
                                       3 4))
             (make-float2x2 1 3
                            2 4))))

  (testing "3x3 matrix transposition"
    (ok (eqg (transpose (make-float3x3 1 2 3
                                       4 5 6
                                       7 8 9))
             (make-float3x3 1 4 7
                            2 5 8
                            3 6 9))))

  (testing "4x4 matrix transposition"
    (ok (eqg (transpose (make-float4x4 1 2 3 4
                                       5 6 7 8
                                       9 10 11 12
                                       13 14 15 16))
             (make-float4x4 1 5 9 13
                            2 6 10 14
                            3 7 11 15
                            4 8 12 16)))))

(define-test det-test
    (:lbge.math)
  (testing "2x2 matrix determinant"
    (ok (= (det (make-float2x2 1 2
                               2 1))
           -3)))

  (testing "3x3 matrix determinant"
    (ok (= (det (make-float3x3 1 2 3
                               3 2 1
                               2 1 3))
           -12)))

  (testing "4x4 matrix determinant"
    (ok (= (det (make-float4x4 1 2 3 4
                               4 2 1 3
                               3 1 4 2
                               2 4 1 3))
           100))))
