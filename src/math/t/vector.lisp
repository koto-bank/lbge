(define-test vector-add-test
    (:lbge.math)
  (testing "2d vector addition"
    (ok (eqv (add (make-float2 1 2)
                  (make-float2 3 4))
             (make-float2 4 6))))

  (testing "3d vector addition"
    (ok (eqv (add (make-float3 1 2 3)
                  (make-float3 4 5 6))
             (make-float3 5 7 9))))

  (testing "4d vector addition"
    (ok (eqv (add (make-float4 1 2 3 4)
                  (make-float4 5 6 7 8))
             (make-float4 6 8 10 12)))))


(define-test vector-sub-test
    (:lbge.math)
  (testing "2d vector subtraction"
    (ok (eqv (sub (make-float2 1 2)
                  (make-float2 3 4))
             (make-float2 -2 -2))))

  (testing "3d vector subtraction"
    (ok (eqv (sub (make-float3 1 2 3)
                  (make-float3 4 5 6))
             (make-float3 -3 -3 -3))))

  (testing "4d vector subtraction"
    (ok (eqv (sub (make-float4 1 2 3 4)
                  (make-float4 5 6 7 8))
             (make-float4 -4 -4 -4 -4)))))


(define-test vector-mul-test
    (:lbge.math)
  (testing "2d vector multiplication"
    (ok (eqv (mul (make-float2 1 2) 2)
             (make-float2 2 4))))

  (testing "3d vector multiplication"
    (ok (eqv (mul (make-float3 1 2 3) 2)
             (make-float3 2 4 6))))

  (testing "4d vector multiplication"
    (ok (eqv (mul (make-float4 1 2 3 4) 2)
             (make-float4 2 4 6 8))))

  (testing "2d by 2d vector multiplication"
    (ok (eqv (mul (make-float2 1 2)
                  (make-float2 3 4))
             (make-float2 3 8))))

  (testing "3d by 3d vector multiplication"
    (ok (eqv (mul (make-float3 1 2 3)
                  (make-float3 4 5 6))
             (make-float3 4 10 18))))

  (testing "4d by 4d vector multiplication"
    (ok (eqv (mul (make-float4 1 2 3 4)
                  (make-float4 5 6 7 8))
             (make-float4 5 12 21 32)))))


(define-test vector-div-test
    (:lbge.math)
  (testing "2d vector division"
    (ok (eqv (div (make-float2 2 4)
                  2)
             (make-float2 1 2))))

  (testing "3d vector division"
    (ok (eqv (div (make-float3 2 4 6)
                  2)
             (make-float3 1 2 3))))

  (testing "4d vector division"
    (ok (eqv (div (make-float4 2 4 6 8)
                  2)
             (make-float4 1 2 3 4))))

  (testing "2d by 2d vector division"
    (ok (eqv (div (make-float2 3 8)
                  (make-float2 3 4))
             (make-float2 1 2))))

  (testing "3d by 3d vector division"
    (ok (eqv (div (make-float3 4 10 18)
                  (make-float3 4 5 6))
             (make-float3 1 2 3))))

  (testing "4d by 4d vector division"
    (ok (eqv (div (make-float4 5 12 21 32)
                  (make-float4 5 6 7 8))
             (make-float4 1 2 3 4)))))


(define-test dot-test
    (:lbge.math)
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


(define-test cross-test
    (:lbge.math)
  (testing "Cross product"
    (ok (eqv (cross (make-float3 1 2 3)
                    (make-float3 4 5 6))
             (make-float3 -3 6 -3)))))


(define-test vector-norm-test
    (:lbge.math)
  (testing "2d vector norm"
    (ok (eqfp (norm (make-float2 1 2))
           (sqrt 5))))

  (testing "3d vector norm"
    (ok (eqfp (norm (make-float3 1 2 3))
           (sqrt 14))))

  (testing "4d vector norm"
    (ok (eqfp (norm (make-float4 1 2 3 4))
           (sqrt 30)))))


(define-test angle-test
    (:lbge.math)
  (testing "Angle between 2 2d vectors"
    (ok (eqfp (angle (make-float2 1 2)
                  (make-float2 2 1))
           (acos 4/5))))

  (testing "Angle between 2 3d vectors"
    (ok (eqfp (angle (make-float3 1 2 3)
                  (make-float3 3 2 1))
           (acos 5/7))))

  (testing "Angle between 2 4d vectors"
    (ok (eqfp (angle (make-float4 1 2 3 4)
                  (make-float4 4 3 2 1))
           (acos 2/3)))))


(define-test negv-test
    (:lbge.math)
  (testing "2d vector negation"
    (ok (eqv (negv (make-float2 1 2))
             (make-float2 -1 -2))))

  (testing "3d vector negation"
    (ok (eqv (negv (make-float3 1 2 3))
             (make-float3 -1 -2 -3))))

  (testing "4d vector negation"
    (ok (eqv (negv (make-float4 1 2 3 4))
             (make-float4 -1 -2 -3 -4)))))


(define-test absv-test
    (:lbge.math)
  (testing "2d vector abs"
    (ok (eqv (absv (make-float2 -1 -2))
             (make-float2 1 2))))

  (testing "3d vector abs"
    (ok (eqv (absv (make-float3 -1 -2 -3))
             (make-float3 1 2 3))))

  (testing "4d vector abs"
    (ok (eqv (absv (make-float4 -1 -2 -3 -4))
             (make-float4 1 2 3 4)))))


(define-test vector-project-test
    (:lbge.math)
  (testing "2d vector projection"
    (ok (eqv (project (make-float2 1 2)
                      (make-float2 3 4))
             (make-float2 (float 33/25) (float 44/25)))))

  (testing "3d vector projection"
    (ok (eqv (project (make-float3 1 2 3)
                      (make-float3 4 5 6))
             (make-float3 (float 128/77) (float 160/77) (float 192/77)))))

  (testing "4d vector projection"
    (ok (eqv (project (make-float4 1 2 3 4)
                      (make-float4 5 6 7 8))
             (make-float4 (float 175/87) (float 70/29) (float 245/87) (float 280/87))))))


(define-test vector-normalize-test
    (:lbge.math)
  (testing "0 vector normalization"
    (ok (eqv (normalize (float2-zero))
             (float2-zero))))

  (testing "2d vector normalization"
    (ok (eqv (normalize (make-float2 3 4))
             (make-float2 3/5 4/5))))

  (testing "3d vector normalization"
    (ok (eqv (normalize (make-float3 2 3 6))
             (make-float3 2/7 3/7 6/7))))

  (testing "4d vector normalization"
    (ok (eqv (normalize (make-float4 1 1 1 1))
             (make-float4 1/2 1/2 1/2 1/2)))))
