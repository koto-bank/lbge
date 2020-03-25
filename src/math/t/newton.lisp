(define-test newton-interpolation
    (:lbge.math)
  (testing "Newton interpolation"
    (ok (eqfp (call (make-newton
                     (make-array '(5)
                      :initial-contents (list
                                         (lbge.math:make-float2 2 0.054514016503653624)
                                         (lbge.math:make-float2 2.75 0.043645242611990376)
                                         (lbge.math:make-float2 3.5 0.042885702986518365)
                                         (lbge.math:make-float2 4.25 0.034908998659194966)
                                         (lbge.math:make-float2 5 0.017578552546796796))))
                    3)
              0.043436986900839025))))

(define-test newton-point-addition
    (:lbge.math)
  (testing "Point addition to a Newton interpolation polynomial"
    (ok (eqfp (call (add-point
                     (add-point (make-newton
                                 (make-array '(3)
                                  :initial-contents (list
                                                     (lbge.math:make-float2 2 0.054514016503653624)
                                                     (lbge.math:make-float2 3.5 0.042885702986518365)
                                                     (lbge.math:make-float2 5 0.017578552546796796))))
                                2.75 0.043645242611990376)
                     4.25 0.034908998659194966)
                    3)
              0.043436986900839025 0.00001))))
