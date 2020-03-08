(define-test beacon-test
    ((:beacon :lbge.beacon)
     :lbge.utils)
  (let* ((beacon (beacon:make :test))
         (data (list 1 2 3))
         (callback (lambda (pos val)
                     (setf (nth pos data) val)
                     (format nil "New data: ~S" data))))
    (flet ((mult (a b)
             (println (* a b))))
      (testing "Beacon creation"
        (ok (eq (beacon:name beacon) :test))
        (ok (null (beacon:links beacon))))
      (testing "Beacon linking and unlinking"
        (beacon:link beacon #'mult)
        (ok (= 1 (length (beacon:links beacon))))
        (ok (eq (car (beacon:links beacon)) #'mult))

        (beacon:unlink beacon #'mult)
        (ok (null (beacon:links beacon)))

        (beacon:link beacon #'mult)
        (beacon:link beacon callback)
        (ok (= 2 (length (beacon:links beacon))))
        (ok (eq (first (beacon:links beacon)) callback))
        (ok (eq (second (beacon:links beacon)) #'mult))

        (beacon:unlink beacon #'mult)
        (ok (= 1 (length (beacon:links beacon))))
        (ok (eq (first (beacon:links beacon)) callback))
        (ok (signals (beacon:link beacon callback)))

        (beacon:link beacon #'mult)
        (ok (= 2 (length (beacon:links beacon))))
        (ok (eq (first (beacon:links beacon)) #'mult))
        (ok (eq (second (beacon:links beacon)) callback))

        (beacon:unlink-all beacon)
        (ok (null (beacon:links beacon))))
      (testing "Beacon blinking"
        (beacon:link beacon #'mult)
        (beacon:link beacon callback)
        (ok (equal '(1 2 3) data))
        (beacon:blink beacon 2 34)
        ;; Also '68' should appear in the stdout
        (ok (equal '(1 2 34) data))))))
