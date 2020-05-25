(define-test timer-test
    ((:timer :lbge.timer)
     (:beacon :lbge.beacon))
  (let* ((data (list))                  ; place for pushing data
         (ti (timer:make 1000 :started nil)))
    (flet ((push-10 ()
             (push 10 data))
           (push-20 ()
             (push 20 data)))
      (testing "Timer creation"
        (ok (= (timer::timeout ti) 1000))
        (ok (timer::beacon ti))
        (ok (= (timer::passed-time ti) 0))
        (ok (null (timer::startedp ti))))
      (testing "Timer linking"
        (timer:link ti #'push-10)
        (ok (= 1 (length (beacon:links (timer::beacon ti))))))
      (testing "Timer starting and blinking"
        (ok (null data))
        (timer:update ti 1000)
        (ok (null data))
        (ok (= 0 (timer::passed-time ti)))
        (ok (null (timer::startedp ti)))
        (timer:update ti 1000)
        (ok (null data))
        (ok (= 0 (timer::passed-time ti)))
        (ok (null (timer::startedp ti)))
        (timer:start ti)
        (ok (timer::startedp ti))

        (timer:update ti 500)
        (ok (null data))
        (ok (= 500 (timer::passed-time ti)))
        (ok (timer::startedp ti))

        (timer:update ti 500)
        (ok (equal data '(10)))
        (ok (= 0 (timer::passed-time ti)))

        (timer:update ti 1000)
        (ok (equal data '(10 10)))
        (ok (= 0 (timer::passed-time ti))))

      (testing "Timer blink order"
        (timer:link ti #'push-20)
        (ok (equal data '(10 10)))
        (timer:update ti 1000)
        (ok (equal data '(20 10 10 10)))
        (ok (= 0 (timer::passed-time ti)))
        (timer:unlink ti #'push-10)
        (timer:update ti 1000)
        (ok (equal data '(20 20 10 10 10)))
        (ok (= 0 (timer::passed-time ti)))))))
