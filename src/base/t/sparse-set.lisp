(define-test sparse-set-base
    ((:sp-s :lbge.sparse-set)
     :lbge.utils)
  (let* ((sparse (make-array '(0) :adjustable t :element-type '(unsigned-byte 64)))
         (packed (make-array '(0) :adjustable t :fill-pointer 0 :element-type '(unsigned-byte 64)))
         (data (make-array '(0) :adjustable t :fill-pointer 0 :element-type 'simple-string)))
    (testing "Add, remove, presence of elements"
      (ok (null (sp-s:existsp 1 sparse packed)))
      (sp-s:insert 1 sparse packed)
      (ok (sp-s:existsp 1 sparse packed))
      (ok (null (sp-s:existsp 2 sparse packed))))

    (testing "Deletion, readdition and inner structures"
      (sp-s:insert 4 sparse packed)
      (sp-s:insert 6 sparse packed)
      (sp-s:insert 5 sparse packed)
      (ok (equalp sparse #(0 0 0 0 1 3 2)))
      (ok (equalp packed #(1 4 6 5)))

      (sp-s:remove 4 sparse packed)
      (sp-s:remove 5 sparse packed)
      (sp-s:remove 666 sparse packed)
      (sp-s:insert 4 sparse packed)
      (ok (= (aref sparse 1) 0))
      (ok (= (aref sparse 6) 1))
      (ok (= (aref sparse 4) 2))
      (ok (equalp packed #(1 6 4))))

    (testing "Data addition"
      (sp-s:clear sparse packed)
      (ok (equalp sparse #()))
      (ok (equalp packed #()))

      (sp-s:insert 4 sparse packed "uh" data)
      (sp-s:insert 1 sparse packed "oh" data)
      (sp-s:insert 3 sparse packed "ez" data)
      (ok (equalp sparse #(0 1 0 2 0)))
      (ok (equalp packed #(4 1 3)))
      (ok (equalp data #("uh" "oh" "ez")))
      (ok (string= (sp-s:get 3 sparse packed data) "ez"))
      (ok (string= (sp-s:get 1 sparse packed data) "oh"))
      (ok (string= (sp-s:get 4 sparse packed data) "uh"))
      (ok (null (sp-s:get 2 sparse packed data)))
      (ok (null (sp-s:get 666 sparse packed data)))

      (sp-s:remove 4 sparse packed data)
      (ok (equalp sparse #(0 1 0 0 0)))
      (ok (equalp packed #(3 1)))
      (ok (equalp data #("ez" "oh")))
      (ok (string= (sp-s:get 3 sparse packed data) "ez"))
      (ok (string= (sp-s:get 1 sparse packed data) "oh"))
      (ok (null (sp-s:get 4 sparse packed data))))

    (testing "Set vectors expansion"
      (sp-s:clear sparse packed data)
      (ok (= 0 (array-total-size sparse)))
      (ok (= 0 (array-total-size packed)))
      (ok (= 0 (array-total-size data)))

      (sp-s:insert 1 sparse packed)
      (ok (= 2 (array-total-size sparse)))
      (ok (= 8 (array-total-size packed)))

      (sp-s:insert 2 sparse packed)
      (sp-s:insert 3 sparse packed)
      (sp-s:insert 4 sparse packed)
      (sp-s:insert 5 sparse packed)
      (sp-s:insert 6 sparse packed)
      (sp-s:insert 7 sparse packed)
      (sp-s:insert 8 sparse packed)
      (ok (= 9 (array-total-size sparse)))
      (ok (= 8 (array-total-size packed)))

      (sp-s:insert 666 sparse packed)
      (ok (= 667 (array-total-size sparse)))
      (ok (= 16 (array-total-size packed))))))
