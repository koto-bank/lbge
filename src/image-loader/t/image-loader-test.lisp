(define-test image-loader-test
    ((:i :lbge.image))
  (testing "Loading TGA"
    (let ((tga (i:load-image #P"test-file.tga")))
      ;; TGA
      (ok (eq 'i:image (type-of tga)))
      (ok (= 128 (i:width tga)))
      (ok (= 128 (i:height tga)))))
  (testing "Loading PNG"
    (let ((png (i:load-image #P"test-file.png")))
      ;; PNG
      (ok (eq 'i:image (type-of png)))
      (ok (= 272 (i:width png)))
      (ok (= 170 (i:height png)))
      (ok (eq :rgb8 (i:channels png))))))
