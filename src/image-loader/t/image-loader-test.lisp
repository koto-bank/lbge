(define-test image-loader-test
    ((:i :lbge.image))
  (testing "Loading TGA"
    (let ((tga (i:load-image #P"test-file.tga")))
      ;; TGA
      (ok (eq 'i:image (type-of tga)))
      (ok (= 128 (i:width tga)))
      (ok (= 128 (i:height tga)))))
  (testing "Loading PNG"
    ;; PNG
    (ok (signals (i:load-image #P"test-file.png")))))
