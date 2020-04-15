(in-package :lbge.image)

(defclass tga-header ()
  ((id-length :accessor id-length)
   (image-type :accessor image-type)
   (color-map-length :accessor color-map-length)
   (color-map-entry-size :accessor color-map-entry-size)
   (x :accessor x)
   (y :accessor y)
   (width :accessor width)
   (height :accessor height)
   (pixel-depth :accessor pixel-depth)))

(defun compressedp (header)
  (let ((type (image-type header)))
    (or (eq :rle-color-mapped type)
        (eq :rle-true-color type)
        (eq :rle-black-and-white type))))

(defun tga-channels (header)
  (ax:switch ((image-type header))
    (0 (error "Image contains no data"))
    (1 (error "Color mapped images not supported"))
    (2 (ax:switch ((pixel-depth header))
         (16 :rg8)
         (24 :rgb8)
         (32 :rgba8)))
    (3 :r8)
    (9 (error "Color mapped images not supported"))
    (10 (ax:switch ((pixel-depth header))
          (16 :rg8)
          (24 :rgb8)
          (32 :rgba8)))
    (11 :r8)))

(defun load-tga (path)
  "Returns the `lbge.image-loader.image::image` object for `path` file."
  (with-open-file (tga path
                       :direction :input
                       :if-does-not-exist :error
                       :element-type '(unsigned-byte 8))
    (let* ((header (read-header tga)))
      (file-position tga (+ 18 ;; header length
                            (id-length header)
                            (* (color-map-length header)
                               (color-map-entry-size header))))
      (make-image :width (width header)
                  :height (height header)
                  :channels (tga-channels header)
                  :data (if (compressedp header)
                          (read-rle tga header)
                          (read-plain tga header))))))

(defun read-plain (tga header)
  (let ((data (make-array (* (width header)
                             (height header)
                             (/ (pixel-depth header) 8))
                          :element-type '(unsigned-byte 8))))
    (read-sequence data tga)
    data))

(defun read-rle (tga header)
  (let* ((bytes-per-pixel (/ (pixel-depth header) 8))
         (pixel-count (* (width header)
                         (height header)))
         (data (make-array (* pixel-count bytes-per-pixel)
                           :element-type '(unsigned-byte 8)))
         (buffer (make-array bytes-per-pixel
                             :element-type '(unsigned-byte 8))))
    (let ((current-pixel 0)
          (current-byte 0)
          (chunk-header 0))
      (loop
        :while (< current-pixel pixel-count)
        :do (progn
              (setf chunk-header (read-byte tga))
              (if (< chunk-header 128)
                ;; Plain chunk
                (progn
                  (incf chunk-header)
                  (dotimes (count chunk-header)
                    (read-sequence buffer tga)
                    (setf (subseq data
                                  current-byte
                                  (+ bytes-per-pixel current-byte))
                          buffer)
                    (incf current-pixel)
                    (incf current-byte bytes-per-pixel)))
                ;; RLE chunk
                (progn
                  (decf chunk-header 127)
                  (read-sequence buffer tga)
                  (dotimes (count chunk-header)
                    (setf (subseq data
                                  current-byte
                                  (+ bytes-per-pixel current-byte))
                          buffer)
                    (incf current-pixel)
                    (incf current-byte bytes-per-pixel)))))))
    data))

(defun read-header (tga)
  (let ((raw-header (make-array '(18)
                                :element-type '(unsigned-byte 8)))
        (header (make-instance 'tga-header)))
    (read-sequence raw-header tga)
    ;;          Field Offset Length    Note
    ;; ID Length           0      1
    (setf (id-length header)
          (aref raw-header 0))
    ;; Color Map type      1      1 ignored
    ;; Image type          2      1
    (setf (image-type header)
          (ax:switch ((aref raw-header 2))
            (0 :no-data)
            (1 :uncompressed-color-mapped)
            (2 :uncompressed-true-color)
            (3 :uncompressed-black-and-white)
            (9 :rle-color-mapped)
            (10 :rle-true-color)
            (11 :rle-black-and-white)))
    ;;          Field Offset Length    Note
    ;; Color map spec      3      5
    (setf (color-map-length header)
          (+ (aref raw-header 5)
             (ash (aref raw-header 6) 8)))
    (setf (color-map-entry-size header)
          (aref raw-header 7))
    ;; Image spec          8     10
    ;; Subfields:
    ;;      .x-origin      8      2
    (setf (x header)
          (+ (aref raw-header 8)
             (ash (aref raw-header 9) 8)))
    ;;      .y-origin     10      2
    (setf (y header)
          (+ (aref raw-header 10)
             (ash (aref raw-header 11) 8)))
    ;;      .width        12      2
    (setf (width header)
          (+ (aref raw-header 12)
             (ash (aref raw-header 13) 8)))
    ;;      .height       14      2
    (setf (height header)
          (+ (aref raw-header 14)
             (ash (aref raw-header 15) 8)))
    ;;      .depth        16       1
    (setf (pixel-depth header) (aref raw-header 16))
    ;;      .descriptor   17       1 ignored
    header))
