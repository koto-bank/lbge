(in-package :lbge.image-loader.tga)

(defun tga (path)
  (let* ((raw (alexandria:read-file-into-byte-vector))
         (data (parse raw)))
    (make-image :width (make-16bits (aref raw 12) (aref raw 13))
                :height (make-16bits (aref raw 14) (aref raw 15))
                :channels (format nil "rgba~a" (aref raw 16))
                :data (get-data raw (* width height)))))

(defun get-data (raw)
  (let* ((bytes-count (* (make-16bits (aref raw 12) (aref raw 13)) ; Width pixels
                         (make-16bits (aref raw 14) (aref raw 15)) ; Height pixels
                         (aref raw 16))) ; Bytes per pixel
         (screen-destination (logand (aref raw 17) #0b00110000))
         (data-start-pos (get-data-start-pos raw))
         (data (if (> 3 (aref raw 2)) ; RLE check
                   (decode-rle (subseq raw raw data-start-pos) bytes-count)
                   (subseq raw data-start-pos (+ data-start-pos bytes-count)))))
    (cond ;; TODO
      ((= #0b00 screen-destination))
      ((= #0b01 screen-destination))
      ((= #0b10 screen-destination) data)
      ((= #0b11 screen-destination)))))

(defun decode-rle (raw bytes-count &optional decoded)
  (let* ((first-byte (aref raw 1))
         (rle? (logandg first-byte #0b10000000))
         (count (logand first-byte #0b11111110)))
    (if (= decoded bytes-count)
        decoded
        (decode-rle (if rle?
                     (subseq raw 3)
                     (subseq raw (1+ count)))
                    bytes-count
                    (if rle?
                        (reduce #'cons decoded (make-list count :initial-element (aref raw 2)))
                        (reduce #'cons
                                decoded
                                :initial-value (subseq raw 1 count)
                                :from-end t))))))

(defun get-data-start-pos (raw)
  (+ 17 ; Header last byte position
     (aref raw 0) ; Image ID field's size
     (get-color-map-size raw)))

(defun get-color-map-size (raw)
  (if (= (aref raw 2) 0)
      0
      (* (make-16bits (aref raw 6) (aref raw 7)) ; Count of entries
         (aref raw 8)))) ; Size of entry

(defun make-16bits (f-byte s-byte)
  "Merge two bytes to one 2 byte value."
  (+ (ash f-byte 8) s-byte))
