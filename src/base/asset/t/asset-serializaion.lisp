(defpackage :lbge.test.asset-serialization
  (:use :cl
        :rove
        :lbge.asset)
  (:local-nicknames
   (:fs :lbge.filesystem)
   (:s :lbge.serialization)))

(in-package :lbge.test.asset-serialization)

(define-asset small-asset ()
  ((slot-1 :initarg :slot-1 :serialize t)
   (slot-2 :initarg :slot-2)))

(define-asset-handler small-asset (mgr key)
  (let ((a (make-asset key :slot-2 nil :state :error)))
    (unless (eq (key-type key) :memory)
      (s:deserialize-file a (find-path-by-path-key mgr key)))
    (setf (asset-state a) :loaded)
    a))

(define-asset big-asset ()
  ((small-1 :dep t :initarg :small-1 :type small-asset)
   (small-2 :dep t :initarg :small-2 :type small-asset)
   (other-slot :initarg :other-slot :serialize t)
   (yet-another-slot :initarg :yet-another-slot)))

(define-asset-handler big-asset (mgr key)
  (let ((a (make-asset key :yet-another-slot "default-value"))
        (path (find-path-by-path-key mgr key)))
    (s:deserialize-file a path)
    (setf (asset-state a) :loaded)
    a))

(deftest asset-serialize-test
  (let* ((mgr (make-instance 'asset-manager))
         (small-1 (make-asset
                   (make-asset-key 'small-asset
                                   :memory
                                   "small-asset")
                   :slot-1 12
                   :slot-2 34))
         (small-1-deserialized
           (s:deserialize nil (s:serialize small-1)))
         (small-2-key (make-asset-key 'small-asset
                                      :disk
                                      ":test-dir/small-asset-2.data"))
         (small-2 (make-asset small-2-key
                              :slot-1 56
                              :slot-2 78))
         (big-key (make-asset-key 'big-asset
                                  :disk
                                  ":test-dir/big-asset.data"))
         (big (make-asset
               big-key
               :small-1 small-1
               :small-2 small-2
               :other-slot "other-slot"
               :yet-another-slot "yet-another-slot"))
         (big-deserialized
           (s:deserialize nil (s:serialize big))))
    (add-root mgr :test-dir "base/asset/t")
    (testing "[De]serialization of assets"
      (ok (= (slot-value small-1 'slot-1)
             (slot-value small-1-deserialized 'slot-1)))
      (ng (slot-boundp small-1-deserialized 'slot-2))
      (let ((big-small-1-key (slot-value (slot-value big-deserialized 'small-1)
                                         'key))
            (big-small-2-key (slot-value (slot-value big-deserialized 'small-2)
                                         'key)))
        (ng (null big-small-1-key))
        (ok (eq (slot-value big-small-1-key 'type) :memory))
        (ok (eq (slot-value big-small-1-key 'asset-type) 'small-asset))
        (ok (equal (slot-value big-small-2-key 'asset-type) 'small-asset))
        (ok (equal (slot-value big-small-2-key 'type) :disk))
        (ok (string= (slot-value big-small-2-key 'path)
                     ":test-dir/small-asset-2.data"))
        (ok (string= (slot-value big-deserialized 'other-slot) "other-slot"))
        (ng (slot-boundp big-deserialized 'yet-another-slot)))
      (add-handler mgr (make-instance 'small-asset-handler))
      (add-handler mgr (make-instance 'big-asset-handler))
      (let* ((small-from-disk (get-asset mgr small-2-key))
             (big-from-disk (get-asset mgr big-key))
             (big-small-1 (slot-value big-from-disk 'small-1))
             (big-small-2 (slot-value big-from-disk 'small-2)))
        (ok (eq (slot-value small-from-disk 'slot-1) 56))
        (ok (null (slot-value small-from-disk 'slot-2)))

        (ok (eq :void (asset-state big-small-1)))
        (ok (eq :void (asset-state big-small-2)))

        (load-dependencies mgr big-from-disk)
        (setf big-small-1 (slot-value big-from-disk 'small-1))
        (setf big-small-2 (slot-value big-from-disk 'small-2))
        (ok (eq :loaded (asset-state big-small-1)))
        (ok (eq :loaded (asset-state big-small-2)))))))
