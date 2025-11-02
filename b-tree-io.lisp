;; b-tree-io.lisp
(in-package :b-tree)

(defun encode-pointer (val)
  "Convert null to 0. Because pointer 0 is for header this must be nil"
  (or val 0))

(defun decode-pointer (i4)
  (if (zerop i4) nil i4))

(defmethod read-node ((tree b-tree) addr)
  (with-in-page tree addr
    (let* ((node (make-b-node tree addr))
           (keys-count (page-read-i4)))
      (setf (node-succ-ptr node) (decode-pointer (page-read-i4)))
      (loop :repeat keys-count
            :for key = (make-instance 'b-key)
            :do (with-slots (key record-ptr pred-ptr) key
                  (setf key        (page-read-i4))
                  (setf record-ptr (decode-pointer (page-read-i4)))
                  (setf pred-ptr   (decode-pointer (page-read-i4))))
            :do (vector-push key (node-keys node)))
      node)))

(defmethod write-node ((tree b-tree) (node b-node))
  (with-out-page tree (node-addr node)
    (page-write-i4 (node-keys-count node))
    (page-write-i4 (encode-pointer (node-succ-ptr node)))
    (for-keys ((k i) node)
      (with-slots (key record-ptr pred-ptr) k
        (page-write-i4 key)
        (page-write-i4 (encode-pointer record-ptr))
        (page-write-i4 (encode-pointer pred-ptr)))
      :finally (return i))))
