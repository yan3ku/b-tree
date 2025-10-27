;; b-tree-io.lisp
(in-package :b-tree)

(defun read-node (addr)
  (with-in-page addr
    (let* ((node (make-b-node addr))
           (keys-count (page-read-i4)))
      (setf (node-succ node) (page-read-i4))
      (loop :repeat keys-count
            :for key = (make-instance 'b-key)
            :do (with-slots (key record-ptr pred-ptr) key
                  (setf key        (page-read-i4))
                  (setf record-ptr (page-read-i4))
                  (setf pred-ptr   (page-read-i4)))
            :do (vector-push key (node-keys node))))))

(defmethod write-node ((self b-node))
  (with-out-page (node-addr self)
    (page-write-i4 (length (node-keys self)))
    (page-write-i4 (node-succ self))
    (loop :for key :across (node-keys self)
          :do (with-slots (key record-ptr pred-ptr) key
                (page-write-i4 key)
                (page-write-i4 record-ptr)
                (page-write-i4 pred-ptr)))))

(defun b-print (tree)
  (let ((root-addr (node-addr (tree-root tree))))
    (format t "--- B-TREE PRINT START ---~%")
    (b-print-node root-addr 0)
    (format t "--- B-TREE PRINT END ---~%")))

(defun b-print-node (node-addr depth)
  (let ((node (read-node node-addr)))
    (format t "~V@T[DEPTH ~D] NODE ADDRESS: ~D, KEYS (~D): "
            (* depth 4) depth node-addr (length (node-keys node)))

    (loop for key-obj across (node-keys node) do
      (format t "~D " (b-key key-obj)))
    (format t "~%")

    ;; TODO: print values
    ;; HERE
    ;;

    (loop for key-obj across (node-keys node) do
      (b-print-node (b-pred key-obj) (1+ depth)))
    (b-print-node (node-succ node) (1+ depth))))
