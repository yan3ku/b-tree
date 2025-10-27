;; b-tree-io.lisp
(in-package :b-tree)

(defun read-node (addr)
  (let* ((node (make-instance 'b-node))
         (page (read-page addr))
         (keys-count (seq-read-i4 page)))
    (setf (node-succ node) (seq-read-i4 page))
    (loop :with keys = (make-array keys-count :fill-pointer 0)
          :repeat keys-count
          :for key = (make-instance 'b-key)
          :do (with-slots (key record-ptr pred-ptr) key
                (setf key        (seq-read-i4 page))
                (setf record-ptr (seq-read-i4 page))
                (setf pred-ptr   (seq-read-i4 page)))
          :do (vector-push key keys))
    (setf (node-addr node) addr)
    (write-page addr page)))


(defmethod write-node ((self b-node))
  (let ((page (make-page-buf)))
    (seq-write-i4 (length (node-keys self)) page)
    (seq-write-i4 (node-succ self) page)
    (loop :for key :across (node-keys self)
          :do (with-slots (key record-ptr pred-ptr) key
                (seq-write-i4 key        page)
                (seq-write-i4 record-ptr page)
                (seq-write-i4 pred-ptr   page)))
    (write-page (node-addr self) page)))

(defmethod initialize-instance :after ((self b-tree) &key)
  (with-slots (node-size pager) self
    (setf node-size (+ (* (tree-order self) +b-key-size+)
                       ;; 2-fields (keys-length, succ)
                       (* 4 2)))
    (setf pager (make-instance 'pager :page-size (tree-size self)))))

(defmethod rem-tree ((self b-tree))
  (close-pager))

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
