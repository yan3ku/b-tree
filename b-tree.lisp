;;;; b-tree.lisp
(in-package #:b-tree)

(defvar +b-key-size+ (* 4 3) ;; 4-bytes * 3-fields (key, record, pred)
  "Size in bytes of b key on page")

(defclass b-tree (pager stat-mixin)
  ((root-addr
    :initform 1
    :accessor root-addr)
   (order
    :initarg :order
    :accessor tree-order
    :documentation "The max number of keys in one node (and one page")
   (dirty
    :initform '()
    :accessor tree-dirty-list
    :documentation "List of changed nodes that should be written to disk."))
  (:documentation "B-tree root"))

(defmethod b-subtree-print (stream (tree b-tree) node-addr &optional (depth 0))
  (let ((node (read-node tree node-addr)))
    (format t "~&~V@T- " (* depth 2))
    (prin1 node)
    (for-keys ((k i) node)
      (when (b-pred-ptr k)
        (b-subtree-print stream tree (b-pred-ptr k) (1+ depth))))
    (when (node-succ-ptr node)
      (b-subtree-print stream tree (node-succ-ptr node) (1+ depth)))))

(defmethod print-object ((tree b-tree) stream)
  (b-subtree-print stream tree (root-addr tree)))

(defmacro with-tree ((var name &key order delete) &body body)
  `(let ((,var (make-b-tree ,name ,order)))
     (unwind-protect
          (progn ,@body)
       (close-b-tree ,var :delete ,delete))))

(defun next-power-of-two (n)
  (expt 2 (ceiling (log n 2))))

(defmethod initialize-instance :before ((tree b-tree) &key order)
  (when order
    (setf (page-size tree) (next-power-of-two
                            (+ (* order +b-key-size+)
                               ;; 2-fields (keys-count, succ-ptr)
                               (* 4 2))))))

(defmethod write-header :after ((tree b-tree))
  (with-out-page tree (header-addr tree)
    (page-write-at 8)
    (page-write-i4 (tree-order tree))
    (page-write-i4 (root-addr tree))))

(defmethod read-header :after ((tree b-tree))
  (with-in-page tree (header-addr tree)
    (page-read-n 8)
    (setf (tree-order tree) (page-read-i4))
    (setf (root-addr tree) (page-read-i4))))

(defmethod close-b-tree ((tree b-tree) &key delete)
  (close-pager tree :delete delete))

(defun make-b-tree (name &optional order)
  (make-instance 'b-tree :order order
                         :index-file  (concatenate 'string name ".i")
                         :record-file (concatenate 'string name ".r")))

(defmethod tree-root ((tree b-tree))
  (read-node tree (root-addr tree)))

(defmethod root-p ((tree b-tree) node)
  (= (node-addr node) (root-addr tree)))
