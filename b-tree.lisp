;;;; b-tree.lisp
(in-package #:b-tree)

(defvar +b-key-size+ (* 4 3) ;; 4-bytes * 3-fields (key, record, pred)
  "Size in bytes of b key on page")

(defclass b-tree (pager)
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

(defmethod tree-root ((tree b-tree))
  (read-node tree (root-addr tree)))

;; (defmethod slot-unbound (class (tree b-tree) (slot-name (eql 'root)))
;;   (setf (tree-root tree) (make-b-node tree (root-addr tree))))

(defun make-b-tree (name &optional order)
  (let ((tree (make-instance 'b-tree :order order
                                     :index-file  (concatenate 'string name ".i")
                                     :record-file (concatenate 'string name ".r"))))
    tree))

(defmethod set-root ((tree b-tree) (node b-node))
  (setf (root-addr tree) (node-addr node)))


(defmethod root-p ((tree b-tree) node)
  (= (node-addr node) (root-addr tree)))
