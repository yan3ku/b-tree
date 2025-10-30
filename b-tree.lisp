;;;; b-tree.lisp
(in-package #:b-tree)

(defvar +b-key-size+ (* 4 3) ;; 4-bytes * 3-fields (key, record, pred)
  "Size in bytes of b key on page")

(defclass b-tree (pager)
  ((root-addr
    :initform 1
    :reader root-addr
    :allocation :class)
   (root
    :type b-node
    :initarg  :root
    :accessor tree-root
    :documentation "The keys in the node")
   (order
    :initarg :order
    :accessor tree-order
    :documentation "The max number of keys in one node (and one page"))
  (:documentation "B-tree root"))

(defclass b-key ()
  ((key
    :type fixnum
    :initarg :key
    :accessor b-key
    :documentation "The key value")
   (record-ptr
    :type (or fixnum null)
    :initarg :record-ptr
    :accessor b-record-ptr
    :documentation "Pointer to the page containing the key record")
   (pred-ptr
    :initform nil
    :type (or fixnum null)
    :initarg :pred-ptr
    :accessor b-pred-ptr
    :documentation "Pointer to b-node page with records lesser than key"))
  (:documentation "B-tree key that contains the pointer to value and predecessor records"))

(defclass b-node ()
  ((addr
    :type fixnum
    :initarg :addr
    :accessor node-addr
    :documentation "Address to the page containing this B-tree node")
   (keys
    :type (vector b-key)
    :initarg :keys
    :accessor node-keys
    :documentation "The keys in the node")
   (succ-ptr
    :initform nil
    :type (or fixnum null)
    :initarg :succ-ptr
    :accessor node-succ-ptr
    :documentation "Pointer to b-node page containing keys greater than the max key in this node"))
  (:documentation "B-tree node coresponding to one page"))

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
    (page-write-i4 (tree-order tree))))

(defmethod read-header :after ((tree b-tree))
  (with-in-page tree (header-addr tree)
    (page-read-n 8)
    (setf (tree-order tree) (page-read-i4)))
  (setf (tree-root tree) (read-node tree (root-addr tree))))

(defmethod close-b-tree ((tree b-tree) &key delete)
  (write-node tree (tree-root tree))
  (close-pager tree :delete delete))

(defmethod make-b-node ((tree b-tree) page-addr)
  (make-instance 'b-node :addr page-addr :keys (make-array (tree-order tree) :fill-pointer 0)))

(defmethod slot-unbound (class (tree b-tree) (slot-name (eql 'root)))
  (setf (tree-root tree) (make-b-node tree (root-addr tree))))

(defun make-b-tree (name &optional order)
  (let ((tree (make-instance 'b-tree :order order
                                     :index-file  (concatenate 'string name ".i")
                                     :record-file (concatenate 'string name ".r"))))
    tree))
