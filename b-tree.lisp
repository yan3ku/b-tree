;;;; b-tree.lisp
(in-package #:b-tree)

(defvar +b-key-size+ (* 4 3) ;; 4-bytes * 3-fields (key, record, pred)
  "Size in bytes of b key on page")

(defclass b-tree (pager)
  ((root
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
    :type fixnum
    :initarg :record-ptr
    :accessor b-record
    :documentation "Pointer to the page containing the key record")
   (pred-ptr
    :type fixnum
    :initarg :pred-ptr
    :accessor b-pred
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
    :type fixnum
    :initarg :succ-ptr
    :accessor node-succ
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
  (with-out-page tree 0
    (page-write-at 8)
    (page-write-i4 (tree-order tree))))

(defmethod read-header :after ((tree b-tree))
  (with-in-page tree 0
    (page-read-n 8)
    (setf (tree-order tree) (page-read-i4))))

(defmethod close-b-tree ((tree b-tree))
  (close-pager tree))

(defmethod make-b-node ((tree b-tree) addr)
  (make-instance 'b-node :addr addr :keys (make-array tree :fill-pointer 0)))

(defun make-b-tree (&optional order)
  (let ((tree (make-instance 'b-tree :order order)))
    (setf (tree-root tree) (make-b-node tree 1))))
