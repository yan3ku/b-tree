;;;; b-tree.lisp
(in-package #:b-tree)

(defvar +b-key-size+ (* 4 3) ;; 4-bytes * 3-fields (key, record, pred)
  "Size in bytes of b key on page")

(defclass b-tree ()
  ((root
    :type b-node
    :initarg  :root
    :accessor tree-root
    :documentation "The keys in the node")
   (order
    :type fixnum
    :initarg :order
    :accessor tree-order
    :documentation "The max number of keys in one node (and one page")
   (node-size
    :type fixnum
    :accessor tree-node-size
    :documentation "Node size size on page")
   (pager
    :type pager
    :accessor tree-pager
    :documentation "Pager used to write nodes to disk"))
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
    :initarg :node-addr
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

(defun b-print ()
  )
