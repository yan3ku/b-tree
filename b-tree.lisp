;;;; b-tree.lisp
(in-package #:b-tree)

(defclass b-tree ()
  ((root
    :initarg  :root
    :accessor :tree-root
    :documentation "The keys in the node"))
  (:documentation "B-tree root"))

(defclass b-node ()
  ((addr
    :initarg :node-addr
    :accessor node-addr
    :documentation "Address to the page containing this B-tree node")
   (keys
    :initarg :keys
    :accessor node-keys
    :documentation "The keys in the node")
   (succ-ptr
    :type fixnume
    :accessor :node-succ
    :initarg   succ-ptr
    :documentation "Pointer to b-node page containing keys greater than the max key in this node"))
  (:documentation "B-tree node coresponding to one page"))

(defclass b-key ()
  ((key
    :initarg :key
    :accessor b-key
    :type fixnum
    :documentation "The key value")
   (record-ptr
    :initarg :record-ptr
    :type fixnum
    :accessor b-record
    :documentation "Pointer to the page containing the key record")
   (pred-ptr
    :initarg :pred-ptr
    :type fixnum
    :accessor b-pred
    :documentation "Pointer to b-node page with records lesser than key"))
  (:documentation "B-tree key that contains the pointer to value and predecessor records"))


(defmethod read-node ((self b-node))
  ())

(defmethod write-node ((self b-node))
  ())
