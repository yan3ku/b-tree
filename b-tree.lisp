;;;; b-tree.lisp

(in-package #:b-tree)


(defclass b-tree ()
  ((root
    :initarg  :root
    :accessor :tree-root
    :documentation "The keys in the node"))
  (:documentation "B-tree root"))

(defclass b-node ()
  ((keys
    :initarg :keys
    :accessor :node-keys
    :documentation "The keys in the node")
   (succ-ptr
    :type fixnume
    :accessor :node-succ
    :initarg :succ-ptr
    :documentation "Pointer to page containing values greater than the max key"))
  (:documentation "B-tree node coresponding to one page"))

(defclass b-key ()
  ((key
    :initarg :key
    :accessor b-key
    :type fixnum
    :documentation "The key value")
   (val-ptr
    :initarg :val-ptr
    :type fixnum
    :accessor b-val
    :documentation "Pointer to the page containing the key value")
   (pred-ptr
    :initarg :pred-ptr
    :type fixnum
    :accessor b-pred
    :documentation "Pointer to b-node with values lesser than key"))
  (:documentation "B-tree key that contains the pointer to value and predecessor values"))
