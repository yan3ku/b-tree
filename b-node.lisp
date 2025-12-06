;;; b-node
(in-package :b-tree)

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

(defmethod print-object ((node b-node) stream)
  (format stream "~A&(" (node-addr node))
  (let ((first nil))
    (for-keys (key node)
      (when first (format stream " "))
      (setf first t)
      (prin1 (b-key key) stream)))
  (format stream ")"))

(defmethod node-pred-ptr ((node b-node))
  (b-pred-ptr (aref (node-keys node) 0)))

(defmethod make-b-node ((tree b-tree) addr)
  (make-instance 'b-node :addr addr :keys (make-array (tree-order tree) :fill-pointer 0)))

(defmethod make-new-b-node ((tree b-tree))
  (let ((node (make-b-node tree (new-page-addr tree))))
    (mark-dirty tree node)
    node))

(defmethod node-fullp ((tree b-tree) (node b-node))
  (<= (tree-order tree) (node-keys-count node)))

(defmethod node-2-left-empty-p ((tree b-tree) (node b-node))
  (>= 2 (- (tree-order tree) (node-keys-count node))))

(defmethod node-keys-count ((node b-node))
  (length (node-keys node)))

(defmethod b-node-leafp ((node b-node))
  (null (node-succ-ptr node)))

(defmethod b-node-internalp ((node b-node))
  (not (b-node-leafp node)))

(defmethod set-root ((tree b-tree) (node b-node))
  (setf (root-addr tree) (node-addr node)))
