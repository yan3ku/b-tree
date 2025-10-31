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

(defmethod make-b-node ((tree b-tree) addr)
  (make-instance 'b-node :addr addr :keys (make-array (tree-order tree) :fill-pointer 0)))

(defmethod make-new-b-node ((tree b-tree))
  (let ((node (make-b-node tree (new-page-addr tree))))
    (mark-dirty tree node)
    node))

(defmethod node-fullp ((tree b-tree) (node b-node))
  (<= (tree-order tree) (node-keys-count node)))

(defmethod node-keys-count ((node b-node))
  (length (node-keys node)))

(defun vector-insert (vector index key)
  (incf (fill-pointer vector))
  (replace vector vector :start1 (1+ index) :start2 index)
  (setf (aref vector index) key)
  vector)

(defun vector-split (to-split into)
  (let ((split (truncate (/ (length to-split) 2))))
    (setf (fill-pointer into) (- (length to-split) split 1))
    (replace into to-split :start2 (1+ split))
    (let ((middle (aref to-split split)))
      (setf (fill-pointer to-split) split)
      (list to-split middle into))))

(defmethod b-node-insert ((tree b-tree) (node b-node) index key)
  (vector-insert (node-keys node) index key)
  (mark-dirty tree node))

(defmethod b-node-split ((tree b-tree) (left-node b-node))
  (let* ((right-node (make-new-b-node tree))
         (split (vector-split (node-keys left-node) (node-keys right-node))))
    (destructuring-bind (left-arr middle right-arr) split
      (setf (node-keys left-node) left-arr)
      (setf (node-keys right-node) right-arr)
      (mark-dirty tree left-node)
      (mark-dirty tree right-node)
      (list left-node middle right-node))))

(defmethod print-object ((node b-node) stream)
  (format stream "~A&(" (node-addr node))
  (let ((first nil))
    (for-keys (key node)
      (when first (format stream " "))
      (setf first t)
      (prin1 (b-key key) stream)))
  (format stream ")"))

(defmethod b-node-find ((tree b-tree) node-addr (to-find b-key))
  (let ((node (read-node tree node-addr)))
    (for-keys ((k i) node)
      (when (key> k to-find)
        (return (make-key-ref node i)))
      :finally (return (make-key-ref node i)))))
