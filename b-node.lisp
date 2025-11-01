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

(defun vector-split-into-lmr (to-split into &optional point)
  (let ((split (or point (truncate (length to-split) 2))))
    (setf (fill-pointer into) (- (length to-split) split 1))
    (let ((mid (aref to-split split)))
      (replace into to-split :start2 (1+ split))
      (setf (fill-pointer to-split) split)
      mid)))

(defmethod b-node-insert ((tree b-tree) (node b-node) index key)
  (vector-insert (node-keys node) index key)
  (mark-dirty tree node))

(defmethod b-node-split ((tree b-tree) (left-node b-node))
  (let* ((right-node (make-new-b-node tree))
         (middle (vector-split-into-lmr (node-keys left-node) (node-keys right-node))))
    (mark-dirty tree left-node)
    (setf (node-succ-ptr right-node) (node-addr left-node))
    ;; (succ right node) <- (succ left node) <- middle <- left-node
    (rotatef (node-succ-ptr right-node) (node-succ-ptr left-node) (b-pred-ptr middle))
    (list left-node middle right-node)))

(defun vector-merge (&rest to-merge)
  (make-array (reduce #'+ (mapcar #'length to-merge))
              :adjustable t
              :fill-pointer t
              :initial-contents (apply #'concatenate 'vector to-merge)))

(defmethod b-node-distribute ((tree b-tree) mid-ref (lt b-node) (rt b-node))
  (let* ((merge (vector-merge (node-keys lt)
                              (list (ref-key mid-ref))
                              (node-keys rt)))
         (split-point (truncate (length merge) 2)))
    ;; we have to avoid loops like this:
    ;; - 3&(30 200)
    ;;   - 1&(-10 -1 0 10 20)
    ;;   - 2&(40 50 60 100)
    ;;   - 4&(300 400 500)
    ;; inserting -20 will redistribute with the same amount of keys in left node....
    (when (= split-point (node-keys-count lt))
      (decf split-point))
    (let ((new-mid (vector-split-into-lmr merge (node-keys rt) split-point)))
      ;; (format t "distributing~%")
      ;; (break)
      ;; left node
      (adjust-array merge (tree-order tree))
      (setf (node-keys lt) merge)
      ;; unset old reference
      (setf (ref-key-ptr  mid-ref) nil)
      (setf (ref-key      mid-ref) new-mid)
      ;; ancestor node
      (setf (ref-key-ptr  mid-ref) (node-addr lt))
      (setf (ref-succ-ptr mid-ref) (node-addr rt))

      (mark-dirty tree lt rt (ref-node mid-ref)))))

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

(defmethod set-root ((tree b-tree) (node b-node))
  (setf (root-addr tree) (node-addr node)))

(defmethod b-node-leafp ((node b-node))
  (null (node-succ-ptr node)))
