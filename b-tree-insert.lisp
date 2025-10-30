;; b-tree-insert.lisp
(in-package :b-tree)

(defmethod b-tree-walk ((tree b-tree) node-addr (to-find b-key))
  "Walk until the node for insertion of key is found."
  (let ((node (read-node tree node-addr)))
    (for-keys ((k i) node)
      (when (> (b-key k) (b-key to-find))
        (if (b-pred-ptr k)
            (return (b-tree-walk tree (b-pred-ptr k) to-find))
            (return (list node i k))))
      :finally (return (list node (node-keys-count node) nil)))))

(defun vector-insert (vector index key)
  (incf (fill-pointer vector))
  (replace vector vector :start1 (1+ index) :start2 index)
  (setf (aref vector index) key)
  vector)

(defmethod b-node-insert ((node b-node) index key)
  (vector-insert (node-keys node) index key))

(defmethod split-node ((tree b-tree) (to-split b-node))
  (let* ((split (truncate (/ (node-keys-count to-split) 2)))
         (left-node (make-b-node tree 2)))
    (setf (fill-pointer (node-keys left-node)) (- split (node-keys-count left-node)))
    (replace (node-keys left-node) (node-keys to-split) :start2 (1+ split))
    (let ((middle (aref (node-keys to-split) split)))
      (setf (fill-pointer (node-keys to-split)) split)
      (list to-split middle left-node))))

(defmethod b-tree-insert ((tree b-tree) (key b-key))
  (destructuring-bind (node index found)
      (b-tree-walk tree (root-addr tree) key)
    (when (or (not found) (/= (b-key found) (b-key key)))
      (if (> (tree-order tree) (node-keys-count node))
          (progn
            (b-node-insert node index key)
            (write-node tree node)
            (setf (tree-root tree) (read-node tree (root-addr tree))))
          (progn
            )))))
