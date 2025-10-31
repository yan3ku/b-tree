;; b-tree-insert.lisp
(in-package :b-tree)

(defmethod split-node ((tree b-tree) (left-node b-node))
  "Split node by moving right half of node elements to new page, the middle and
right node should be linked to parent."
  (destructuring-bind (left-node middle right-node)
      (b-node-split tree left-node)
    (setf (node-succ-ptr right-node) (node-addr left-node))
    ;; (succ right node) <- (succ left node) <- middle <- left-node
    (rotatef (node-succ-ptr right-node) (node-succ-ptr left-node) (b-pred-ptr middle))
    (list left-node middle right-node)))

(defmethod grow-tree ((tree b-tree) middle right-node)
  (let ((new-root (make-new-b-node tree)))
    (set-root tree new-root)
    (b-node-insert tree new-root 0 middle)
    (setf (node-succ-ptr new-root) (node-addr right-node))
    (mark-dirty tree new-root)))

(defun parent-insert (tree parent middle right-node)
  (if (ref-succession-p parent)
      (setf (node-succ-ptr (ref-node parent)) (node-addr right-node))
      (setf (b-pred-ptr (ref-key parent))     (node-addr right-node)))
  (b-node-insert tree (ref-node parent) (ref-index parent) middle))

(defmethod b-tree-insert ((tree b-tree) (key b-key))
  (destructuring-bind (parent found)
      (b-tree-walk tree (root-addr tree) key)
    (when (or (ref-succession-p found) (key/= (ref-key found) key))
      (if (not (node-fullp tree (ref-node found)))
          (b-node-insert tree (ref-node found) (ref-index found) key)
          (destructuring-bind (left-node middle right-node)
              (split-node tree (ref-node found))
            (declare (ignore left-node))
            (if (root-p tree (ref-node found))
                (grow-tree tree middle right-node)
                (parent-insert tree parent middle right-node)))))))
