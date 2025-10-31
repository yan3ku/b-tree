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
    (setf (node-succ-ptr new-root) (node-addr right-node))))

(defun parent-insert (tree parent middle right-node)
  (if (ref-succession-p parent)
      (setf (node-succ-ptr (ref-node parent)) (node-addr right-node))
      (setf (b-pred-ptr (ref-key parent))     (node-addr right-node)))
  (b-node-insert tree (ref-node parent) (ref-index parent) middle))

(defmethod b-tree-split-node (tree parent to-split)
  (destructuring-bind (left-node middle right-node)
      (split-node tree to-split)
    (declare (ignore left-node))
    (if (root-p tree to-split)
        (grow-tree tree middle right-node)
        (parent-insert tree parent middle right-node))))

(defmethod b-tree-insert-rec :around ((tree b-tree) (key b-key) parent node-addr)
  (call-next-method tree key parent (read-node tree node-addr)))

(defmethod b-tree-insert-rec ((tree b-tree) (key b-key) parent node)
  (when (node-fullp tree node)
    (b-tree-split-node tree parent node)
    (write-dirty tree)
    (if parent
        (setf node (ref-node parent))
        (setf node (read-node tree (root-addr tree))))
    (setf parent nil))
  (let* ((found (b-node-find tree node key))
         (found-ref (make-key-ref node found)))
    (if (b-node-leafp node)
        (b-node-insert tree node found key)
        (b-tree-insert-rec tree key found-ref (ref-key-ptr found-ref)))))

(defmethod b-tree-insert ((tree b-tree) (key b-key))
  (b-tree-insert-rec tree key nil (root-addr tree)))
