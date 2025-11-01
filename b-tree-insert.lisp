;; b-tree-insert.lisp
(in-package :b-tree)

(defmethod grow-tree ((tree b-tree) middle right-node)
  (let ((new-root (make-new-b-node tree)))
    (set-root tree new-root)
    (b-node-insert tree new-root 0 middle)
    (setf (node-succ-ptr new-root) (node-addr right-node))))

(defun parent-insert (tree parent middle right-node)
  (setf (ref-key-ptr parent) (node-addr right-node))
  (b-node-insert tree (ref-node parent) (ref-index parent) middle))

(defmethod b-tree-split-node ((tree b-tree) parent to-split)
  (destructuring-bind (left-node middle right-node)
      (b-node-split tree to-split)
    (declare (ignore left-node))
    (if (root-p tree to-split)
        (grow-tree tree middle right-node)
        (parent-insert tree parent middle right-node))))

(defmethod b-tree-insert-rec :around ((tree b-tree) (key b-key) parent node-addr)
  (call-next-method tree key parent (read-node tree node-addr)))

(defmethod b-tree-insert-rec ((tree b-tree) (key b-key) parent node)
  (let ((move-up nil))
    (when (node-fullp tree node)
      (when (and (ref-p parent) (b-node-leafp node))
        (setf move-up (b-tree-compensation tree parent node))
        (when move-up (incf (compensation-count tree))))
      (unless move-up
        (b-tree-split-node tree parent node)
        (incf (split-count tree))
        (setf move-up t)))
    (when move-up
      (write-dirty tree)
      (setf node (if parent
                     (ref-node parent)
                     (read-node tree (root-addr tree))))
      (setf parent nil)))
  (let* ((found (b-node-find tree node key)))
    (if (b-node-leafp node)
        (b-node-insert tree node (ref-index found) key)
        (b-tree-insert-rec tree key found (ref-key-ptr found)))))

(defmethod b-tree-insert ((tree b-tree) (key b-key))
  (b-tree-insert-rec tree key nil (root-addr tree)))
