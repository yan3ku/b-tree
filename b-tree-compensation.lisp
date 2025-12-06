;;; b-tree-aux.lisp
(in-package :b-tree)

(defmethod b-tree-compensation ((tree b-tree) parent node)
  "Distribute if there is non full right or left node, return T on success NIL if compensation is not possible"
  (if-let ((right (right-non-full tree parent)))
    (values t (b-node-distribute tree parent node right))
    (when-let ((left (left-non-full tree parent)))
      (values t (b-node-distribute tree (ref-left parent) left node)))))

(defmethod b-tree-underflow-compensation ((tree b-tree) parent node)
  "Distribute if there is non full right or left node, return T on success NIL if compensation is not possible"
  (if-let ((right (right-non-underflow tree parent node)))
    (values t (b-node-distribute tree parent node right))
    (when-let ((left (left-non-underflow tree parent node)))
      (values t (b-node-distribute tree (ref-left parent) left node)))))

