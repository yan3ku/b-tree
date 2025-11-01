;;; b-tree-dirties.lisp
(in-package :b-tree)

(defmethod mark-dirty ((tree b-tree) &rest dirty)
  (dolist (node dirty)
    (pushnew node (tree-dirty-list tree))))

(defmethod write-dirty ((tree b-tree))
  (dolist (node (tree-dirty-list tree))
    (write-node tree node))
  (setf (tree-dirty-list tree) '()))

(defmethod b-tree-insert :after ((tree b-tree) (key b-key))
  (write-dirty tree))
