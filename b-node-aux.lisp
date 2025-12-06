;;; b-node-aux.lisp
(in-package :b-tree)

(defun node-right-sibling (tree parent)
  (when (not (ref-node-succ-p parent))
    (let ((right (read-node tree (ref-right-ptr parent))))
      right)))

(defun node-left-sibling (tree parent)
  (when (> (ref-index parent) 0)
    (let ((left (read-node tree (ref-left-ptr parent))))
      left)))

(defun right-non-full (tree parent)
  "Return non full right node; if not possible nil"
  (when-let ((right (node-right-sibling tree parent)))
    (when (not (node-2-left-empty-p tree right))
      right)))

(defun left-non-full (tree parent)
  "Return non full left node; if not possible nil"
  (when-let ((left (node-left-sibling tree parent)))
    (when (not (node-2-left-empty-p tree left))
      left)))

(defun nodes-underflow-p (tree &rest nodes)
  "When sum of node-keys-count is smaller than tree-order its possible to merge node."
  (> (tree-order tree) (reduce #'+ (mapcar #'node-keys-count nodes))))

(defun right-non-underflow (tree parent node)
  (when-let ((right (node-right-sibling tree parent)))
    (unless (nodes-underflow-p tree node right)
      right)))

(defun left-non-underflow (tree parent node)
  (when-let ((left (node-left-sibling tree parent)))
    (unless (nodes-underflow-p tree node left)
      left)))

(defun right-underflow (tree parent node)
  (when-let ((right (node-right-sibling tree parent)))
    (when (nodes-underflow-p tree node right)
      right)))

(defun left-underflow (tree parent node)
  (when-let ((left (node-left-sibling tree parent)))
    (when (nodes-underflow-p tree node left)
      left)))
