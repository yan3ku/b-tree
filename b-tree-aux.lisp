;;; b-tree-aux.lisp
(in-package :b-tree)

(defmacro with-tree ((var name &key order delete) &body body)
  `(let ((,var (make-b-tree ,name ,order)))
     (unwind-protect
          (progn ,@body)
       (close-b-tree ,var :delete ,delete))))

(defun right-non-full (tree parent)
  "Return non full right node; if not possible nil"
  (when (not (ref-succession-p parent))
    (let ((right (read-node tree (ref-succ-ptr parent))))
      (when (not (node-2-left-empty-p tree right))
        right))))

(defun left-non-full (tree parent)
  "Return non full left node; if not possible nil"
  (when (> (ref-index parent) 0)
    (let ((left (read-node tree (ref-pred-ptr parent))))
      (when (not (node-2-left-empty-p tree left))
        left))))

(defmethod b-tree-compensation ((tree b-tree) parent node)
  "Distribute if there is non full right or left node, return T on success NIL if compensation is not possible"
  (if-let ((right (right-non-full tree parent)))
    (progn
      (b-node-distribute tree parent node right)
      t)
    (when-let ((left (left-non-full tree parent)))
      (progn
        (b-node-distribute tree (ref-pred-key parent) left node)
        t))))
