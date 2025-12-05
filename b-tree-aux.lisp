;;; b-tree-aux.lisp
(in-package :b-tree)

(defmacro with-tree ((var name &key order delete) &body body)
  `(let ((,var (make-b-tree ,name ,order)))
     (unwind-protect
          (progn ,@body)
       (close-b-tree ,var :delete ,delete))))

(defun right-sibling (tree parent)
  (when (not (ref-succession-p parent))
    (let ((right (read-node tree (ref-succ-ptr parent))))
      right)))

(defun left-sibling (tree parent)
  (when (> (ref-index parent) 0)
    (let ((left (read-node tree (ref-pred-ptr parent))))
      left)))

(defun right-non-full (tree parent)
  "Return non full right node; if not possible nil"
  (when-let ((right (right-sibling tree parent)))
    (when (not (node-2-left-empty-p tree right))
      right)))

(defun left-non-full (tree parent)
  "Return non full left node; if not possible nil"
  (when-let ((left (left-sibling tree parent)))
    (when (not (node-2-left-empty-p tree left))
      left)))

(defun enough-to-compensate (tree x)
  (<= (tree-order tree) x))

(defun right-non-underflow (tree parent node)
  (when-let ((right (right-sibling tree parent)))
    (when (enough-to-compensate tree
                                (+ (node-keys-count node)
                                   (node-keys-count right)))
      right)))

(defun left-non-underflow (tree parent node)
  "Return non full left node; if not possible nil"
  (when-let ((left (left-sibling tree parent)))
    (when (enough-to-compensate tree
                                (+ (node-keys-count node)
                                   (node-keys-count left)))
      left)))

(defmethod b-tree-underflow-compensation ((tree b-tree) parent node)
  "Distribute if there is non full right or left node, return T on success NIL if compensation is not possible"
  (if-let ((right (right-non-underflow tree parent node)))
    (values t (b-node-distribute tree parent node right))
    (if-let ((left (left-non-underflow tree parent node)))
      (values t (b-node-distribute tree (ref-pred-key parent) left node))
      )))

(defmethod b-tree-merge ((tree b-tree) parent node)
  )

(defmethod b-tree-compensation ((tree b-tree) parent node)
  "Distribute if there is non full right or left node, return T on success NIL if compensation is not possible"
  (if-let ((right (right-non-full tree parent)))
    (values t (b-node-distribute tree parent node right))
    (when-let ((left (left-non-full tree parent)))
      (values t (b-node-distribute tree (ref-pred-key parent) left node)))))
