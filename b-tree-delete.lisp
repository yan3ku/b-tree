;;; b-tree-delete.lisp
(in-package :b-tree)

(defun find-leaf-replacement (tree parent)
  (if-let ((right (right-sibling tree parent)))
    (make-key-ref right 0)
    (when-let ((left (left-sibling tree parent)))
      (make-key-ref left (node-keys-count left)))))


(defmethod b-tree-delete ((tree b-tree) (key b-key))
  (when-let ((found (b-tree-find tree key)))
    (multiple-value-bind (ref parent)
        found
      (cond
        ((b-node-leafp (ref-node ref))
         (b-node-delete-key tree ref))
        ((b-node-internalp (ref-node ref))
         (let ((replacement (find-leaf-replacement tree parent)))
           (setf (ref-key ref) (ref-key replacement))
           (b-node-delete-key tree replacement)))))))
