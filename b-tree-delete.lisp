;;; b-tree-delete.lisp
(in-package :b-tree)

(defmethod b-tree-merge ((tree b-tree) parent node)
  (if-let ((right (right-underflow tree parent node)))
    (b-node-merge tree parent node right)
    (when-let ((left (left-underflow tree parent node)))
      (b-node-merge tree (ref-left parent) left node))))

(defmethod b-tree-delete ((tree b-tree) (key b-key))
  (multiple-value-bind (to-del to-del-parent)
      (b-tree-find tree key)
    (when to-del
      (when (b-node-internalp (ref-node to-del))
        (multiple-value-bind (max max-parent)
            (left-subtree-max-key tree to-del)
          (replace-key (ref-key to-del) (ref-key max))
          (mark-dirty tree (ref-node to-del))
          (setf to-del max)
          (setf to-del-parent max-parent)))
      (assert (b-node-leafp (ref-node to-del)))
      (prog1 (ref-key to-del)
        (b-node-delete-key tree to-del)
        (write-dirty tree)
        (unless (root-p tree (ref-node to-del))
          (unless (b-tree-underflow-compensation tree to-del-parent (ref-node to-del))
            (b-tree-merge tree to-del-parent (ref-node to-del))
            ;; make the merged node root if there is no keys in root
            (when (and (root-p tree (ref-node to-del-parent))
                       (= 0 (node-keys-count (ref-node to-del-parent))))
              (setf (root-addr tree) (ref-node-addr to-del)))))))))

#+nil
(b-tree-print *t*)
