;; b-tree-find.lisp
(in-package :b-tree)

(defmethod b-node-find ((tree b-tree) node (to-find b-key))
  (let* ((key-count (node-keys-count node))
         (keys (node-keys node))
         (low 0)
         (high (1- key-count))
         (result (make-ref node (node-keys-count node))))
    (loop
      (when (> low high)
        (return result))
      (let* ((mid (floor (+ low high) 2))
             (mid-key (aref keys mid)))
        (cond
          ((key= mid-key to-find)
           (return (make-ref node mid)))
          ((key> mid-key to-find)
           (setf result (make-ref node mid))
           (setf high (1- mid)))
          ((not (key> mid-key to-find))
           (setf low (1+ mid))))))))

(defmethod b-tree-find ((tree b-tree) (to-find b-key) &optional (node-addr (root-addr tree)) parent)
  (let* ((node (read-node tree node-addr))
         (found (b-node-find tree node to-find)))
    (if (and (ref-key found) (key= to-find (ref-key found)))
        (values found parent)
        (if (b-node-leafp node)
            nil
            (b-tree-find tree to-find (ref-ptr found) found)))))

(defmethod b-tree-inorder-map ((tree b-tree) &optional (callback #'print) (node-addr (root-addr tree)))
  "In-order traversal of the B-tree.
   Calls CALLBACK on each key (b-key) in sorted order."
  (let ((node (read-node tree node-addr)))
    (for-keys ((k i) node)
      (when (b-pred-ptr k)
        (b-tree-inorder-map tree callback (b-pred-ptr k)))
      (funcall callback k))
    (when (node-succ-ptr node)
      (b-tree-inorder-map tree callback (node-succ-ptr node)))))
