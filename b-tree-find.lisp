;; b-tree-find.lisp
(in-package :b-tree)

(defmethod b-node-find ((tree b-tree) node (to-find b-key))
  (let* ((key-count (node-keys-count node))
         (keys (node-keys node))
         (low 0)
         (high (1- key-count))
         (result (make-key-ref node (node-keys-count node))))
    (loop
      (when (> low high)
        (return result))
      (let* ((mid (floor (+ low high) 2))
             (mid-key (aref keys mid)))
        (cond
          ((key= mid-key to-find)
           (return (make-key-ref node mid)))
          ((key> mid-key to-find)
           (setf result (make-key-ref node mid))
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
            (b-tree-find tree found to-find node)))))

(defmethod b-tree-walk ((tree b-tree) node-addr (to-find b-key) &optional parent-ref)
  "Walk until the node for insertion of key is found."
  (let ((node (read-node tree node-addr)))
    (for-keys ((k i) node)
      (when (key> k to-find)
        (if (b-pred-ptr k)
            (return (b-tree-walk tree (b-pred-ptr k) to-find (make-key-ref node i)))
            (return (list parent-ref (make-key-ref node i)))))
      :finally (if (node-succ-ptr node)
                   (return (b-tree-walk tree (node-succ-ptr node) to-find (make-key-ref node i)))
                   (return (list parent-ref (make-key-ref node i)))))))

(defmethod b-subtree-print (stream (tree b-tree) node-addr &optional (depth 0))
  (let ((node (read-node tree node-addr)))
    (format t "~&~V@T- " (* depth 2))
    (prin1 node)

    (for-keys ((k i) node)
      (when (b-pred-ptr k)
        (b-subtree-print stream tree (b-pred-ptr k) (1+ depth))))

    (when (node-succ-ptr node)
      (b-subtree-print stream tree (node-succ-ptr node) (1+ depth)))))

(defmethod print-object ((tree b-tree) stream)
  (b-subtree-print stream tree (root-addr tree)))

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
