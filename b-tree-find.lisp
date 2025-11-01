;; b-tree-find.lisp
(in-package :b-tree)

(defmethod b-node-find ((tree b-tree) node (to-find b-key))
  (for-keys ((k i) node)
    (when (key> k to-find)
      (return (make-key-ref node i)))
    :finally (return (make-key-ref node i))))

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

(defmethod b-tree-print ((tree b-tree)  &optional (node-addr (root-addr tree)) (depth 0))
  "Recursively walk the B-tree and print every node in a readable way."
  (let ((node (read-node tree node-addr)))
    ;; Indentation for tree structure
    (format t "~&~V@T- " (* depth 2))
    ;; Print the node itself
    (prin1 node)

    ;; Recursively visit children (if this node has pointers)
    (for-keys ((k i) node)
      (when (b-pred-ptr k)
        ;; Go down to the child
        (b-tree-print tree (b-pred-ptr k) (1+ depth))))

    ;; Handle the rightmost child (after all keys)
    (when (node-succ-ptr node)
      (b-tree-print tree (node-succ-ptr node) (1+ depth)))))

(defmethod b-tree-traverse ((tree b-tree) &optional (node-addr (root-addr tree)) (callback #'print))
  "In-order traversal of the B-tree.
   Calls CALLBACK on each key (b-key) in sorted order."
  (let ((node (read-node tree node-addr)))
    (for-keys ((k i) node)
      (when (b-pred-ptr k)
        (b-tree-traverse tree (b-pred-ptr k) callback))
      (funcall callback k))
    (when (node-succ-ptr node)
      (b-tree-traverse tree (node-succ-ptr node) callback))))
