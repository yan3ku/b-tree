;;; b-node-op.lisp
(in-package :b-tree)

(defmethod b-node-split ((tree b-tree) (left-node b-node))
  (let* ((right-node (make-new-b-node tree))
         (middle (vector-split-into-lmr (node-keys left-node) (node-keys right-node))))
    (mark-dirty tree left-node)
    (setf (node-succ-ptr right-node) (node-addr left-node))
    ;; (succ right node) <- (succ left node) <- middle <- left-node
    (rotatef (node-succ-ptr right-node) (node-succ-ptr left-node) (b-pred-ptr middle))
    (list left-node middle right-node)))


(defmethod b-node-distribute ((tree b-tree) mid-ref (lt b-node) (rt b-node))
  (let* ((merge (vector-merge (node-keys lt)
                              (list (ref-key mid-ref))
                              (node-keys rt)))
         (split-point (truncate (length merge) 2)))
    ;; we have to avoid loops like this:
    ;; - 3&(30 200)
    ;;   - 1&(-10 -1 0 10 20)
    ;;   - 2&(40 50 60 100)
    ;;   - 4&(300 400 500)
    ;; inserting -20 will redistribute with the same amount of keys in left node....
    (let ((new-mid (vector-split-into-lmr merge (node-keys rt) split-point)))
      ;; left node
      (adjust-array merge (tree-order tree))
      (setf (node-keys lt) merge)
      ;; unset old reference
      (setf (ref-ptr  mid-ref) nil)
      (setf (ref-key      mid-ref) new-mid)
      ;; ancestor node
      (setf (ref-ptr  mid-ref) (node-addr lt))
      (setf (ref-right-ptr mid-ref) (node-addr rt))

      (mark-dirty tree lt rt (ref-node mid-ref)))))

(defmethod b-node-merge ((tree b-tree) mid-ref (lt b-node) (rt b-node))
  (let* ((merge (vector-merge (node-keys lt)
                              (list (ref-key mid-ref))
                              (node-keys rt))))
    (setf (ref-ptr mid-ref) nil)
    ;; the mid key is removed and it contains the pred pointer to lt
    ;; so the rt is what remains, we reuse its page address here
    (let ((new (make-b-node tree (node-addr rt))))
      (setf (node-keys new) merge)
      (b-node-delete-key tree mid-ref)
      (mark-dirty tree new (ref-node mid-ref))
      new)))

(defmethod b-node-delete-key ((tree b-tree) ref)
  (vector-delete (node-keys (ref-node ref)) (ref-index ref))
  (mark-dirty tree (ref-node ref)))

(defmethod b-node-insert ((tree b-tree) (node b-node) index key)
  (vector-insert (node-keys node) index key)
  (mark-dirty tree node)
  (make-ref node index))
