;; b-tree-test.lisp

(in-package :b-tree-test)

(def-suite b-tree)

(in-suite b-tree)

(test create-b-tree
  (for-all ((order (gen-integer :min 0 :max 9999)))
    (let ((tree (make-b-tree "test" order)))
      (unwind-protect
           (progn
             (close-b-tree tree)
             (setf tree (make-b-tree "test"))
             (is (equal order (tree-order tree)))
             (is (zerop (node-keys-count (tree-root tree)))))
        (close-b-tree tree :delete t)))))

(test split-test
  (for-all ((test (gen-list :length (gen-integer :min 3 :max 100))))
    (let ((a (make-array (length test) :initial-contents test :fill-pointer (length test)))
          (b (make-array (length test) :fill-pointer 0)))
      (destructuring-bind (l m r)
          (b-tree::vector-split a b)
        (is (equalp test (concatenate 'list l (list m) r)))))))

;; (defclass foo ()
;;   ((a)))

;; (defmethod foo-do2 ((f foo))
;;   (format t "the original foo-do2"))

;; (defmethod foo-do ((f foo))
;;   (foo-do2 f))

;; (defclass bar (foo)
;;   ())

;; (defmethod foo-do2 ((f bar))
;;   (format t "overwrite the original foo-do2"))
