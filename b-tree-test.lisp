;; b-tree-test.lisp

(in-package :b-tree-test)

(def-suite b-tree)

(in-suite b-tree)

(test create-b-tree
  (for-all ((order (gen-integer :min 0 :max 9999)))
    (let ((tree (make-b-tree "b-tree-test" order)))
      (unwind-protect
           (progn
             (close-b-tree tree)
             (setf tree (make-b-tree "b-tree-test"))
             (is (equal order (tree-order tree)))
             (is (zerop (node-keys-count (tree-root tree)))))
        (close-b-tree tree :delete t)))))

(test split-test
  (for-all ((test (gen-list :length (gen-integer :min 3 :max 10000))))
    (let ((a (make-array (length test) :initial-contents test :fill-pointer (length test)))
          (b (make-array (length test) :fill-pointer 0)))
      (let ((m (b-tree::vector-split-into-lmr a b)))
        (is (equalp test (concatenate 'list a (list m) b)))))))

(test insertion-test
  (with-tree (tree "b-tree-test" :order 10)
    (let ((expected
            (loop for i from 0 to 10000
                  for r = (random 100000)
                  collect r do
                    (b-tree-insert tree (b-tree::make-b-key r)))))
      (b-tree::b-tree-print tree)
      (let ((result nil))
        (b-tree::b-tree-traverse tree (b-tree::root-addr tree)
                                 (lambda (key) (push (b-tree::b-key key) result)))
        (is (equal (sort expected #'>) result)))
      (format t "~%compensation: ~A~%" (b-tree::compensation-count tree))
      (format t "split ~A~%" (b-tree::split-count tree)))))

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
