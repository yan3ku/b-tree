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
  (with-tree (tree "b-tree-test" :order 10 :delete t)
    (let ((expected
            (loop for i from 0 to 10000
                  for r = (random 100000)
                  for insert = (multiple-value-list (b-tree-insert tree (make-b-key r)))
                  when (car insert)
                    collect (b-key (ref-key (cadr insert))))))
      ;; (print tree)
      (let ((result nil))
        (b-tree-inorder-map tree
                            (lambda (key) (push (b-key key) result)))
        (is (equal (sort expected #'>) result)))
      (show-stats tree t))))

(test deletion-test
  (with-tree (tree "b-tree-delete-test" :order 10 :delete t)
    ;; Define the full set of keys for insertion (1 to 1000)
    (let* ((initial-keys (loop for i from 1 to 10000 collect i))
           ;; Define the subset of keys to delete (all even numbers)
           (keys-to-delete (remove-if-not (lambda (x) (= (mod x 2) 0)) initial-keys))
           ;; The expected keys remaining after deletion (all odd numbers)
           ;; We ensure this list is sorted ascending for final comparison.
           (expected-remaining (sort (set-difference initial-keys keys-to-delete :test 'equal) #'<)))

      ;; 1. Insertion Phase: Populate the tree
      (loop for k in initial-keys
            do (b-tree-insert tree (make-b-key k)))

      ;; 2. Deletion Phase: Delete the chosen subset
      (loop for k in keys-to-delete
            do (b-tree-delete tree (make-b-key k)))

      ;; 3. Verification Phase: Check the keys remaining in the tree via inorder traversal
      (let ((result-keys nil))
        (b-tree-inorder-map tree
                            ;; Keys are pushed to the head of the list, resulting in descending order.
                            (lambda (key) (push (b-key key) result-keys)))

        ;; To compare against the ascending 'expected-remaining' list, we reverse the map result.
        ;; If the B-tree is correctly structured, the inorder map should produce sorted keys.
        (is (equal expected-remaining (reverse result-keys))))
      ;; (print tree)
      (show-stats tree t))))
