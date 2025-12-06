;;;; b-tree.asd

#+nil
(progn
  (setf fiveam:*run-test-when-defined* t)
  (setf fiveam:*on-error* :debug))

(asdf:defsystem #:b-tree
  :description "Implementation of b-tree for database index use."
  :author "Yaneko Yoruneko"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "vector-aux")
               (:file "record")
               (:file "pager")
               (:file "pager-io")
               (:file "b-tree")
               (:file "b-key")
               (:file "b-node")
               (:file "b-tree-stats")
               (:file "b-tree-io")
               (:file "b-node-aux")
               (:file "b-node-op")
               (:file "b-tree-compensation")
               (:file "b-tree-find")
               (:file "b-tree-insert")
               (:file "b-tree-delete"))
  :in-order-to ((test-op (test-op #:b-tree/test))))


(asdf:defsystem #:b-tree/test
  :description "Implementation of b-tree for database index use."
  :author "Yaneko Yoruneko"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (#:b-tree)
  :serial t
  :components ((:file "pager-test")
               (:file "b-tree-test"))
  :perform (test-op (o s)
                    (uiop:symbol-call :fiveam :run-all-tests)))
