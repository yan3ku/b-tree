;;;; b-tree.asd

(asdf:defsystem #:b-tree
  :description "Implementation of b-tree for database index use."
  :author "Yaneko Yoruneko"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "record")
               (:file "pager")
               (:file "pager-io")
               (:file "b-tree")
               (:file "b-tree-io")
               (:file "b-tree-insert"))
  :in-order-to ((test-op (test-op #:b-tree/test))))


(asdf:defsystem #:b-tree/test
  :description "Implementation of b-tree for database index use."
  :author "Yaneko Yoruneko"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (#:b-tree)
  :serial t
  :components ((:file "pager-test"))
  :perform (test-op (o s)
                    (uiop:symbol-call :fiveam :run-all-tests)))
