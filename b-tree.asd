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
               (:file "pager-aux")
               (:file "b-tree")
               (:file "b-tree-io")
               (:file "b-tree-insert")))
