;;;; b-tree.asd

(asdf:defsystem #:b-tree
  :description "Implementation of b-tree for database index use."
  :author "Yaneko Yoruneko"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "pager")
               (:file "b-tree")))
