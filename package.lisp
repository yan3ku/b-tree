;;;; package.lisp

(defpackage #:record
  (:use #:cl)
  (:export #:read-record-from-stream  #:write-record-to-stream
           #:seq-write-i4 #:seq-read-i4
           #:make-record
           #:record-value
           #:make-random-record))

(defpackage #:pager
  (:use #:cl)
  (:import-from #:record #:seq-write-i4 #:seq-read-i4)
  (:export #:pager #:*pager*
           #:open-pager #:close-pager
           #:seq-write-i4 #:seq-read-i4
           #:read-header
           #:write-header
           #:next-page-addr
           #:page-size
           #:page-position
           #:page-read-n
           #:page-write-at
           #:header-addr
           #:new-page-addr
           #:new-record-addr
           #:read-record #:write-record
           #:make-page-buf
           #:read-page   #:write-page
           #:with-in-page #:with-out-page
           #:page-read-i4 #:page-write-i4
           #:page-nr #:page-off))

(defpackage #:b-tree
  (:use #:cl #:pager #:record #:alexandria)
  (:export #:b-tree
           #:tree-root
           #:show-stats
           #:make-b-tree
           #:node-keys
           #:node-keys-count
           #:with-tree
           #:ref-key
           #:b-key
           #:close-b-tree
           #:tree-order
           #:make-b-key
           #:make-b-key-record
           #:b-key
           #:b-tree-inorder-map
           #:b-tree-insert
           #:b-tree-delete))

(defpackage #:pager-test
  (:use #:cl #:fiveam #:pager))

(defpackage #:b-tree-test
  (:use #:cl #:fiveam #:pager #:record #:b-tree))
