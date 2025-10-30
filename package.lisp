;;;; package.lisp

(defpackage #:record
  (:use #:cl)
  (:export #:read-record  #:write-record
           #:seq-write-i4 #:seq-read-i4
           #:make-record
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
           ;; #:read-record #:write-record
           #:make-page-buf
           #:read-page   #:write-page
           #:with-in-page #:with-out-page
           #:page-read-i4 #:page-write-i4
           #:page-nr #:page-off))

(defpackage #:b-tree
  (:use #:cl #:pager #:record)
  (:export #:b-tree
           #:tree-root
           #:make-b-tree
           #:node-keys
           #:node-keys-count
           #:close-b-tree
           #:tree-order))

(defpackage #:pager-test
  (:use #:cl #:fiveam #:pager))

(defpackage #:b-tree-test
  (:use #:cl #:fiveam #:pager #:b-tree))
