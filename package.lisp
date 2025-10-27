;;;; package.lisp

(defpackage #:record
  (:use #:cl)
  (:export #:read-record  #:write-record
           #:seq-write-i4 #:seq-read-i4
           #:make-record
           #:current-page-size #:current-page-count
           #:make-random-record))

(defpackage #:pager
  (:use #:cl)
  (:import-from #:record #:seq-write-i4 #:seq-read-i4)
  (:export #:pager #:*pager*
           #:open-pager #:close-pager
           #:seq-write-i4 #:seq-read-i4
           #:next-page-addr
           ;; #:read-record #:write-record
           #:make-page-buf
           #:read-page   #:write-page
           #:with-in-page #:with-out-page
           #:page-read-i4 #:page-write-i4
           #:page-nr #:page-off))

(defpackage #:pager-test
  (:use #:cl #:fiveam #:pager))

(defpackage #:b-tree
  (:use #:cl #:pager #:record))
