;;;; package.lisp

(defpackage #:record
  (:use #:cl)
  (:export #:read-record #:write-record
           #:make-record
           #:make-random-record))

(defpackage #:pager
  (:use #:cl)
  (:export #:*pager*
           #:read-record #:write-record
           #:read-page   #:write-page
           #:page-nr #:page-off))

(defpackage #:b-tree
  (:use #:cl #:pager))
