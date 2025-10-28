;; pager.lisp
(in-package :pager)

(defparameter *pager* nil)

(defmacro page-numer  (addr) `(ldb (byte 16 0)  ,addr))
(defmacro page-offset (addr) `(ldb (byte 16 16) ,addr))
(defun page-byte0 (addr) (* (page-numer addr) (page-size *pager*)))
(defun page-byte  (addr) (+ (page-byte0 addr) (page-offset addr)))

(defclass pager ()
  ((index-file
    :initarg :index-file
    :accessor index-file
    :documentation "File containing B-tree index")
   (record-file
    :initarg :record-file
    :accessor record-file
    :documentation "File containing the indexed records")
   (page-size
    :initform nil
    :initarg  :page-size
    :accessor page-size
    :documentation "The size of one page")
   (page-count
    :accessor page-count
    :documentation "The number of currently managed pages"))
  (:documentation "Disk access layer implementing block reads and writes for B-tree nodes"))

(defun current-page-size ()
  (page-size pager::*pager*))

(defun current-page-count ()
  (page-count pager::*pager*))

(defun make-page-buf ()
  (make-array (page-size *pager*) :element-type '(unsigned-byte 8) :fill-pointer 0))

(defun next-page-addr ()
  (incf (page-count *pager*)))

;; (defun write-record (record &optional addr)
;;   (when addr
;;     (assert (file-position (index-file *pager*) (byte-loc addr))))
;;   (record:write-record record (record-file *pager*)))

;; (defun read-record (&optional addr)
;;   (when addr
;;     (assert (file-position (index-file *pager*) (byte-loc addr))))
;;   (record:read-record (record-file *pager*)))
