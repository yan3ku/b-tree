;; pager.lisp
(in-package :pager)

(defparameter *pager* nil)

(defmacro page-nr  (addr) `(ldb (byte 16 0)  ,addr))
(defmacro page-off (addr) `(ldb (byte 16 16) ,addr))
(defun page-byte-loc (addr) (* (page-nr addr) (page-size *pager*)))
(defun byte-loc      (addr) (+ (page-byte-loc addr) (page-off addr)))

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

(defun read-page (page-addr)
  "Load page from index file beginning at address"
  (let ((buf (make-page-buf)))
    (assert (file-position (index-file *pager*) (page-byte-loc page-addr)))
    (setf (fill-pointer buf) (page-size *pager*))
    (read-sequence buf (index-file *pager*))
    ;; poping from vector (what record::seq-read does) effectively reverses the order
    ;; so this nreverse cancels out
    (nreverse buf)))

(defun write-page (page-addr page-buf)
  "Write page to the index file"
  (assert (file-position (index-file *pager*) (page-byte-loc page-addr)))
  (write-sequence page-buf (index-file *pager*)))

;; (defun write-record (record &optional addr)
;;   (when addr
;;     (assert (file-position (index-file *pager*) (byte-loc addr))))
;;   (record:write-record record (record-file *pager*)))

;; (defun read-record (&optional addr)
;;   (when addr
;;     (assert (file-position (index-file *pager*) (byte-loc addr))))
;;   (record:read-record (record-file *pager*)))
