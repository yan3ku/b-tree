;; pager.lisp
(in-package :pager)

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

(defmacro page-numer  (addr) `(ldb (byte 16 0)  ,addr))
(defmacro page-offset (addr) `(ldb (byte 16 16) ,addr))

(defmethod page-byte0 ((p pager) addr)
  "Return addres of first byte (byte 0) on page"
  (* (page-numer addr) (page-size p)))

(defmethod page-byte  ((p pager) addr)
  "Return address of byte under page with added offset"
  (+ (page-byte0 addr p) (page-offset addr)))

(defmethod make-page-buffer ((p pager))
  (make-array (page-size p) :element-type '(unsigned-byte 8) :fill-pointer 0))

(defmethod pager-new-page-addr ((p pager))
  (incf (page-count p)))

;; (defun write-record (record &optional addr)
;;   (when addr
;;     (assert (file-position (index-file *pager*) (byte-loc addr))))
;;   (record:write-record record (record-file *pager*)))

;; (defun read-record (&optional addr)
;;   (when addr
;;     (assert (file-position (index-file *pager*) (byte-loc addr))))
;;   (record:read-record (record-file *pager*)))
