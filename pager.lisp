;; pager.lisp
(in-package :pager)

(defclass pager ()
  ((header-addr
    :initform 0
    :reader header-addr
    :allocation :class)
   (index-file
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
    :documentation "The number of currently managed pages")
   (record-count
    :accessor record-count
    :documentation "The number of records managed"))
  (:documentation "Disk access layer implementing block reads and writes for B-tree nodes"))

(defmacro page-numer  (addr) `(ldb (byte 16 0)  ,addr))
(defmacro page-offset (addr) `(ldb (byte 16 16) ,addr))

(defmethod page-byte0 ((p pager) addr)
  "Return addres of first byte (byte 0) on page"
  (* (page-numer addr) (page-size p)))

(defmethod page-byte  ((p pager) addr)
  "Return address of byte under page with added offset"
  (+ (page-byte0 addr p) (page-offset addr)))

(defmethod record-byte ((p pager) addr)
  (* 4 addr))

(defmethod make-page-buffer ((p pager))
  (make-array (page-size p) :element-type '(unsigned-byte 8) :fill-pointer 0))

(defmethod new-page-addr ((p pager))
  (incf (page-count p)))

(defmethod new-record-addr ((p pager))
  (incf (record-count p)))
