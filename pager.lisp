;; pager.lisp
(in-package :pager)

(defmacro page-nr  (addr) `(ldb (byte 16 0)  ,addr))
(defmacro page-off (addr) `(ldb (byte 16 16) ,addr))
(defun page-byte-loc (addr) (* (page-nr addr) (page-size *pager*)))
(defun byte-loc      (addr) (+ (page-byte-loc addr) (page-off addr)))

(defparameter *pager* nil)

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
    :initarg  :page-size
    :accessor page-size
    :documentation "The size of one page"))
  (:documentation "Disk access layer implementing block reads and writes for B-tree nodes"))

(defun open-pager (name page-size)
  (let* ((pager (make-instance 'pager :page-size page-size))
         (index-file-name  (concatenate 'string name ".i"))
         (record-file-name (concatenate 'string name ".r"))
         (open-opts '(:direction :io :if-exists :supersede :if-does-not-exist :create
                      :element-type (unsigned-byte 8))))
    (with-slots (record-file index-file) pager
      (setf record-file (apply #'open record-file-name open-opts))
      (setf index-file  (apply #'open index-file-name  open-opts)))
    (setf *pager* pager)))

(defun close-pager ()
  (close (record-file pager))
  (close (index-file pager)))

(defun read-page (page-addr)
  "Load page from index file beginning at address"
  (let ((buf (make-array (page-size *pager*) :element-type '(unsigned-byte 8))))
    (assert (file-position (index-file *pager*) (page-byte-loc page-addr)))
    (read-sequence buf (index-file *pager*))
    buf))

(defun write-page (page-addr page-buf)
  "Write page to the index file"
  (assert (file-position (index-file *pager*) (page-byte-loc page-addr)))
  (write-sequence page-buf (index-file *pager*)))

(defun write-record (record &optional addr)
  (when addr
    (assert (file-position (index-file *pager*) (byte-loc addr))))
  (record:write-record record (record-file *pager*)))

(defun read-record (&optional addr)
  (when addr
    (assert (file-position (index-file *pager*) (byte-loc addr))))
  (record:read-record (record-file *pager*)))
