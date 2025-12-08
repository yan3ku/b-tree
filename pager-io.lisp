(in-package :pager)

(defparameter *page-buffer* nil)
(defparameter *page-write-at* 0)

(defun page-write-i4 (i4) (seq-write-i4 i4 *page-buffer*))
(defun page-read-i4  ()   (seq-read-i4 *page-buffer*))
(defun page-read-n (n)
  "Consume n bytes; usefull for reading from offset"
  (loop :repeat n :do (vector-pop *page-buffer*)))

(defun page-write-at (n)
  "Sets offset in page for appending"
  (setf *page-write-at* n))

(defmethod read-page ((p pager) page-addr)
  "Load page from index file beginning at address"
  (let ((buf (make-page-buffer p)))
    (assert (file-position (index-file p) (page-byte0 p page-addr)))
    (setf (fill-pointer buf) (page-size p))
    (read-sequence buf (index-file p))
    ;; poping from vector (what record::seq-read does) effectively reverses the order
    ;; so this nreverse cancels out
    (nreverse buf)))

(defmethod write-page ((p pager) page-addr page-buffer)
  "Write page to the index file"
  (assert (file-position (index-file p) (+ (page-byte0 p page-addr)
                                           *page-write-at*)))
  (write-sequence page-buffer (index-file p))
  (setf *page-write-at* 0))

(defmacro with-out-page (pager page-nr &body body)
  `(let ((*page-buffer* (make-page-buffer ,pager)))
     ,@body
     (write-page ,pager ,page-nr *page-buffer*)))


(defmacro with-in-page (pager page-nr &body body)
  `(let ((*page-buffer* (read-page ,pager ,page-nr)))
     ,@body))

(defmethod read-header ((p pager))
  (when (> 8 (file-length (index-file p)))
    (error "file doesn't have header"))
  (setf (page-size p) 12)
  (with-in-page p 0
    (setf (page-size    p) (page-read-i4))
    (setf (page-count   p) (page-read-i4))
    (setf (record-count p) (page-read-i4))))

(defmethod write-header ((p pager))
  (with-out-page p 0
    (page-write-i4 (page-size  p))
    (page-write-i4 (page-count p))
    (page-write-i4 (record-count p))))

(defmethod write-record ((p pager) record addr)
  (file-position (record-file p) (record-byte p addr))
  (record:write-record-to-stream record (record-file p))
  addr)

(defmethod read-record ((p pager) addr)
  (file-position (record-file p) (record-byte p addr))
  (record:read-record-from-stream (record-file p)))


(defmethod initialize-instance :after ((p pager) &key index-file record-file)
  (let* ((open-opts `(:direction :io
                      :if-exists ,(if (page-size p) :supersede :append)
                      :if-does-not-exist :create
                      :element-type (unsigned-byte 8))))
    (setf (record-file p) (apply #'open record-file open-opts))
    (setf (index-file p)  (apply #'open index-file  open-opts))
    (if (page-size p)
        (progn
          (setf (record-count p) 0)
          (setf (page-count p) (1+ (header-addr p))) ;  page 0 is header
          (write-header p))
        (read-header p))))

(defun open-pager (name &optional page-size)
  (make-instance 'pager :index-file  (concatenate 'string name ".i")
                        :record-file (concatenate 'string name ".r")
                        :page-size page-size))

(defmethod close-pager ((p pager) &key delete)
  (write-header p)
  (close (record-file p))
  (close (index-file  p))
  (when  delete
    (delete-file (record-file p))
    (delete-file (index-file p))))

