(in-package :pager)

(defparameter *page-buf* nil)

(defun page-write-i4 (i4) (seq-write-i4 i4 *page-buf*))
(defun page-read-i4  ()   (seq-read-i4 *page-buf*))

(defmethod read-page ((p pager) page-addr)
  "Load page from index file beginning at address"
  (let ((buf (make-page-buffer p)))
    (assert (file-position (index-file p) (page-byte0 p page-addr)))
    (setf (fill-pointer buf) (page-size p))
    (read-sequence buf (index-file p))
    ;; poping from vector (what record::seq-read does) effectively reverses the order
    ;; so this nreverse cancels out
    (nreverse buf)))

(defmethod write-page ((p pager) page-addr page-buf)
  "Write page to the index file"
  (assert (file-position (index-file p) (page-byte0 p page-addr)))
  (write-sequence page-buf (index-file p)))

(defmacro with-out-page (pager page-nr &body body)
  `(let ((*page-buf* (make-page-buffer ,pager)))
     ,@body
     (write-page ,pager ,page-nr *page-buf*)))

(defmacro with-in-page (pager page-nr &body body)
  `(let ((*page-buf* (read-page ,pager ,page-nr)))
     ,@body))

(defmethod read-header ((p pager))
  (when (> 8 (file-length (index-file p)))
    (error "file doesn't have header"))
  (setf (page-size p) 8)
  (with-in-page p 0
    (setf (page-size  p) (page-read-i4))
    (setf (page-count p) (page-read-i4))))

(defmethod write-header ((p pager))
  (with-out-page p 0
    (page-write-i4 (page-size  p))
    (page-write-i4 (page-count p))))

(defun open-pager (name &optional page-size)
  (let ((pager (make-instance 'pager :page-size page-size)))
    (let* ((index-file-name  (concatenate 'string name ".i"))
           (record-file-name (concatenate 'string name ".r"))
           (open-opts `(:direction :io
                        :if-exists ,(if page-size :supersede :append)
                        :if-does-not-exist :create
                        :element-type (unsigned-byte 8))))
      (with-slots (record-file index-file) pager
        (setf record-file (apply #'open record-file-name open-opts))
        (setf index-file  (apply #'open index-file-name  open-opts)))
      (if page-size
          (progn
            (setf (page-count pager) 1) ;  page 0 is header and page 1 is root
            (write-header pager))
          (read-header pager))
      pager)))

(defmethod close-pager ((p pager) &key (delete nil delete-p))
  ;; (write-header p)
  (declare (ignore delete))
  (close (record-file p))
  (close (index-file  p))
  (when  delete-p
    (delete-file (record-file p))
    (delete-file (index-file p))))

