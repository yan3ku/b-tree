(in-package :pager)

(defparameter *page-buf* nil)

(defun page-write-i4 (i4) (seq-write-i4 i4 *page-buf*))
(defun page-read-i4  ()   (seq-read-i4 *page-buf*))

(defun read-page (page-addr)
  "Load page from index file beginning at address"
  (let ((buf (make-page-buf)))
    (assert (file-position (index-file *pager*) (page-byte0 page-addr)))
    (setf (fill-pointer buf) (page-size *pager*))
    (read-sequence buf (index-file *pager*))
    ;; poping from vector (what record::seq-read does) effectively reverses the order
    ;; so this nreverse cancels out
    (nreverse buf)))

(defun write-page (page-addr page-buf)
  "Write page to the index file"
  (assert (file-position (index-file *pager*) (page-byte0 page-addr)))
  (write-sequence page-buf (index-file *pager*)))

(defmacro with-out-page (page-nr &body body)
  `(let ((*page-buf* (make-page-buf)))
     ,@body
     (write-page ,page-nr *page-buf*)))

(defmacro with-in-page (page-nr &body body)
  `(let ((*page-buf* (read-page ,page-nr)))
     ,@body))

(defmethod read-header ((self pager))
  (when (> 8 (file-length (index-file self)))
    (error "file doesn't have header"))
  (setf (page-size self) 8)
  (with-in-page 0
    (setf (page-size  self) (page-read-i4))
    (setf (page-count self) (page-read-i4))))

(defmethod write-header ((self pager))
  (with-out-page 0
    (page-write-i4 (page-size  self))
    (page-write-i4 (page-count self))))

(defun open-pager (name &optional page-size)
  (when *pager* (close-pager))
  (setf *pager* (make-instance 'pager :page-size page-size))
  (let* ((index-file-name  (concatenate 'string name ".i"))
         (record-file-name (concatenate 'string name ".r"))
         (open-opts `(:direction :io
                      :if-exists ,(if page-size :supersede :append)
                      :if-does-not-exist :create
                      :element-type (unsigned-byte 8))))
    (with-slots (record-file index-file) *pager*
      (setf record-file (apply #'open record-file-name open-opts))
      (setf index-file  (apply #'open index-file-name  open-opts)))
    (if page-size
        (progn
          (setf (page-count *pager*) 1) ;  page 0 is header and page 1 is root
          (write-header *pager*))
        (read-header *pager*))))

(defun close-pager (&key (delete nil delete-p))
  ;; (write-header *pager*)
  (declare (ignore delete))
  (when *pager*
    (close (record-file *pager*))
    (close (index-file  *pager*))
    (when  delete-p
      (delete-file (record-file *pager*))
      (delete-file (index-file *pager*)))  
    (setf *pager* nil)))

