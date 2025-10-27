(in-package :pager)

(defparameter *page-buf* nil)

(defun page-write-i4 (i4) (seq-write-i4 i4 *page-buf*))
(defun page-read-i4  ()   (seq-read-i4 *page-buf*))

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
          (setf (page-count *pager*) 0)
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

