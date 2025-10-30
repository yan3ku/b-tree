;; record.lisp
(in-package :record)

(defclass record ()
  ((value
    :initarg :value
    :accessor record-value
    :documentation "Time record stored as unix timestamp int32")))

(defun make-record (&optional (sec 0))
  (make-instance 'record :value sec))

(defun make-random-record ()
  (make-instance 'record :value (random (get-universal-time))))

(defun seq-read-i4 (buf)
  (let ((i4 0))
    (declare (fixnum i4) ((vector (unsigned-byte 8)) buf))
    (setf (ldb (byte 8 24) i4) (vector-pop buf))
    (setf (ldb (byte 8 16) i4) (vector-pop buf))
    (setf (ldb (byte 8 8)  i4) (vector-pop buf))
    (setf (ldb (byte 8 0)  i4) (vector-pop buf))
    (- (ldb (byte 32 0) (+ i4 (ash 1 31))) (ash 1 31))))

(defun seq-write-i4 (i4 buf)
  (declare (fixnum i4) ((vector (unsigned-byte 8)) buf))
  (when (= (length buf) (array-total-size buf))
    (error "overflow"))
  (vector-push (ldb (byte 8 24) i4) buf)
  (vector-push (ldb (byte 8 16) i4) buf)
  (vector-push (ldb (byte 8 8)  i4) buf)
  (vector-push (ldb (byte 8 0)  i4) buf))

(defmethod print-object ((r record) stream)
  (print-unreadable-object (r stream :type t)
    (multiple-value-bind (second minute hour date month year) (decode-universal-time (record-value r))
      (format stream "~2,'0d:~2,'0d:~2,'0d ~2,'0d/~2,'0d/~S" hour minute second date month year))))

(defmethod write-record ((r record) buf)
  (seq-write-i4 buf (record-value r)))

(defun read-record (buf)
  (let ((rc (make-record)))
    (setf (record-value rc) (seq-read-i4 buf))
    rc))
