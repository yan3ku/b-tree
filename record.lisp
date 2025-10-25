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

(defun read-i4 (in)
  (let ((i4 0))
    (declare (fixnum i4))
    (setf (ldb (byte 8 24) i4) (read-byte in))
    (setf (ldb (byte 8 16) i4) (read-byte in))
    (setf (ldb (byte 8 8)  i4) (read-byte in))
    (setf (ldb (byte 8 0)  i4) (read-byte in))
    i4))

(defun write-i4 (out i4)
  (declare (fixnum i4))
  (write-byte (ldb (byte 8 24) i4) out)
  (write-byte (ldb (byte 8 16) i4) out)
  (write-byte (ldb (byte 8 8)  i4) out)
  (write-byte (ldb (byte 8 0)  i4) out))

(defmethod print-object ((self record) stream)
  (print-unreadable-object (self stream :type t)
    (multiple-value-bind (second minute hour date month year) (decode-universal-time (record-value self))
      (format stream "~2,'0d:~2,'0d:~2,'0d ~2,'0d/~2,'0d/~S" hour minute second date month year))))

(defmethod write-record ((self record) out)
  (write-i4 out (record-value self)))

(defun read-record (in)
  (let ((rc (make-record)))
    (setf (record-value rc) (read-i4 in))
    rc))
