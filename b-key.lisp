;;; b-key
(in-package :b-tree)

(defclass b-key ()
  ((key
    :type fixnum
    :initarg :key
    :accessor b-key
    :documentation "The key value")
   (record-ptr
    :type (or fixnum null)
    :initarg :record-ptr
    :accessor b-record-ptr
    :documentation "Pointer to the page containing the key record")
   (pred-ptr
    :initform nil
    :type (or fixnum null)
    :initarg :pred-ptr
    :accessor b-pred-ptr
    :documentation "Pointer to b-node page with records lesser than key"))
  (:documentation "B-tree key that contains the pointer to value and predecessor records"))

(defmacro for-keys ((key node) &body body)
  (let ((index) (key-var key))
    (when (listp key)
      (setf index   (cadr key)
            key-var (car  key)))
    `(loop ,@(if (listp key) `(:for ,index :from 0))
           :for ,key-var :across (node-keys ,node) :do
           ,@body)))

(defmethod make-b-key (key)
  (make-instance 'b-key :key key :record-ptr nil))

(defmethod key> ((a b-key) (b b-key))
  (> (b-key a) (b-key b)))

(defmethod key< ((a b-key) (b b-key))
  (< (b-key a) (b-key b)))

(defmethod key= ((a b-key) (b b-key))
  (= (b-key a) (b-key b)))

(defmethod key/= ((a b-key) (b b-key))
  (not (key= a b)))

(defmethod print-object ((key b-key) stream)
  (print-unreadable-object (key stream :type t)
    (prin1 (b-key key) stream)))

(defclass b-key-ref ()
  ((node
    :type b-node
    :initarg :node
    :reader  ref-node)
   (index
    :type fixnum
    :initarg :index
    :reader  ref-index))
  (:documentation "Used to address b-key in context of node"))

(defmethod ref-key ((ref b-key-ref))
  (aref (node-keys (ref-node ref)) (ref-index ref)))

(defmethod ref-succession-p ((ref b-key-ref))
  "Check if reference points to the succession pointer"
  (= (ref-index ref) (node-keys-count (ref-node ref))))

(defun make-key-ref (node index)
  (make-instance 'b-key-ref :node node :index index))

(defmethod print-object ((ref b-key-ref) stream)
  (print-unreadable-object (ref stream :type t)
    (format stream "~A@~A"
            (ref-index ref)
            (ref-node ref))))

(defmethod ref-key-ptr ((ref b-key-ref))
  "Return the referenced pointer."
  (if (ref-succession-p ref)
      (node-succ-ptr (ref-node ref))
      (b-pred-ptr (ref-key ref))))
