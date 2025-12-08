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
  (:documentation "B-tree key that contains the pointer to record and predecessor records"))

(defmethod replace-key ((k1 b-key) (k2 b-key))
  "Replace the key value and record pointer but leave the pred pointer. Used for deletion."
  (setf (b-key k1) (b-key k2))
  (setf (b-record-ptr k1) (b-record-ptr k2)))

(defmethod make-b-key (key)
  (make-instance 'b-key :key key :record-ptr nil))

(defmethod make-b-key-record (tree key rc)
  (make-instance 'b-key :key key :record-ptr (write-record tree rc (new-record-addr tree))))

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
    (prin1 (b-key key) stream)
    (format stream ", ")
    (prin1 (b-record-ptr key) stream)))

(defclass b-key-ref ()
  ((node
    :type b-node
    :initarg :node
    :reader  ref-node)
   (index
    :type fixnum
    :initarg :index
    :reader  ref-index))
  (:documentation "Used to address b-key in context of node. use ref-ptr for the ptr"))


(defmethod print-object ((ref b-key-ref) stream)
  (print-unreadable-object (ref stream :type t)
    (format stream "~A@~A"
            (ref-index ref)
            (ref-node ref))))

(defun ref-p (ref)
  (typep ref 'b-key-ref))

(defun make-ref (node index)
  (make-instance 'b-key-ref :node node :index index))

(defmethod ref-key ((ref b-key-ref))
  "Return the key value."
  (and (not (ref-node-succ-p ref))
       (aref (node-keys (ref-node ref)) (ref-index ref))))

(defmethod ref-ptr ((ref b-key-ref))
  "Return b-pred-ptr if reference points at b-key or node-succ-ptr when it's out of bounds."
  (if (ref-node-succ-p ref)
      (node-succ-ptr (ref-node ref))
      (b-pred-ptr (ref-key ref))))

(defmethod (setf ref-key) (new (ref b-key-ref))
  "Put the new key at the place referenced by ref."
  (setf (aref (node-keys (ref-node ref)) (ref-index ref)) new))

(defmethod ref-node-addr ((ref b-key-ref))
  (node-addr (ref-node ref)))

(defmethod ref-node-succ-p ((ref b-key-ref))
  "Check if reference points to the node succession pointer"
  (= (ref-index ref) (node-keys-count (ref-node ref))))

(defmethod ref-left ((ref b-key-ref))
  "Return reference to left neightboor of the ref (the smaller key ref)."
  (make-ref (ref-node ref)
            (1- (ref-index ref))))

(defmethod ref-right ((ref b-key-ref))
  "Return reference to right neightboor of the ref (the greater key ref)."
  (make-ref (ref-node ref)
            (1+ (ref-index ref))))

(defmethod ref-left-ptr ((ref b-key-ref))
  "The ref-ptr of left neightboor"
  (ref-ptr (ref-left ref)))

(defmethod ref-right-ptr ((ref b-key-ref))
  "The ref-ptr of right neighboor"
  (ref-ptr (ref-right ref)))

(defmethod (setf ref-ptr) (new (ref b-key-ref))
  "Set pointer to the lower node."
  (if (ref-node-succ-p ref)
      (setf (node-succ-ptr (ref-node ref)) new)
      (setf (b-pred-ptr (ref-key ref)) new)))

(defmethod (setf ref-right-ptr) (new (ref b-key-ref))
  "Set the right neighboor lower node pointer."
  (let ((succ (make-ref (ref-node ref) (1+ (ref-index ref)))))
    (setf (ref-ptr succ) new)))
