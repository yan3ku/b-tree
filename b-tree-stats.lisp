(in-package :b-tree)

(defclass stat-mixin ()
  ((compensation-count
    :type fixnum
    :initform 0
    :accessor compensation-count)
   (split-count
    :type fixnum
    :initform 0
    :accessor split-count)
   (merge-count
    :type fixnum
    :initform 0
    :accessor merge-count))
  (:documentation "Track statistics."))

(defmethod show-stats ((object stat-mixin) (stream t))
  (let ((slots (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots (class-of (make-instance 'stat-mixin))))))
    (format stream "~&STAT:~%")
    (dolist (slot slots)
      (let ((slot-value (slot-value object slot)))
        (format stream " ~A: ~A~%" slot slot-value)))))

(defmethod b-node-distribute :after ((tree b-tree) mid-ref lt rt)
  (incf (compensation-count tree)))

(defmethod b-tree-split-node :after ((tree b-tree) parent to-split)
  (incf (split-count tree)))

(defmethod b-tree-merge :after ((tree b-tree) parent to-split)
  (incf (merge-count tree)))
