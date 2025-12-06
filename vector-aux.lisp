;;; vector-aux.lisp
(in-package :b-tree)

(defun vector-insert (vector index key)
  (incf (fill-pointer vector))
  (replace vector vector :start1 (1+ index) :start2 index)
  (setf (aref vector index) key)
  vector)

(defun vector-split-into-lmr (to-split into &optional point)
  (let ((split (or point (truncate (length to-split) 2))))
    (setf (fill-pointer into) (- (length to-split) split 1))
    (let ((mid (aref to-split split)))
      (replace into to-split :start2 (1+ split))
      (setf (fill-pointer to-split) split)
      mid)))

(defun vector-delete (vector index)
  (replace vector vector :end2 index)
  (replace vector vector :start1 index :start2 (1+ index))
  (decf (fill-pointer vector))
  vector)

(defun vector-merge (&rest to-merge)
  (make-array (reduce #'+ (mapcar #'length to-merge))
              :adjustable t
              :fill-pointer t
              :initial-contents (apply #'concatenate 'vector to-merge)))
