;; pager-test.lisp
(in-package :pager-test)

(def-suite pager)

(in-suite pager)
(test page-1-test
  (let ((p (open-pager "pager-test" 512)))
    (with-out-page p 1
      (loop for i from 1 to (/ 512 4) do
        (page-write-i4 i)))
    (with-in-page p 1
      (loop for i from 1 to (/ 512 4) do
        (is (= i (page-read-i4)))))
    (close-pager p :delete t)))

(test page-10-test
  (let ((p (open-pager "pager-test" 512)))
    (with-out-page p 10
      (loop for i from 1 to (/ 512 4) do
        (page-write-i4 i)))
    (with-in-page p 10
      (loop for i from 1 to (/ 512 4) do
        (is (= i (page-read-i4)))))
    (close-pager p :delete t)))

(test page-10-persistent-test
  (let ((p (open-pager "pager-test" 512)))
    (with-out-page p 10
      (loop for i from 1 to (/ 512 4) do
        (page-write-i4 i)))
    (close-pager p)
    (setf p (open-pager "pager-test"))
    (with-in-page p 10
      (loop for i from 1 to (/ 512 4) do
        (is (= i (page-read-i4)))))
    (close-pager p :delete t)))

#+nil
(setf *run-test-when-defined* t)
#+nil
(setf *debug-on-error* t)
