;; pager-test.lisp
(in-package :pager-test)

(def-suite pager)

(in-suite pager)
(test page-1-test
  (open-pager "pager-test" 512)
  (with-out-page 1
    (loop for i from 1 to (/ 512 4) do
      (page-write-i4 i)))
  (with-in-page 1
    (loop for i from 1 to (/ 512 4) do
      (is (= i (page-read-i4)))))
  (close-pager :delete t))

(test page-10-test
  (open-pager "pager-test" 512)
  (with-out-page 10
    (loop for i from 1 to (/ 512 4) do
      (page-write-i4 i)))
  (with-in-page 10
    (loop for i from 1 to (/ 512 4) do
      (is (= i (page-read-i4)))))
  (close-pager :delete t))

(test page-10-persistent-test
  (open-pager "pager-test" 512)
  (with-out-page 10
    (loop for i from 1 to (/ 512 4) do
      (page-write-i4 i)))
  (close-pager)
  (open-pager "pager-test")
  (with-in-page 10
    (loop for i from 1 to (/ 512 4) do
      (is (= i (page-read-i4)))))
  (close-pager :delete t))

#+nil
(setf *run-test-when-defined* t)
#+nil
(setf *debug-on-error* t)
