(defpackage :gdk-pixbuf-test
  (:use :fiveam :common-lisp)
  (:import-from :gio)
  (:export #:run!
           #:gdk-pixbuf-suite))

(in-package :gdk-pixbuf-test)

(def-suite gdk-pixbuf-suite)
(in-suite gdk-pixbuf-suite)

;; Ensure directory for the output of test results
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ensure-directories-exist
      (asdf:system-relative-pathname :cl-cffi-gdk-pixbuf "test/out/")))

;; Get the pathname for a file in the testsuite
(defun sys-path (filename &optional (system :cl-cffi-gdk-pixbuf))
  (asdf:system-relative-pathname system
                                 (concatenate 'string "test/" filename)))

;; A sorted list of the class property names without inherited properties
(defun list-properties (gtype)
  (sort (set-difference (mapcar #'g:param-spec-name
                                (g:object-class-list-properties gtype))
                        (mapcar #'g:param-spec-name
                                (g:object-class-list-properties
                                  (g:type-parent gtype)))
                        :test #'string=)
        #'string<))

;;; --- 2023-5-5 ---------------------------------------------------------------
