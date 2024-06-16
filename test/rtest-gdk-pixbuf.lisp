(defpackage :gdk-pixbuf-test
  (:use :fiveam :common-lisp)
  (:import-from :gio)
  (:export #:run!
           #:gdk-pixbuf-test))

(in-package :gdk-pixbuf-test)

(def-suite gdk-pixbuf-test)
(in-suite gdk-pixbuf-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (glib-sys:get-current-package) "cl-cffi-gdk-pixbuf")
  (glib-sys:check-and-create-resources "test/rtest-gdk-pixbuf.gresource.xml"
                                       :package "cl-cffi-gdk-pixbuf"
                                       :sourcedir "test/resource/"
                                       :verbose t)
  ;; Ensure directory for the output of test results
  (ensure-directories-exist
      (asdf:system-relative-pathname :cl-cffi-gdk-pixbuf "test/out/")))

;;; 2024-6-16
