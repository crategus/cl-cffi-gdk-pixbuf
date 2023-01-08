(defpackage :gdk-pixbuf-test
  (:use :fiveam :cffi :common-lisp)
  (:export #:run!
           #:gdk-pixbuf-suite
))

(in-package :gdk-pixbuf-test)

(def-suite gdk-pixbuf-suite)
(in-suite gdk-pixbuf-suite)

(defun sys-path (filename &optional (package :cl-cffi-gdk-pixbuf))
  (let ((system-path (asdf:system-source-directory package)))
    (princ-to-string (merge-pathnames filename system-path))))

;; A sorted list of the class property names without inherited properties
(defun list-class-property-names (gtype)
  (sort (set-difference (mapcar #'g:param-spec-name
                                (g:object-class-list-properties gtype))
                        (mapcar #'g:param-spec-name
                                (g:object-class-list-properties
                                  (g:type-parent gtype)))
                        :test #'string=)
        #'string<))

;;; 2022-11-28
