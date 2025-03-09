;;; ----------------------------------------------------------------------------
;;; gdk-pixbuf.init.lisp
;;;
;;; Copyright (C) 2011 - 2025 Dieter Kaiser
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------

(in-package :gdk-pixbuf)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; push the hostname on *features*
  (pushnew (intern (string-upcase (machine-instance)) :keyword) *features*)
  (pushnew :gdk-pixbuf *features*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library gdk-pixbuf
    ((:and :unix (:not :darwin))
     (:or "libgdk_pixbuf-2.0.so.0" "libgdk_pixbuf-2.0.so"))
     (:darwin (:or "libgdk_pixbuf-2.0.0.dylib" "libgdk_pixbuf-2.0.dylib"))
     (:windows (:or "libgdk_pixbuf-win32-2.0-0" "libgdk_pixbuf-2.0-0.dll"))
     (t "libgdk_pixbuf-2.0"))

  (cffi:use-foreign-library gdk-pixbuf))

(cffi:defcvar ("gdk_pixbuf_major_version" +major-version+ :read-only t) :uint)
(cffi:defcvar ("gdk_pixbuf_minor_version" +minor-version+ :read-only t) :uint)

(glib-init:push-library-version-features
  gdk-pixbuf
  +major-version+ +minor-version+
  2 38  ; Since 01-09-2018
  2 40  ; Since 08-10-2019
  2 42  ; Since 09-11-2020
)

(glib-init:require-library-version
  "gdk-pixbuf"
  2 38  ; Since 01-09-2018
  +major-version+
  +minor-version+)

;;; End of file gdk-pixbuf.init.lisp -------------------------------------------
