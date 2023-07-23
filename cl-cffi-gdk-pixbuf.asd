;;; ----------------------------------------------------------------------------
;;; cl-cffi-gdk-pixbuf.asd
;;;
;;; Copyright (C) 2011 - 2022 Dieter Kaiser
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
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------

(defsystem :cl-cffi-gdk-pixbuf
  :name "cl-cffi-gdk-pixbuf"
  :version "0.3.0"
  :author "Dieter Kaiser"
  :license "MIT"
  :components
  ((:module src
    :serial t
    :components
    ((:file "gdk-pixbuf.package")
     (:file "gdk-pixbuf.init")
     (:file "gdk-pixbuf.version")   ; Library version numbers
     (:file "gdk-pixbuf.structure") ; Implementation of Pixbuf
     (:file "gdk-pixbuf.interface") ; Extending GdkPixBuf
     (:file "gdk-pixbuf.load")      ; Loading a Pixbuf
     (:file "gdk-pixbuf.save")      ; Saving a Pixbuf
     (:file "gdk-pixbuf.memory")    ; Image Data in Memory
     (:file "gdk-pixbuf.scaling")   ; Scaling pixbufs
     (:file "gdk-pixbuf.utilities") ; Utility functions
     (:file "gdk-pixbuf.animation") ; Animated images
     (:file "gdk-pixbuf.loader")    ; Progressive image loading
    )))
  :in-order-to ((asdf:test-op (test-op "cl-cffi-gdk-pixbuf/test")))
  :depends-on (:cl-cffi-glib
               :cffi))

;; Definine a test operation for the library

(defsystem :cl-cffi-gdk-pixbuf/test
  :name "cl-cffi-gdk-pixbuf/test"
  :depends-on (:cl-cffi-gdk-pixbuf :fiveam)
  :perform (test-op (o c)
               (uiop:symbol-call :fiveam :run!
                                 (uiop:find-symbol* :gdk-pixbuf-suite
                                                    :gdk-pixbuf-test)))
  :pathname "test/"
  :serial t
  :components ((:file "rtest-gdk-pixbuf")
               (:file "rtest-gdk-pixbuf-version")
               (:file "rtest-gdk-pixbuf-structures")

               (:file "rtest-gdk-pixbuf-interface")
               (:file "rtest-gdk-pixbuf-load")

              ))

;;; --- End of file cl-cffi-gdk-pixbuf.asd ---------------------------------
