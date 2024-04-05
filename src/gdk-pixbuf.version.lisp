;;; ----------------------------------------------------------------------------
;;; gdk-pixbuf.version.lisp
;;;
;;; The documentation of this file is taken from the GDK-PixBuf Reference Manual
;;; Version 2.42 and modified to document the Lisp binding to the GDK-PixBuf
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2012 - 2024 Dieter Kaiser
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
;;;
;;; Initialization and Versions
;;;
;;;      Library version numbers
;;;
;;; Types and Values
;;;
;;;     gdk_pixbuf_version
;;;     gdk_pixbuf_major_version
;;;     gdk_pixbuf_minor_version
;;;     gdk_pixbuf_micro_version
;;; ----------------------------------------------------------------------------

(in-package :gdk-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_version
;;; ----------------------------------------------------------------------------

(cffi:defcvar ("gdk_pixbuf_version" +version+ :read-only t) :string)

#+liber-documentation
(setf (liber:alias-for-symbol '+version+)
      "Constant"
      (liber:symbol-documentation '+version+)
 "@version{2024-4-2}
  @begin{short}
    Contains the full version of the GDK-Pixbuf library as a string.
  @end{short}
  This is the version currently in use by a running program.
  @see-symbol{gdk-pixbuf:+major-version+}
  @see-symbol{gdk-pixbuf:+minor-version+}
  @see-symbol{gdk-pixbuf:+micro-version+}")

(export '+version+)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_major_version
;;; ----------------------------------------------------------------------------

(cffi:defcvar ("gdk_pixbuf_major_version" +major-version+ :read-only t) :uint)

#+liber-documentation
(setf (liber:alias-for-symbol '+major-version+)
      "Constant"
      (liber:symbol-documentation '+major-version+)
 "@version{2024-4-2}
  @begin{short}
    The major version number of the GDK-Pixbuf library.
  @end{short}
  E.g. in GDK-Pixbuf version 2.42.8 this is 2. This variable is in the library,
  so represents the GDK-Pixbuf library you have loaded.
  @see-symbol{gdk-pixbuf:+version+}
  @see-symbol{gdk-pixbuf:+minor-version+}
  @see-symbol{gdk-pixbuf:+micro-version+}")

(export '+major-version+)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_minor_version
;;; ----------------------------------------------------------------------------

(cffi:defcvar ("gdk_pixbuf_minor_version" +minor-version+ :read-only t) :uint)

#+liber-documentation
(setf (liber:alias-for-symbol '+minor-version+)
      "Constant"
      (liber:symbol-documentation '+minor-version+)
 "@version{2024-4-2}
  @begin{short}
    The minor version number of the GDK-Pixbuf library.
  @end{short}
  E.g. in GDK-Pixbuf version 2.42.8 this is 42. This variable is in the library,
  so represents the GDK-Pixbuf library you have loaded.
  @see-symbol{gdk-pixbuf:+version+}
  @see-symbol{gdk-pixbuf:+major-version+}
  @see-symbol{gdk-pixbuf:+micro-version+}")

(export '+minor-version+)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_micro_version
;;; ----------------------------------------------------------------------------

(cffi:defcvar ("gdk_pixbuf_micro_version" +micro-version+ :read-only t) :uint)

#+liber-documentation
(setf (liber:alias-for-symbol '+micro-version+)
      "Constant"
      (liber:symbol-documentation '+micro-version+)
 "@version{2024-4-2}
  @begin{short}
    The micro version number of the GDK-Pixbuf library.
  @end{short}
  E.g. in GDK-Pixbuf version 2.42.8 this is 8. This variable is in the library,
  so represents the GDK-Pixbuf library you have loaded.
  @see-symbol{gdk-pixbuf:+version+}
  @see-symbol{gdk-pixbuf:+major-version+}
  @see-symbol{gdk-pixbuf:+minor-version+}")

(export '+micro-version+)

;;; --- End of file gdk-pixbuf.version.lisp ------------------------------------
