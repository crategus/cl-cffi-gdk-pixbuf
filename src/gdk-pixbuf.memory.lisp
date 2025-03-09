;;; ----------------------------------------------------------------------------
;;; gdk-pixbuf.memory.lisp
;;;
;;; The documentation in this file is taken from the GDK-PixBuf Reference Manual
;;; Version 2.42 and modified to document the Lisp binding to the GDK-PixBuf
;;; library, see <http://www.gtk.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
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
;;;
;;; Image Data in Memory
;;;
;;;     Creating a pixbuf from image data that is already in memory.
;;;
;;; Functions
;;;
;;;     gdk_pixbuf_new
;;;     gdk_pixbuf_new_from_bytes
;;;     gdk_pixbuf_new_from_data
;;;     gdk_pixbuf_new_from_xpm_data
;;;     gdk_pixbuf_new_from_inline                          Deprecated 2.32
;;;     gdk_pixbuf_new_subpixbuf
;;;     gdk_pixbuf_copy
;;; ----------------------------------------------------------------------------

(in-package :gdk-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_new" pixbuf-new) (g:object pixbuf)
 #+liber-documentation
 "@version{#2024-6-29}
  @argument[colorspace]{a @symbol{gdk-pixbuf:colorspace} value for the image}
  @argument[has-alpha]{a boolean whether the image should have transparency
    information}
  @argument[bits-per-sample]{an integer with number of bits per color sample}
  @argument[width]{an integer with the width of image in pixels, must be > 0}
  @argument[height]{an integer with the height of image in pixels, must be > 0}
  @begin{return}
    The newly created @class{gdk-pixbuf:pixbuf} object with a reference count
    of 1, or @code{nil} if not enough memory could be allocated for the image
    buffer.
  @end{return}
  @begin{short}
    Creates a new @class{gdk-pixbuf:pixbuf} object and allocates a buffer for
    it.
  @end{short}
  The buffer has an optimal rowstride. Note that the buffer is not cleared. You
  will have to fill it completely yourself.
  @see-class{gdk-pixbuf:pixbuf}
  @see-symbol{gdk-pixbuf:colorspace}"
  (colorspace colorspace)
  (has-alpha :boolean)
  (bits-per-sample :int)
  (width :int)
  (height :int))

(export 'pixbuf-new)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_new_from_bytes
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_new_from_bytes" pixbuf-new-from-bytes)
    (g:object pixbuf)
 "@version{#2024-6-29}
  @argument[data]{a @class{g:bytes} instance with the image data in 8-bit
    sample packed format}
  @argument[colorspace]{a @symbol{gdk-pixbuf:colorspace} value for the image}
  @argument[has-alpha]{a boolean whether the image should have transparency
    information}
  @argument[bits-per-sample]{an integer with number of bits per color sample}
  @argument[width]{an integer with the width of image in pixels, must be > 0}
  @argument[height]{an integer with the height of image in pixels, must be > 0}
  @argument[rowstride]{an integer with the distance in bytes between row starts}
  @begin{return}
    The newly created @class{gdk-pixbuf:pixbuf} object with a reference count
    of 1.
  @end{return}
  @begin{short}
    Creates a new @class{gdk-pixbuf:pixbuf} object out of in-memory readonly
    image data.
  @end{short}
  Currently only RGB images with 8 bits per sample are supported.
  @see-class{gdk-pixbuf:pixbuf}
  @see-class{g:bytes}
  @see-symbol{gdk-pixbuf:colorspace}"
  (data (g:boxed g:bytes))
  (colorspace colorspace)
  (has-alpha :boolean)
  (bits-per-sample :int)
  (width :int)
  (height :int)
  (rowstride :int))

(export 'pixbuf-new-from-bytes)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_new_from_data ()
;;;
;;; GdkPixbuf * gdk_pixbuf_new_from_data (const guchar *data,
;;;                                       GdkColorspace colorspace,
;;;                                       gboolean has_alpha,
;;;                                       int bits_per_sample,
;;;                                       int width,
;;;                                       int height,
;;;                                       int rowstride,
;;;                                       GdkPixbufDestroyNotify destroy_fn,
;;;                                       gpointer destroy_fn_data);
;;;
;;; Creates a new GdkPixbuf out of in-memory image data. Currently only RGB
;;; images with 8 bits per sample are supported.
;;;
;;; data :
;;;     Image data in 8-bit/sample packed format.
;;;
;;; colorspace :
;;;     Colorspace for the image data
;;;
;;; has_alpha :
;;;     Whether the data has an opacity channel
;;;
;;; bits_per_sample :
;;;     Number of bits per sample
;;;
;;; width :
;;;     Width of the image in pixels, must be > 0
;;;
;;; height :
;;;     Height of the image in pixels, must be > 0
;;;
;;; rowstride :
;;;     Distance in bytes between row starts
;;;
;;; destroy_fn :
;;;     Function used to free the data when the pixbuf's reference count drops
;;;     to zero, or NULL if the data should not be freed
;;;
;;; destroy_fn_data :
;;;     Closure data to pass to the destroy notification function
;;;
;;; Returns :
;;;     A newly created GdkPixbuf structure with a reference count of 1.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_new_from_xpm_data ()
;;;
;;; GdkPixbuf * gdk_pixbuf_new_from_xpm_data (const char **data);
;;;
;;; Creates a new pixbuf by parsing XPM data in memory. This data is commonly
;;; the result of including an XPM file into a program's C source.
;;;
;;; data :
;;;     pointer to inline XPM data
;;;
;;; Returns :
;;;     A newly created pixbuf with a reference count of 1.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_new_from_inline ()                           Deprecated 2.32
;;;
;;; GdkPixbuf * gdk_pixbuf_new_from_inline (gint data_length,
;;;                                         const guint8 *data,
;;;                                         gboolean copy_pixels,
;;;                                         GError **error);
;;;
;;; Warning :
;;;
;;;     gdk_pixbuf_new_from_inline has been deprecated since version 2.32 and
;;;     should not be used in newly written code.
;;;
;;;     Use GResource instead.
;;;
;;; Create a GdkPixbuf from a flat representation that is suitable for storing
;;; as inline data in a program. This is useful if you want to ship a program
;;; with images, but don't want to depend on any external files.
;;;
;;; gdk-pixbuf ships with a program called gdk-pixbuf-csource which allows for
;;; conversion of GdkPixbufs into such a inline representation. In almost all
;;; cases, you should pass the --raw flag to gdk-pixbuf-csource. A sample
;;; invocation would be:
;;;
;;;     gdk-pixbuf-csource --raw --name=myimage_inline myimage.png
;;;
;;; For the typical case where the inline pixbuf is read-only static data, you
;;; don't need to copy the pixel data unless you intend to write to it, so you
;;; can pass FALSE for copy_pixels. (If you pass --rle to gdk-pixbuf-csource, a
;;; copy will be made even if copy_pixels is FALSE, so using this option is
;;; generally a bad idea.)
;;;
;;; If you create a pixbuf from const inline data compiled into your program,
;;; it's probably safe to ignore errors and disable length checks, since things
;;; will always succeed:
;;;
;;;     pixbuf = gdk_pixbuf_new_from_inline (-1, myimage_inline, FALSE, NULL);
;;;
;;; For non-const inline data, you could get out of memory. For untrusted inline
;;; data located at runtime, you could have corrupt inline data in addition.
;;;
;;; data_length :
;;;     Length in bytes of the data argument or -1 to disable length checks
;;;
;;; data :
;;;     Byte data containing a serialized GdkPixdata structure
;;;
;;; copy_pixels :
;;;     Whether to copy the pixel data, or use direct pointers data for the
;;;     resulting pixbuf
;;;
;;; error :
;;;     GError return location, may be NULL to ignore errors
;;;
;;; Returns :
;;;     A newly created GdkPixbuf structure with a reference, count of 1, or
;;;     NULL if an error occurred.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_new_subpixbuf
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_new_subpixbuf" pixbuf-new-subpixbuf)
    (g:object pixbuf)
 #+liber-documentation
 "@version{#2024-6-29}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object}
  @argument[x]{an integer with the x coord in @arg{pixbuf}}
  @argument[y]{an integer with the y coord in @arg{pixbuf}}
  @argument[width]{an integer with the width of region in @arg{pixbuf}}
  @argument[height]{an integer with the height of region in @arg{pixbuf}}
  @return{The new @class{gdk-pixbuf:pixbuf} object.}
  @begin{short}
    Creates a new pixbuf which represents a sub-region of @arg{pixbuf}.
  @end{short}
  The new pixbuf shares its pixels with the original pixbuf, so writing to one
  affects both. The new pixbuf holds a reference to @arg{pixbuf}, so
  @arg{pixbuf} will not be finalized until the new pixbuf is finalized.
  @see-class{gdk-pixbuf:pixbuf}"
  (pixbuf (g:object pixbuf))
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(export 'pixbuf-new-subpixbuf)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_copy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_copy" pixbuf-copy) (g:object pixbuf)
 #+liber-documentation
 "@version{#2024-6-29}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object}
  @begin{return}
    The newly created pixbuf with a reference count of 1, or @code{nil} if not
    enough memory could be allocated.
  @end{return}
  @begin{short}
    Creates a new @class{gdk-pixbuf:pixbuf} object with a copy of the
    information in the specified @arg{pixbuf}.
  @end{short}
  @see-class{gdk-pixbuf:pixbuf}"
  (pixbuf (g:object pixbuf)))

(export 'pixbuf-copy)

;;; --- End of file gdk-pixbuf.memory.lisp -------------------------------------
