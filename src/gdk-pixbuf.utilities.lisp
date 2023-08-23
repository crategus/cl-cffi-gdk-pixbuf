;;; ----------------------------------------------------------------------------
;;; gdk-pixbuf.utilities.lisp
;;;
;;; The documentation of this file is taken from the GDK-PixBuf Reference Manual
;;; Version 2.36 and modified to document the Lisp binding to the GDK-PixBuf
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2013 - 2023 Dieter Kaiser
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
;;; Utilities
;;;
;;;     Utility and miscellaneous convenience functions.
;;;
;;; Functions
;;;
;;;     gdk_pixbuf_add_alpha
;;;     gdk_pixbuf_copy_area
;;;     gdk_pixbuf_saturate_and_pixelate
;;;     gdk_pixbuf_apply_embedded_orientation
;;;     gdk_pixbuf_fill
;;;
;;; Description
;;;
;;; These functions provide miscellaneous utilities for manipulating pixbufs.
;;; The pixel data in pixbufs may of course be manipulated directly by
;;; applications, but several common operations can be performed by these
;;; functions instead.
;;; ----------------------------------------------------------------------------

(in-package :gdk-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_add_alpha ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_add_alpha" pixbuf-add-alpha) (g:object pixbuf)
 #+liber-documentation
 "@version{#2021-12-12}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object}
  @argument[substitute]{a boolean whether to set a color to zero opacity, if
    this is @em{false}, then the (@arg{red}, @arg{green}, @arg{blue}) arguments
    will be ignored}
  @argument[red]{an unsigned char with the red value to substitute}
  @argument[green]{an unsigned char with the green value to substitute}
  @argument[blue]{an unsigned char with the blue value to substitute}
  @return{A newly created pixbuf with a reference count of 1.}
  @begin{short}
    Takes an existing @arg{pixbuf} and adds an alpha channel to it.
  @end{short}
  If the existing @arg{pixbuf} already had an alpha channel, the channel values
  are copied from the original; otherwise, the alpha channel is initialized to
  255 (full opacity).

  If the @arg{substitute} argument is @em{true}, then the color specified by
  (@arg{red}, @arg{green}, @arg{blue}) will be assigned zero opacity. That is,
  if you pass (255, 255, 255) for the substitute color, all white pixels will
  become fully transparent.
  @see-class{gdk-pixbuf:pixbuf}"
  (pixbuf (g:object pixbuf))
  (substitute :boolean)
  (red :uchar)
  (green :uchar)
  (blue :uchar))

(export 'pixbuf-add-alpha)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_copy_area ()
;;; ----------------------------------------------------------------------------

;; TODO: Change the implementation to return the dest pixbuf!?

(cffi:defcfun ("gdk_pixbuf_copy_area" pixbuf-copy-area) :void
 #+liber-documentation
 "@version{#2021-12-12}
  @argument[src]{a @class{gdk-pixbuf:pixbuf} object}
  @argument[xsrc]{an integer with the source x coordinate within @arg{src}}
  @argument[ysrc]{an integer with the source y coordinate within @arg{src}}
  @argument[width]{an integer with the width of the area to copy}
  @argument[height]{an integer with the height of the area to copy}
  @argument[dest]{a @class{gdk-pixbuf:pixbuf} destination object}
  @argument[xdest]{an integer with the x coordinate within @arg{dest}}
  @argument[ydest]{an integer with the y coordinate within @arg{dest}}
  @begin{short}
    Copies a rectangular area from @arg{src} to @arg{dest}.
  @end{short}
  Conversion of pixbuf formats is done automatically.

  If the source rectangle overlaps the destination rectangle on the same pixbuf,
  it will be overwritten during the copy operation. Therefore, you can not use
  this function to scroll a pixbuf.
  @see-class{gdk-pixbuf:pixbuf}"
  (src (g:object pixbuf))
  (xsrc :int)
  (ysrc :int)
  (width :int)
  (height :int)
  (dest (g:object pixbuf))
  (xdest :int)
  (ydest :int))

(export 'pixbuf-copy-area)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_saturate_and_pixelate ()
;;;
;;; void gdk_pixbuf_saturate_and_pixelate (const GdkPixbuf *src,
;;;                                        GdkPixbuf *dest,
;;;                                        gfloat saturation,
;;;                                        gboolean pixelate);
;;;
;;; Modifies saturation and optionally pixelates src, placing the result in
;;; dest. src and dest may be the same pixbuf with no ill effects. If saturation
;;; is 1.0 then saturation is not changed. If it's less than 1.0, saturation is
;;; reduced (the image turns toward grayscale); if greater than 1.0, saturation
;;; is increased (the image gets more vivid colors). If pixelate is TRUE, then
;;; pixels are faded in a checkerboard pattern to create a pixelated image. src
;;; and dest must have the same image format, size, and rowstride.
;;;
;;; src :
;;;     source image
;;;
;;; dest :
;;;     place to write modified version of src
;;;
;;; saturation :
;;;     saturation factor
;;;
;;; pixelate :
;;;     whether to pixelate
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_apply_embedded_orientation ()
;;;
;;; GdkPixbuf * gdk_pixbuf_apply_embedded_orientation (GdkPixbuf *src);
;;;
;;; Takes an existing pixbuf and checks for the presence of an associated
;;; "orientation" option, which may be provided by the jpeg loader (which reads
;;; the exif orientation tag) or the tiff loader (which reads the tiff
;;; orientation tag, and compensates it for the partial transforms performed by
;;; libtiff). If an orientation option/tag is present, the appropriate transform
;;; will be performed so that the pixbuf is oriented correctly.
;;;
;;; src :
;;;     A GdkPixbuf.
;;;
;;; Returns :
;;;     A newly-created pixbuf, or a reference to the input pixbuf (with an
;;;     increased reference count). [transfer full]
;;;
;;; Since 2.12
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_fill ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_fill" pixbuf-fill) :void
 #+liber-documentation
 "@version{#2021-12-12}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object}
  @argument[pixel]{an unsigned integer with the RGBA pixel to clear to,
    @code{#xffffffff} is opaque white, @code{#x00000000} transparent black}
  @begin{short}
    Clears a pixbuf to the given RGBA value, converting the RGBA value
    into the pixel format of the pixbuf.
  @end{short}
  The alpha will be ignored if the pixbuf does not have an alpha channel.
  @see-class{gdk-pixbuf:pixbuf}"
  (pixbuf (g:object pixbuf))
  (pixel :uint32))

(export 'pixbuf-fill)

;;; --- End of file gdk-pixbuf.utilities.lisp ----------------------------------
