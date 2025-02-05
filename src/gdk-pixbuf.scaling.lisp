;;; ----------------------------------------------------------------------------
;;; gdk-pixbuf.scaling.lisp
;;;
;;; The documentation of this file is taken from the GDK-PixBuf Reference Manual
;;; Version 2.42 and modified to document the Lisp binding to the GDK-PixBuf
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp binding
;;; is available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2024 Dieter Kaiser
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
;;; Scaling
;;;
;;;     Scaling pixbufs and scaling and compositing pixbufs
;;;
;;; Types and Values
;;;
;;;     GdkInterpType
;;;     GdkPixbufRotation
;;;
;;; Functions
;;;
;;;     gdk_pixbuf_scale_simple
;;;     gdk_pixbuf_scale
;;;     gdk_pixbuf_composite_color_simple
;;;     gdk_pixbuf_composite
;;;     gdk_pixbuf_composite_color
;;;     gdk_pixbuf_rotate_simple
;;;     gdk_pixbuf_flip
;;; ----------------------------------------------------------------------------

(in-package :gdk-pixbuf)

;;; ----------------------------------------------------------------------------
;;; GdkInterpType
;;; ----------------------------------------------------------------------------

;; We change the name of the enumeration to pixbuf-interp-type.

(gobject:define-genum "GdkInterpType" pixbuf-interp-type
  (:export t
   :type-initializer "gdk_interp_type_get_type")
  (:nearest 0)
  (:tiles 1)
  (:bilinear 2)
  (:hyper 3))

#+liber-documentation
(setf (liber:alias-for-symbol 'pixbuf-interp-type)
      "GEnum"
      (liber:symbol-documentation 'pixbuf-interp-type)
 "@version{#2024-6-29}
  @begin{declaration}
(gobject:define-genum \"GdkInterpType\" pixbuf-interp-type
  (:export t
   :type-initializer \"gdk_interp_type_get_type\")
  (:nearest 0)
  (:tiles 0)
  (:bilinear 0)
  (:hyper 0))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:nearest]{Nearest neighbor sampling: This is the fastest and lowest
        quality mode. Quality is normally unacceptable when scaling down, but
        may be fine when scaling up.}
      @entry[:tiles]{This is an accurate simulation of the PostScript image
        operator without any interpolation enabled. Each pixel is rendered as a
        tiny parallelogram of solid color, the edges of which are implemented
        with antialiasing. It resembles nearest neighbor for enlargement, and
        bilinear for reduction.}
      @entry[:bilinear]{Bilinear interpolation: Best quality/speed balance. Use
        this mode by default. For enlargement, it is equivalent to
        point-sampling the ideal bilinear-interpolated image. For reduction, it
        is equivalent to laying down small tiles and integrating over the
        coverage area.}
      @entry[:hyper]{This is the slowest and highest quality reconstruction
        function. It is derived from the hyperbolic filters in Wolberg's
        \"Digital Image Warping\", and is formally defined as the
        hyperbolic-filter sampling the ideal hyperbolic-filter interpolated
        image. The filter is designed to be idempotent for 1:1 pixel mapping.
        @em{Deprecated:} This interpolation filter is deprecated since 2.38,
        as in reality it has a lower quality than the @code{:bilinear} filter.}
    @end{table}
  @end{values}
  @begin{short}
    This enumeration describes the different interpolation modes that can be
    used with the scaling functions.
  @end{short}
  The @code{:nearest} mode is the fastest scaling method, but has horrible
  quality when scaling down. The @code{:bilinear} mode is the best choice if
  you are not sure what to choose, it has a good speed/quality balance.
  @begin[Note]{dictionary}
    Cubic filtering is missing from the list. Hyperbolic interpolation is just
    as fast and results in higher quality.
  @end{dictionary}
  @see-class{gdk-pixbuf:pixbuf}")

;;; ----------------------------------------------------------------------------
;;; GdkPixbufRotation
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GdkPixbufRotation" pixbuf-rotation
  (:export t
   :type-initializer "gdk_pixbuf_rotation_get_type")
  (:none 0)
  (:counterclockwise 90)
  (:upsidedown 180)
  (:clockwise 270))

#+liber-documentation
(setf (liber:alias-for-symbol 'pixbuf-rotation)
      "GEnum"
      (liber:symbol-documentation 'pixbuf-rotation)
 "@version{#2024-6-29}
  @begin{declaration}
(gobject:define-genum \"GdkPixbufRotation\" pixbuf-rotation
  (:export t
   :type-initializer \"gdk_pixbuf_rotation_get_type\")
  (:none 0)
  (:counterclockwise 90)
  (:upsidedown 180)
  (:clockwise 270))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:none]{No rotation.}
      @entry[:counterclockwise]{Rotate by 90 degrees.}
      @entry[:upsidedown]{Rotate by 180 degrees.}
      @entry[:clockwise]{Rotate by 270 degrees.}
    @end{table}
  @end{values}
  @begin{short}
    The possible rotations which can be passed to the
    @fun{gdk-pixbuf:pixbuf-rotate-simple} function.
  @end{short}
  To make them easier to use, their numerical values are the actual degrees.
  @see-class{gdk-pixbuf:pixbuf}
  @see-function{gdk-pixbuf:pixbuf-rotate-simple}")

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_scale_simple
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_scale_simple" pixbuf-scale-simple) (g:object pixbuf)
 #+liber-documentation
 "@version{#2024-6-29}
  @argument[src]{a @class{gdk-pixbuf:pixbuf} object}
  @argument[width]{an integer with the width of destination image}
  @argument[height]{an integer with the height of destination image}
  @argument[interp]{a @symbol{gdk-pixbuf:pixbuf-interp-type} interpolation type
    for the transformation}
  @begin{return}
    The new @class{gdk-pixbuf:pixbuf} object, or @code{nil} if not enough
    memory could be allocated for it.
  @end{return}
  @begin{short}
    Create a new @class{gdk-pixbuf:pixbuf} object containing a copy of @arg{src}
    scaled to @arg{width} @code{x} @arg{height}.
  @end{short}
  Leaves @arg{src} unaffected. The @arg{interp} mode should be @code{:nearest}
  if you want maximum speed, but when scaling down the @code{:nearest} mode is
  usually unusably ugly. The default @arg{interp} mode should be
  @code{:bilinear} which offers reasonable quality and speed.

  You can scale a sub-portion of @arg{src} by creating a sub-pixbuf pointing
  into @arg{src}, see the @fun{gdk-pixbuf:pixbuf-new-subpixbuf} function.

  For more complicated scaling/compositing see the @fun{gdk-pixbuf:pixbuf-scale}
  and @fun{gdk-pixbuf:pixbuf-composite} functions.
  @see-class{gdk-pixbuf:pixbuf}
  @see-symbol{gdk-pixbuf:pixbuf-interp-type}
  @see-function{gdk-pixbuf:pixbuf-new-subpixbuf}
  @see-function{gdk-pixbuf:pixbuf-scale}
  @see-function{gdk-pixbuf:pixbuf-composite}"
  (src (g:object pixbuf))
  (width :int)
  (height :int)
  (interp pixbuf-interp-type))

(export 'pixbuf-scale-simple)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_scale
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_scale" %pixbuf-scale) :void
  (src (g:object pixbuf))
  (dest (g:object pixbuf))
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (xoffset :double)
  (yoffset :double)
  (xscale :double)
  (yscale :double)
  (interp pixbuf-interp-type))

(defun pixbuf-scale (src dest
                     x y width height
                     xoffset yoffset
                     xscale yscale
                     interp)
 #+liber-documentation
 "@version{#2024-6-29}
  @argument[src]{a @class{gdk-pixbuf:pixbuf} object}
  @argument[dest]{a @class{gdk-pixbuf:pixbuf} object into which to render the
    results}
  @argument[x]{an integer with the left coordinate for region to render}
  @argument[y]{an integer with the top coordinate for region to render}
  @argument[width]{an integer with the width of the region to render}
  @argument[height]{an integer with the height of the region to render}
  @argument[xoffset]{a number coerced to a double float with the offset in the
    x direction, currently rounded to an integer}
  @argument[yoffset]{a number coerce to a double float with the offset in the
    y direction, currently rounded to an integer}
  @argument[xscale]{a number coerced to a double float with the scale factor in
    the x direction}
  @argument[yscale]{a number coerced to a double float with the scale factor
    in the y direction}
  @argument[interp]{a @symbol{gdk-pixbuf:pixbuf-interp-type} interpolation type
    for the transformation}
  @begin{short}
    Creates a transformation of the source image @arg{src} by scaling by
    @arg{xscale} and @arg{yscale} then translating by @arg{xoffset} and
    @arg{yoffset}, then renders the rectangle @code{(x,y,width,height)} of the
    resulting image onto the destination image replacing the previous contents.
  @end{short}

  Try to use the @fun{gdk-pixbuf:pixbuf-scale-simple} function first, this
  function is the industrial-strength power tool you can fall back to if the
  @fun{gdk-pixbuf:pixbuf-scale-simple} function is not powerful enough.

  If the source rectangle overlaps the destination rectangle on the same
  pixbuf, it will be overwritten during the scaling which results in rendering
  artifacts.
  @see-class{gdk-pixbuf:pixbuf}
  @see-symbol{gdk-pixbuf:pixbuf-interp-type}
  @see-function{gdk-pixbuf:pixbuf-scale-simple}"
  (%pixbuf-scale src dest
                 x y width height
                 (coerce xoffset 'double-float)
                 (coerce yoffset 'double-float)
                 (coerce xscale 'double-float)
                 (coerce yscale 'double-float)
                 interp))

(export 'pixbuf-scale)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_composite_color_simple
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_composite_color_simple"
               pixbuf-composite-color-simple) (g:object pixbuf)
 #+liber-documentation
 "@version{#2024-6-29}
  @argument[src]{a @class{gdk-pixbuf:pixbuf} object}
  @argument[width]{an integer with the width of destination image}
  @argument[height]{an integer with the height of destination image}
  @argument[interp]{a @symbol{gdk-pixbuf:pixbuf-interp-type} interpolation type
    for the transformation}
  @argument[alpha]{an integer with the overall alpha for source image (0..255)}
  @argument[size]{an integer with the size of checks in the checkboard, must be
    a power of two}
  @argument[color1]{an unsigned integer with the color of check at upper left}
  @argument[color2]{an unsigned integer with the color of the other check}
  @begin{return}
    The new @class{gdk-pixbuf:pixbuf} object, or @code{nil} if not enough memory
    could be allocated for it.
  @end{return}
  @begin{short}
    Creates a new @class{gdk-pixbuf:pixbuf} object by scaling @arg{src} to
    @arg{width} @code{x} @arg{height} and compositing the result with a
    checkboard of colors @arg{color1} and @arg{color2}.
  @end{short}
  @see-class{gdk-pixbuf:pixbuf}
  @see-symbol{gdk-pixbuf:pixbuf-interp-type}
  @see-function{gdk-pixbuf:pixbuf-composite-color}"
  (src (g:object pixbuf))
  (width :int)
  (height :int)
  (interp pixbuf-interp-type)
  (alpha :int)
  (size :int)
  (color1 :uint32)
  (color2 :uint32))

(export 'pixbuf-composite-color-simple)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_composite
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_composite" %pixbuf-composite) :void
  (src (g:object pixbuf))
  (dest (g:object pixbuf))
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (xoffset :double)
  (yoffset :double)
  (xscale :double)
  (yscale :double)
  (interp pixbuf-interp-type)
  (alpha :int))

(defun pixbuf-composite (src dest
                         x y width height
                         xoffset yoffset
                         xscale yscale
                         interp
                         alpha)
#+liber-documentation
 "@version{#2024-6-29}
  @argument[src]{a @class{gdk-pixbuf:pixbuf} object}
  @argument[dest]{a @class{gdk-pixbuf:pixbuf} object into which to render the
    results}
  @argument[x]{an integer with the left coordinate for region to render}
  @argument[y]{an integer with the top coordinate for region to render}
  @argument[width]{an integer with the width of the region to render}
  @argument[height]{an integer with the height of the region to render}
  @argument[xoffset]{a number coerced to a double float with the offset in
    the x direction, currently rounded to an integer}
  @argument[yoffset]{a number coerced to a double float with the offset in
    the y direction, currently rounded to an integer}
  @argument[xscale]{a number coerced to a double float with the scale factor in
    the x direction}
  @argument[yscale]{a number coerced to a double float with the scale factor in
    the y direction}
  @argument[interp]{a @symbol{gdk-pixbuf:pixbuf-interp-type} interpolation type
    for the transformation}
  @argument[alpha]{an integer with the overall alpha for source image (0..255)}
  @begin{short}
    Creates a transformation of the source image @arg{src} by scaling by
    @arg{xscale} and @arg{yscale} then translating by @arg{xoffset} and
    @arg{yoffset}.
  @end{short}
  This gives an image in the coordinates of the destination pixbuf. The
  rectangle @code{(x,y,width,height)} is then composited onto the corresponding
  rectangle of the original destination image.

  When the destination rectangle contains parts not in the source image, the
  data at the edges of the source image is replicated to infinity.

  @image[composite]{Figure: Composite}

  @see-class{gdk-pixbuf:pixbuf}
  @see-symbol{gdk-pixbuf:pixbuf-interp-type}
  @see-function{gdk-pixbuf:pixbuf-composite-color}
  @see-function{gdk-pixbuf:pixbuf-composite-color-simple}"
  (%pixbuf-composite src dest
                     x y width height
                     (coerce xoffset 'double-float)
                     (coerce yoffset 'double-float)
                     (coerce xscale 'double-float)
                     (coerce yscale 'double-float)
                     interp
                     alpha))

(export 'pixbuf-composite)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_composite_color
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_composite_color" %pixbuf-composite-color)
    (g:object pixbuf)
  (src (g:object pixbuf))
  (dest (g:object pixbuf))
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (xoffset :double)
  (yoffset :double)
  (xscale :double)
  (yscale :double)
  (interp pixbuf-interp-type)
  (alpha :int)
  (xcheck :int)
  (ycheck :int)
  (size :int)
  (color1 :uint32)
  (color2 :uint32))

(defun pixbuf-composite-color (src dest
                               x y width height
                               xoffset yoffset
                               xscale yscale
                               interp
                               alpha
                               xcheck ycheck
                               size
                               color1 color2)
 #+liber-documentation
 "@version{#2024-6-29}
  @argument[src]{a @class{gdk-pixbuf:pixbuf} object}
  @argument[dest]{a @class{gdk-pixbuf:pixbuf} object into which to render the
    results}
  @argument[x]{an integer with the left coordinate for region to render}
  @argument[y]{an integer with the top coordinate for region to render}
  @argument[width]{an integer with the width of the region to render}
  @argument[height]{an integer with the height of the region to render}
  @argument[xoffset]{a number coerced to a double float with the offset in
    the x direction, currently rounded to an integer}
  @argument[yoffset]{a number coerced to a double float with the offset in
    the y direction, currently rounded to an integer}
  @argument[xscale]{a number coerced to a double float with the scale factor in
    the x direction}
  @argument[yscale]{a number coerced to a double float with the scale factor in
    the y direction}
  @argument[interp]{a @symbol{gdk-pixbuf:pixbuf-interp-type} interpolation type
    for the transformation}
  @argument[alpha]{an integer with the overall alpha for source image (0..255)}
  @argument[xcheck]{an integer with the x offset for the checkboard, origin of
    checkboard is at @code{(-@arg{xcheck}, -@arg{ycheck})}}
  @argument[ycheck]{an integer with the y offset for the checkboard}
  @argument[size]{an integer with the size of checks in the checkboard, must be
    a power of two}
  @argument[color1]{an unsigned integer with the color of check at upper left}
  @argument[color2]{an unsigned integer with the color of the other check}
  @begin{short}
    Creates a transformation of the source image @arg{src} by scaling by
    @arg{xscale} and @arg{yscale} then translating by @arg{xoffset} and
    @arg{yoffset}, then composites the rectangle @code{(x,y,width,height)} of
    the resulting image with a checkboard of the colors @arg{color1} and
    @arg{color2} and renders it onto the destination image.
  @end{short}

  See the @fun{gdk-pixbuf:pixbuf-composite-color-simple} function for a simpler
  variant of this function suitable for many tasks.
  @see-class{gdk-pixbuf:pixbuf}
  @see-symbol{gdk-pixbuf:pixbuf-interp-type}
  @see-function{gdk-pixbuf:pixbuf-composite-color-simple}"
  (%pixbuf-composite-color src dest
                           x y width height
                           (coerce xoffset 'double-float)
                           (coerce yoffset 'double-float)
                           (coerce xscale 'double-float)
                           (coerce yscale 'double-float)
                           interp
                           alpha
                           xcheck ycheck
                           size
                           color1 color2))

(export 'pixbuf-composite-color)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_rotate_simple
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_rotate_simple" pixbuf-rotate-simple)
    (g:object pixbuf)
#+liber-documentation
 "@version{#2024-6-29}
  @argument[src]{a @class{gdk-pixbuf:pixbuf} object}
  @argument[angle]{a @symbol{gdk-pixbuf:pixbuf-rotation} value}
  @return{The new @class{gdk-pixbuf:pixbuf} object, or @code{nil} if not enough
    memory could be allocated for it.}
  @begin{short}
    Rotates a pixbuf by a multiple of 90 degrees, and returns the result in a
    new pixbuf.
  @end{short}
  @see-class{gdk-pixbuf:pixbuf}
  @see-symbol{gdk-pixbuf:pixbuf-rotation}"
  (src (g:object pixbuf))
  (angle pixbuf-rotation))

(export 'pixbuf-rotate-simple)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_flip
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_flip" pixbuf-flip) (g:object pixbuf)
#+liber-documentation
 "@version{#2024-6-29}
  @argument[src]{a @class{gdk-pixbuf:pixbuf} object}
  @argument[horizontal]{@em{true} to flip horizontally, @em{false} to flip
    vertically}
  @begin{short}
    Flips a pixbuf horizontally or vertically and returns the result in a new
    pixbuf.
  @end{short}
  @see-class{gdk-pixbuf:pixbuf}"
  (src (g:object pixbuf))
  (horizontal :boolean))

(export 'pixbuf-flip)

;;; --- End of file gdk-pixbuf.scaling.lisp ------------------------------------
