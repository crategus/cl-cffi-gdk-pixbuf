;;; ----------------------------------------------------------------------------
;;; gdk-pixbuf.structure.lisp
;;;
;;; The documentation of this file is taken from the GDK-PixBuf Reference Manual
;;; Version 2.42 and modified to document the Lisp binding to the GDK-PixBuf
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
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
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------
;;;
;;; The GdkPixbuf Structure
;;;
;;;     Information that describes an image.
;;;
;;; Types and Values
;;;
;;;     GdkPixbufError
;;;     GDK_PIXBUF_ERROR
;;;
;;;     GdkColorspace
;;;     GdkPixbufAlphaMode                                  not implemented
;;;     GdkPixbuf
;;;
;;; Accessors
;;;
;;;     gdk_pixbuf_get_bits_per_sample
;;;     gdk_pixbuf_get_colorspace
;;;     gdk_pixbuf_get_has_alpha
;;;     gdk_pixbuf_get_height
;;;     gdk_pixbuf_get_n_channels
;;;     gdk_pixbuf_get_pixels
;;;     gdk_pixbuf_get_width
;;;
;;; Functions
;;;
;;;     gdk_pixbuf_get_pixels_with_length
;;;     gdk_pixbuf_get_byte_length
;;;     gdk_pixbuf_get_option
;;;     gdk_pixbuf_set_option
;;;     gdk_pixbuf_remove_option
;;;     gdk_pixbuf_get_options                              not implemented
;;;     gdk_pixbuf_copy_options
;;;     gdk_pixbuf_read_pixels
;;;
;;; Properties
;;;
;;;     bits-per-sample
;;;     colorspace
;;;     has-alpha
;;;     height
;;;     n-channels
;;;     pixel-bytes
;;;     pixels
;;;     rowstride
;;;     width
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkPixbuf
;;;
;;; Implemented Interfaces
;;;
;;;     GdkPixbuf implements GIcon and GLoadableIcon.
;;; ----------------------------------------------------------------------------

(in-package :gdk-pixbuf)

;;; ----------------------------------------------------------------------------
;;; enum GdkPixbufError
;;;
;;; typedef enum {
;;;         /* image data hosed */
;;;         GDK_PIXBUF_ERROR_CORRUPT_IMAGE,
;;;         /* no mem to load image */
;;;         GDK_PIXBUF_ERROR_INSUFFICIENT_MEMORY,
;;;         /* bad option passed to save routine */
;;;         GDK_PIXBUF_ERROR_BAD_OPTION,
;;;         /* unsupported image type (sort of an ENOSYS) */
;;;         GDK_PIXBUF_ERROR_UNKNOWN_TYPE,
;;;         /* unsupported operation (load, save) for image type */
;;;         GDK_PIXBUF_ERROR_UNSUPPORTED_OPERATION,
;;;         GDK_PIXBUF_ERROR_FAILED
;;; } GdkPixbufError;
;;;
;;; An error code in the GDK_PIXBUF_ERROR domain. Many &gdk-pixbuf; operations
;;; can cause errors in this domain, or in the G_FILE_ERROR domain.
;;;
;;; GDK_PIXBUF_ERROR_CORRUPT_IMAGE
;;;     An image file was broken somehow.
;;;
;;; GDK_PIXBUF_ERROR_INSUFFICIENT_MEMORY
;;;     Not enough memory.
;;;
;;; GDK_PIXBUF_ERROR_BAD_OPTION
;;;     A bad option was passed to a pixbuf save module.
;;;
;;; GDK_PIXBUF_ERROR_UNKNOWN_TYPE
;;;     Unknown image type.
;;;
;;; GDK_PIXBUF_ERROR_UNSUPPORTED_OPERATION
;;;     Don't know how to perform the given operation on the type of image at
;;;     hand.
;;;
;;; GDK_PIXBUF_ERROR_FAILED
;;;     Generic failure code, something went wrong.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GDK_PIXBUF_ERROR
;;;
;;; #define GDK_PIXBUF_ERROR gdk_pixbuf_error_quark ()
;;;
;;; Error domain used for pixbuf operations. Indicates that the error code will
;;; be in the GdkPixbufError enumeration. See GError for information on error
;;; domains and error codes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GdkColorspace
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GdkColorspace" colorspace
  (:export t
   :type-initializer "gdk_colorspace_get_type")
  :rgb)

#+liber-documentation
(setf (liber:alias-for-symbol 'colorspace)
      "GEnum"
      (liber:symbol-documentation 'colorspace)
 "@version{2024-6-29}
  @begin{declaration}
(gobject:define-g-enum \"GdkColorspace\" colorspace
  (:export t
   :type-initializer \"gdk_colorspace_get_type\")
  :rgb)
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:rgb]{Indicates a red/green/blue additive color space.}
    @end{table}
  @end{values}
  @begin{short}
    This enumeration defines the color spaces that are supported by the
    GDK-Pixbuf library.
  @end{short}
  Currently only RGB is supported.
  @see-class{gdk-pixbuf:pixbuf}")

;;; ----------------------------------------------------------------------------
;;; GdkPixbufAlphaMode                                      Deprecated 2.42
;;; ----------------------------------------------------------------------------

;; Only needed for deprecated functionality

(gobject:define-g-enum "GdkPixbufAlphaMode" pixbuf-alpha-mode
  (:export nil
   :type-initializer "gdk_pixbuf_alpha_mode_get_type")
  (:bilevel 0)
  (:full 1))

;;; ----------------------------------------------------------------------------
;;; GdkPixbuf
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GdkPixbuf" pixbuf
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_pixbuf_get_type")
  ((bits-per-sample
    pixbuf-bits-per-sample
    "bits-per-sample" "gint" t nil)
   (colorspace
    pixbuf-colorspace
    "colorspace" "GdkColorspace" t nil)
   (has-alpha
    pixbuf-has-alpha
    "has-alpha" "gboolean" t nil)
   (height
    pixbuf-height
    "height" "gint" t nil)
   (n-channels
    pixbuf-n-channels
    "n-channels" "gint" t nil)
   (pixel-bytes
    pixbuf-pixel-bytes
    "pixel-bytes" "GBytes" t t)
   (pixels
    pixbuf-pixels
    "pixels" "gpointer" t nil)
   (rowstride
    pixbuf-rowstride
    "rowstride" "gint" t nil)
   (width
    pixbuf-width
    "width" "gint" t nil)))

#+liber-documentation
(setf (documentation 'pixbuf 'type)
 "@version{2024-6-29}
  @begin{short}
    The @class{gdk-pixbuf:pixbuf} object contains information that describes an
    image in memory.
  @end{short}
  It contains information about the image's pixel data, its color space, bits
  per sample, width and height, and the rowstride (the number of bytes between
  the start of one row and the start of the next).
  @begin[Image Data]{dictionary}
    Image data in a pixbuf is stored in memory in uncompressed, packed format.
    Rows in the image are stored top to bottom, and in each row pixels are
    stored from left to right. There may be padding at the end of a row. The
    \"rowstride\" value of a pixbuf, as returned by the
    @fun{gdk-pixbuf:pixbuf-rowstride} function, indicates the number of bytes
    between rows.
  @end{dictionary}
  @begin{examples}
    The following code illustrates a simple @code{put-pixel} function for RGB
    pixbufs with 8 bits per channel with an alpha channel. It is not included
    in the @class{gdk-pixbuf:pixbuf} library for performance reasons. Rather
    than making several function calls for each pixel, your own code can take
    shortcuts.
    @begin{pre}
(defun put-pixel (pixbuf x y red green blue alpha)
  (let ((n-channels (gdk-pixbuf:pixbuf-n-channels pixbuf))
        (rowstride (gdk-pixbuf:pixbuf-rowstride pixbuf))
        (pixels (gdk-pixbuf:pixbuf-pixels pixbuf)))
    ;; Add offset to the pointer pixels into the pixbuf
    (cffi:incf-pointer pixels (+ (* y rowstride) (* x n-channels)))
    ;; Set the color of the point and the alpha value
    (setf (cffi:mem-aref pixels :uchar 0) red)
    (setf (cffi:mem-aref pixels :uchar 1) green)
    (setf (cffi:mem-aref pixels :uchar 2) blue)
    (setf (cffi:mem-aref pixels :uchar 3) alpha)))
    @end{pre}
    This function will not work for pixbufs with images that are other than
    8 bits per sample or channel, but it will work for most of the pixbufs that
    GTK uses.
  @end{examples}
  @begin{notes}
    If you are doing @code{memcpy()} of raw pixbuf data, note that the last row
    in the pixbuf may not be as wide as the full rowstride, but rather just as
    wide as the pixel data needs to be. That is, it is unsafe to do
    @code{memcpy (dest, pixels, rowstride * height)} to copy a whole pixbuf. Use
    the @fun{gdk-pixbuf:pixbuf-copy} function instead, or compute the width in
    bytes of the last row as @code{width * ((n_channels * bits_per_sample + 7) /
    8)}.
  @end{notes}
  @see-slot{gdk-pixbuf:pixbuf-bits-per-sample}
  @see-slot{gdk-pixbuf:pixbuf-colorspace}
  @see-slot{gdk-pixbuf:pixbuf-has-alpha}
  @see-slot{gdk-pixbuf:pixbuf-height}
  @see-slot{gdk-pixbuf:pixbuf-n-channels}
  @see-slot{gdk-pixbuf:pixbuf-pixel-bytes}
  @see-slot{gdk-pixbuf:pixbuf-pixels}
  @see-slot{gdk-pixbuf:pixbuf-rowstride}
  @see-slot{gdk-pixbuf:pixbuf-width}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gdk-pixbuf:pixbuf-bits-per-sample --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "bits-per-sample" 'pixbuf) t)
 "The @code{bits-per-sample} property of type @code{:int}
  (Read / Write / Construct Only) @br{}
  The number of bits per sample. Currently only 8 bit per sample are supported.
  @br{}
  Allowed values: [1,16] @br{}
  Default value: 8")

#+liber-documentation
(setf (liber:alias-for-function 'pixbuf-bits-per-sample)
      "Accessor"
      (documentation 'pixbuf-bits-per-sample 'function)
 "@version{2024-6-29}
  @syntax[]{(gdk-pixbuf:pixbuf-bits-per-sample object) => bits-per-sample}
  @argument[object]{a @class{gdk-pixbuf:pixbuf} object}
  @argument[bits-per-sample]{an integer with the number of bits per sample}
  @begin{short}
    Accessor of the @slot[gdk-pixbuf:pixbuf]{bits-per-sample} slot of the
    @class{gdk-pixbuf:pixbuf} class.
  @end{short}
  Queries the number of bits per color sample in a pixbuf. Currently only 8 bit
  per sample are supported.
  @see-class{gdk-pixbuf:pixbuf}")

;;; --- gdk-pixbuf:pixbuf-colorspace -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "colorspace" 'pixbuf) t)
 "The @code{colorspace} property of type @symbol{gdk-pixbuf:colorspace}
  (Read / Write / Construct Only) @br{}
  The colorspace in which the samples are interpreted. @br{}
  Default value: @code{:rgb}")

#+liber-documentation
(setf (liber:alias-for-function 'pixbuf-colorspace)
      "Accessor"
      (documentation 'pixbuf-colorspace 'function)
 "@version{2024-6-29}
  @syntax[]{(gdk-pixbuf:pixbuf-colorspace object) => colorspace}
  @argument[object]{a @class{gdk-pixbuf:pixbuf} object}
  @argument[colorspace]{a @symbol{gdk-pixbuf:colorspace} value}
  @begin{short}
    Accessor of the @slot[gdk-pixbuf:pixbuf]{colorspace} slot of the
    @class{gdk-pixbuf:pixbuf} class.
  @end{short}
  Querries the colorspace in which the samples are interpreted.
  @see-class{gdk-pixbuf:pixbuf}
  @see-symbol{gdk-pixbuf:colorspace}")

;;; --- gdk-pixbuf:pixbuf-has-alpha --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-alpha" 'pixbuf) t)
 "The @code{has-alpha} property of type @code{:boolean}
  (Read / Write / Construct Only) @br{}
  Whether the pixbuf has an alpha channel. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'pixbuf-has-alpha)
      "Accessor"
      (documentation 'pixbuf-has-alpha 'function)
 "@version{2024-6-29}
  @syntax[]{(gdk-pixbuf:pixbuf-has-alpha object) => has-alpha}
  @argument[object]{a @class{gdk-pixbuf:pixbuf} object}
  @argument[has-alpha]{a boolean whether the pixbuf has an alpha channel}
  @begin{short}
    Accessor of the @slot[gdk-pixbuf:pixbuf]{has-alpha} slot of the
    @class{gdk-pixbuf:pixbuf} class.
  @end{short}
  Queries whether a pixbuf has an alpha channel (opacity information).
  @see-class{gdk-pixbuf:pixbuf}")

;;; --- gdk-pixbuf:pixbuf-height -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "height" 'pixbuf) t)
 "The @code{height} property of type @code{:int} (Read / Write / Construct Only)
  @br{}
  The number of rows of the pixbuf. @br{}
  Allowed values: >= 1 @br{}
  Default value: 1")

#+liber-documentation
(setf (liber:alias-for-function 'pixbuf-height)
      "Accessor"
      (documentation 'pixbuf-height 'function)
 "@version{2024-6-29}
  @syntax[]{(gdk-pixbuf:pixbuf-height object) => height}
  @argument[object]{a @class{gdk-pixbuf:pixbuf} object}
  @argument[height]{an integer with the number of rows of the pixbuf}
  @begin{short}
    Accessor of the @slot[gdk-pixbuf:pixbuf]{height} slot of the
    @class{gdk-pixbuf:pixbuf} class.
  @end{short}
  Queries the height of a pixbuf.
  @see-class{gdk-pixbuf:pixbuf}")

;;; --- gdk-pixbuf:pixbuf-n-channels -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "n-channels" 'pixbuf) t)
 "The @code{n-channels} property of type @code{:int}
  (Read / Write / Construct Only) @br{}
  The number of samples per pixel. Currently, only 3 or 4 samples per pixel
  are supported. @br{}
  Allowed values: >= 0 @br{}
  Default value: 3")

#+liber-documentation
(setf (liber:alias-for-function 'pixbuf-n-channels)
      "Accessor"
      (documentation 'pixbuf-n-channels 'function)
 "@version{2024-6-29}
  @syntax[]{(gdk-pixbuf:pixbuf-n-channels object) => n-channels}
  @argument[object]{a @class{gdk-pixbuf:pixbuf} object}
  @argument[n-channels]{an integer with the number of channels}
  @begin{short}
    Accessor of the @slot[gdk-pixbuf:pixbuf]{n-channels} slot of the
    @class{gdk-pixbuf:pixbuf} class.
  @end{short}
  Queries the number of channels of a pixbuf.
  @see-class{gdk-pixbuf:pixbuf}")

;;; --- gdk-pixbuf:pixbuf-pixel-bytes ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "pixel-bytes" 'pixbuf) t)
 "The @code{pixel-bytes} property of type @class{g:bytes}
  (Read / Write / Construct Only) @br{}
  Readonly pixel data.")

#+liber-documentation
(setf (liber:alias-for-function 'pixbuf-pixel-bytes)
      "Accessor"
      (documentation 'pixbuf-pixel-bytes 'function)
 "@version{2024-6-29}
  @syntax[]{(gdk-pixbuf:pixbuf-pixel-bytes object) => pixel-bytes}
  @argument[object]{a @class{gdk-pixbuf:pixbuf} object}
  @argument[pixel-bytes]{a @class{g:bytes} instance with the pixel data}
  @begin{short}
    Accessor of the @slot[gdk-pixbuf:pixbuf]{pixel-bytes} slot of the
    @class{gdk-pixbuf:pixbuf} class.
  @end{short}
  Querries the readonly pixel data.
  @see-class{gdk-pixbuf:pixbuf}")

;;; --- gdk-pixbuf:pixbuf-pixels -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "pixels" 'pixbuf) t)
 "The @code{pixels} property of type @code{:pointer}
  (Read / Write / Construct Only) @br{}
  The pointer to the pixel data of the pixbuf.")

#+liber-documentation
(setf (liber:alias-for-function 'pixbuf-pixels)
      "Accessor"
      (documentation 'pixbuf-pixels 'function)
 "@version{2024-6-29}
  @syntax[]{(gdk-pixbuf:pixbuf-pixels object) => pixels}
  @argument[object]{a @class{gdk-pixbuf:pixbuf} object}
  @argument[pixels]{a pointer to the pixel data of @arg{pixbuf}}
  @begin{short}
    Accessor of the @slot[gdk-pixbuf:pixbuf]{pixels} slot of the
    @class{gdk-pixbuf:pixbuf} class.
  @end{short}
  Queries a pointer to the pixel data of a pixbuf. Please see the section called
  \"Image Data\" in the @class{gdk-pixbuf:pixbuf} documentation for information
  about how the pixel data is stored in memory. This function will cause an
  implicit copy of the pixbuf data if the pixbuf was created from read-only
  data.
  @see-class{gdk-pixbuf:pixbuf}")

;;; --- gdk-pixbuf:pixbuf-rowstride --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "rowstride" 'pixbuf) t)
 "The @code{rowstride} property of type @code{:int}
  (Read / Write / Construct Only) @br{}
  The number of bytes between the start of a row and the start of the next
  row. This number must be at least as large as the width of the pixbuf. @br{}
  Allowed values: >= 1 @br{}
  Default value: 1")

#+liber-documentation
(setf (liber:alias-for-function 'pixbuf-rowstride)
      "Accessor"
      (documentation 'pixbuf-rowstride 'function)
 "@version{2024-6-29}
  @syntax[]{(gdk-pixbuf:pixbuf-rowstride object) => rowstride}
  @argument[object]{a @class{gdk-pixbuf:pixbuf} object}
  @argument[rowstride]{an integer with the distance between row starts}
  @begin{short}
    Accessor of the @slot[gdk-pixbuf:pixbuf]{rowstride} slot of the
    @class{gdk-pixbuf:pixbuf} class.
  @end{short}
  Queries the rowstride of a pixbuf, which is the number of bytes between
  the start of a row and the start of the next row.
  @see-class{gdk-pixbuf:pixbuf}")

;;; --- gdk-pixbuf:pixbuf-width ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "width" 'pixbuf) t)
 "The @code{width} property of type @code{:int} (Read / Write / Construct Only)
  @br{}
  The number of columns of the pixbuf. @br{}
  Allowed values: >= 1 @br{}
  Default value: 1")

#+liber-documentation
(setf (liber:alias-for-function 'pixbuf-width)
      "Accessor"
      (documentation 'pixbuf-width 'function)
 "@version{2024-6-29}
  @syntax[]{(gdk-pixbuf:pixbuf-width object) => width}
  @argument[object]{a @class{gdk-pixbuf:pixbuf} object}
  @argument[width]{an integer with the width of the pixbuf}
  @begin{short}
    Accessor of the @slot[gdk-pixbuf:pixbuf]{width} slot of the
    @class{gdk-pixbuf:pixbuf} class.
  @end{short}
  Queries the width of a pixbuf.
  @see-class{gdk-pixbuf:pixbuf}")

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_pixels_with_length
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_get_pixels_with_length" pixbuf-pixels-with-length)
    (:pointer :uchar)
 #+liber-documentation
 "@version{#2024-6-29}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object}
  @argument[length]{an unsigned integer with the length of the binary data}
  @return{The pointer to the pixel data of the pixbuf.}
  @begin{short}
    Queries a pointer to the pixel data of a pixbuf.
  @end{short}
  Please see the section on image data in the @class{gdk-pixbuf:pixbuf}
  documentation for information about how the pixel data is stored in memory.
  This function will cause an implicit copy of the pixbuf data if the pixbuf
  was created from read-only data.
  @see-class{gdk-pixbuf:pixbuf}
  @see-function{gdk-pixbuf:pixbuf-pixels}"
  (pixbuf (g:object pixbuf))
  (length :uint))

(export 'pixbuf-pixels-with-length)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_byte_length
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_get_byte_length" pixbuf-byte-length) :size
 #+liber-documentation
 "@version{#2024-6-29}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object}
  @return{The integer with the length of the pixel data.}
  @short{Returns the length of the pixel data, in bytes.}
  @see-class{gdk-pixbuf:pixbuf}"
  (pixbuf (g:object pixbuf)))

(export 'pixbuf-byte-length)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_option
;;; gdk_pixbuf_set_option
;;; ----------------------------------------------------------------------------

(defun (setf pixbuf-option) (value pixbuf key)
  (when (cffi:foreign-funcall "gdk_pixbuf_set_option"
                              (g:object pixbuf) pixbuf
                              :string key
                              :string value
                              :boolean)
    value))

(cffi:defcfun ("gdk_pixbuf_get_option" pixbuf-option) :string
 #+liber-documentation
 "@version{#2024-6-29}
  @syntax{(gdk-pixbuf:pixbuf-option pixbuf key) => value}
  @syntax{(setf (gdk-pixbuf:pixbuf-option pixbuf key) value)}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object}
  @argument[key]{a string with a key}
  @argument[value]{a string with a value}
  @begin{short}
    The @fun{gdk-pixbuf:pixbuf-option} function looks up @arg{key} in the list
    of options that may have been attached to the pixbuf when it was loaded, or
    that may have been attached.
  @end{short}
  The @setf{gdk-pixbuf:pixbuf-option} function attaches a key/value pair as
  an option to a pixbuf. If @arg{key} already exists in the list of options
  attached to pixbuf, the new value is ignored.

  For instance, the ANI loader provides \"Title\" and \"Artist\" options. The
  ICO, XBM, and XPM loaders provide \"x_hot\" and \"y_hot\" hot-spot options
  for cursor definitions. The PNG loader provides the tEXt ancillary chunk
  key/value pairs as options. Since 2.12, the TIFF and JPEG loaders return an
  \"orientation\" option string that corresponds to the embedded TIFF/Exif
  orientation tag (if present). Since 2.32, the TIFF loader sets the
  \"multipage\" option string to \"yes\" when a multi-page TIFF is loaded.
  Since 2.32 the JPEG and PNG loaders set \"x-dpi\" and \"y-dpi\" if the file
  contains image density information in dots per inch.
  @see-class{gdk-pixbuf:pixbuf}"
  (pixbuf (g:object pixbuf))
  (key :string))

(export 'pixbuf-option)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_remove_option
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_remove_option" pixbuf-remove-option) :boolean
 #+liber-documentation
 "@version{#2024-6-29}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} oject}
  @argument[key]{a string representing the key to remove}
  @return{@em{True} if an option was removed, @em{false} if not.}
  @begin{short}
    Remove the key/value pair option attached to a pixbuf.
  @end{short}
  @see-class{gdk-pixbuf:pixbuf}
  @see-function{gdk-pixbuf:pixbuf-option}"
  (pixbuf (g:object pixbuf))
  (key :string))

(export 'pixbuf-remove-option)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_options ()
;;;
;;; GHashTable *
;;; gdk_pixbuf_get_options (GdkPixbuf *pixbuf);
;;;
;;; Returns a GHashTable with a list of all the options that may have been
;;; attached to the pixbuf when it was loaded, or that may have been attached
;;; by another function using gdk_pixbuf_set_option().
;;;
;;; See gdk_pixbuf_get_option() for more details.
;;;
;;; pixbuf :
;;;     a GdkPixbuf
;;;
;;; Returns :
;;;     a GHashTable of key/values.
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_copy_options
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_copy_options" pixbuf-copy-options) :boolean
 #+liber-documentation
 "@version{#2024-6-29}
  @argument[src]{a @class{gdk-pixbuf:pixbuf} to copy options from}
  @argument[dest]{a @class{gdk-pixbuf:pixbuf} to copy options to}
  @return{@em{True} on sucess.}
  @begin{short}
    Copy the key/value pair options attached to a pixbuf to another.
  @end{short}
  This is useful to keep original metadata after having manipulated a file.
  However be careful to remove metadata which you have already applied, such as
  the \"orientation\" option after rotating the image.
  @see-class{gdk-pixbuf:pixbuf}
  @see-function{gdk-pixbuf:pixbuf-option}"
  (src (g:object pixbuf))
  (dest (g:object pixbuf)))

(export 'pixbuf-copy-options)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_read_pixels
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_read_pixels" pixbuf-read-pixels) (:pointer :uint8)
 #+liber-documentation
 "@version{#2024-6-29}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object}
  @return{The pointer to the pixel data.}
  @begin{short}
    Returns a read-only pointer to the raw pixel data.
  @end{short}
  Must not be modified. This function allows skipping the implicit copy that
  must be made if the @fun{gdk-pixbuf:pixbuf-pixels} function is called on a
  read-only pixbuf.
  @see-class{gdk-pixbuf:pixbuf}
  @see-function{gdk-pixbuf:pixbuf-pixels}"
  (pixbuf (g:object pixbuf)))

(export 'pixbuf-read-pixels)

;;; --- End of file gdk-pixbuf.structure.lisp ----------------------------------
