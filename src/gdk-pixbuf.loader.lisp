;;; ----------------------------------------------------------------------------
;;; gdk-pixbuf.loader.lisp
;;;
;;; The documentation in this file is taken from the GDK-PixBuf Reference Manual
;;; version 2.42 and modified to document the Lisp binding to the GDK-PixBuf
;;; library, see <http://www.gtk.org>. The API documentation for the Lisp
;;; binding is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2013 - 2025 Dieter Kaiser
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
;;; GdkPixbufLoader
;;;
;;;     Application-driven progressive image loading
;;;
;;; Types and Values
;;;
;;;     GdkPixbufLoader
;;;
;;; Functions
;;;
;;;     gdk_pixbuf_loader_new
;;;     gdk_pixbuf_loader_new_with_type
;;;     gdk_pixbuf_loader_new_with_mime_type
;;;     gdk_pixbuf_loader_get_format
;;;     gdk_pixbuf_loader_write
;;;     gdk_pixbuf_loader_write_bytes
;;;     gdk_pixbuf_loader_set_size
;;;     gdk_pixbuf_loader_get_pixbuf
;;;     gdk_pixbuf_loader_get_animation
;;;     gdk_pixbuf_loader_close
;;;
;;; Signals
;;;
;;;     area-prepared
;;;     area-updated
;;;     closed
;;;     size-prepared
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkPixbufLoader
;;;
;;; ----------------------------------------------------------------------------

(in-package :gdk-pixbuf)

;;; ----------------------------------------------------------------------------
;;; GdkPixbufLoader
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GdkPixbufLoader" pixbuf-loader
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_pixbuf_loader_get_type")
  nil)

#+liber-documentation
(setf (documentation 'pixbuf-loader 'type)
 "@version{2025-06-29}
  @begin{short}
    The @class{gdk-pixbuf:pixbuf-loader} class provides a way for applications
    to drive the process of loading an image, by letting them send the image
    data directly to the loader instead of having the loader read the data from
    a file.
  @end{short}
  Applications can use this functionality instead of the
  @fun{gdk-pixbuf:pixbuf-new-from-file} or
  @fun{gdk-pixbuf:pixbuf-animation-new-from-file} functions when they need to
  parse image data in small chunks. For example, it should be used when reading
  an image from a (potentially) slow network connection, or when loading an
  extremely large file.

  To use the @class{gdk-pixbuf:pixbuf-loader} class to load an image, just
  create a new one, and call the @fun{gdk-pixbuf:pixbuf-loader-write} function
  to send the data to it. When done, the @fun{gdk-pixbuf:pixbuf-loader-close}
  function should be called to end the stream and finalize everything. The
  loader will emit three important signals throughout the process. The first,
  the @code{\"size-prepared\"} signal, will be called as soon as the image has
  enough information to determine the size of the image to be used. If you want
  to scale the image while loading it, you can call the
  @fun{gdk-pixbuf:pixbuf-loader-set-size} function in response to this signal.

  The second signal, the @code{\"area-prepared\"} signal, will be called as soon
  as the pixbuf of the desired has been allocated. You can obtain it by calling
  the @fun{gdk-pixbuf:pixbuf-loader-pixbuf} function. In addition, no actual
  information will be passed in yet, so the pixbuf can be safely filled with any
  temporary graphics (or an initial color) as needed. You can also call the
  @fun{gdk-pixbuf:pixbuf-loader-pixbuf} function later and get the same pixbuf.

  The last signal, the @code{\"area-updated\"} signal gets called every time a
  region is updated. This way you can update a partially completed image. Note
  that you do not know anything about the completeness of an image from the area
  updated. For example, in an interlaced image, you need to make several passes
  before the image is done loading.

  @subheading{Loading an animation}
    Loading an animation is almost as easy as loading an image. Once the first
    @code{\"area-prepared\"} signal has been emitted, you can call the
    @fun{gdk-pixbuf:pixbuf-loader-animation} function to get the
    @class{gdk-pixbuf:pixbuf-animation} object and the
    @fun{gdk-pixbuf:pixbuf-animation-iter} function to get an
    @class{gdk-pixbuf:pixbuf-animation-iter} object for displaying it.
  @begin[Signal Details]{dictionary}
    @begin[pixbuf-loader::area-prepared]{signal}
      @begin{pre}
lambda (loader)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[loader]{The @class{gdk-pixbuf:pixbuf-loader} object that received
          the signal.}
      @end{simple-table}
      The signal is emitted when the pixbuf loader has allocated the pixbuf
      in the desired size. After this signal is emitted, applications can call
      the @fun{gdk-pixbuf:pixbuf-loader-pixbuf} function to fetch the partially-
      loaded pixbuf.
    @end{signal}
    @begin[pixbuf-loader::area-updated]{signal}
      @begin{pre}
lambda (loader x y width height)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[loader]{The @class{gdk-pixbuf:pixbuf-loader} object that received
          the signal.}
        @entry[x]{The integer for the x offset of upper-left corner of the
          updated area.}
        @entry[y]{The integer for the y offset of upper-left corner of the
          updated area.}
        @entry[width]{The integer for the width of updated area.}
        @entry[height]{The integer for the height of updated area.}
      @end{simple-table}
      The signal is emitted when a significant area of the image being loaded
      has been updated. Normally it means that a complete scanline has been read
      in, but it could be a different area as well. Applications can use this
      signal to know when to repaint areas of an image that is being loaded.
    @end{signal}
    @begin[pixbuf-loader::closed]{signal}
      @begin{pre}
lambda (loader)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[loader]{The @class{gdk-pixbuf:pixbuf-loader} object that
          received the signal.}
      @end{simple-table}
      The signal is emitted when the @fun{gdk-pixbuf:pixbuf-loader-close}
      function is called. It can be used by different parts of an application
      to receive notification when an image loader is closed by the code that
      drives it.
    @end{signal}
    @begin[pixbuf-loader::size-prepared]{signal}
      @begin{pre}
lambda (loader width height)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[loader]{The @class{gdk-pixbuf:pixbuf-loader} object that
          received the signal.}
        @entry[width]{The integer for the original width of the image.}
        @entry[height]{The integer for the original height of the image.}
      @end{simple-table}
      The signal is emitted when the pixbuf loader has been fed the initial
      amount of data that is required to figure out the size of the image that
      it will create. Applications can call the
      @fun{gdk-pixbuf:pixbuf-loader-set-size} function in response to this
      signal to set the desired size to which the image should be scaled.
    @end{signal}
  @end{dictionary}
  @see-class{gdk-pixbuf:pixbuf-animation}
  @see-class{gdk-pixbuf:pixbuf-animation-iter}")

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_loader_new
;;; ----------------------------------------------------------------------------

(declaim (inline pixbuf-loader-new))

(defun pixbuf-loader-new ()
 #+liber-documentation
 "@version{2025-03-01}
  @return{The newly created @class{gdk-pixbuf:pixbuf-loader} object.}
  @short{Creates a new pixbuf loader object.}
  @see-class{gdk-pixbuf:pixbuf-loader}"
  (make-instance 'pixbuf-loader))

(export 'pixbuf-loader-new)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_loader_new_with_type ()
;;;
;;; GdkPixbufLoader *
;;; gdk_pixbuf_loader_new_with_type (const char *image_type, GError **error);
;;;
;;; Creates a new pixbuf loader object that always attempts to parse image data
;;; as if it were an image of type image_type, instead of identifying the type
;;; automatically. Useful if you want an error if the image isn't the expected
;;; type, for loading image formats that can't be reliably identified by looking
;;; at the data, or if the user manually forces a specific type.
;;;
;;; The list of supported image formats depends on what image loaders are
;;; installed, but typically "png", "jpeg", "gif", "tiff" and "xpm" are among
;;; the supported formats. To obtain the full list of supported image formats,
;;; call gdk_pixbuf_format_get_name() on each of the GdkPixbufFormat structs
;;; returned by gdk_pixbuf_get_formats().
;;;
;;; image_type :
;;;     name of the image format to be loaded with the image
;;;
;;; error :
;;;     return location for an allocated GError, or NULL to ignore errors
;;;
;;; Returns :
;;;     A newly created pixbuf loader.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_loader_new_with_mime_type ()
;;;
;;; GdkPixbufLoader *
;;; gdk_pixbuf_loader_new_with_mime_type (const char *mime_type, GError **error)
;;;
;;; Creates a new pixbuf loader object that always attempts to parse image data
;;; as if it were an image of mime type mime_type, instead of identifying the
;;; type automatically. Useful if you want an error if the image isn't the
;;; expected mime type, for loading image formats that can't be reliably
;;; identified by looking at the data, or if the user manually forces a
;;; specific mime type.
;;;
;;; The list of supported mime types depends on what image loaders are
;;; installed, but typically "image/png", "image/jpeg", "image/gif",
;;; "image/tiff" and "image/x-xpixmap" are among the supported mime types. To
;;; obtain the full list of supported mime types, call
;;; gdk_pixbuf_format_get_mime_types() on each of the GdkPixbufFormat structs
;;; returned by gdk_pixbuf_get_formats().
;;;
;;; mime_type :
;;;     the mime type to be loaded
;;;
;;; error :
;;;     return location for an allocated GError, or NULL to ignore errors
;;;
;;; Returns :
;;;     A newly created pixbuf loader.
;;;
;;; Since 2.4
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_loader_get_format ()
;;;
;;; GdkPixbufFormat *
;;; gdk_pixbuf_loader_get_format (GdkPixbufLoader *loader);
;;;
;;; Obtains the available information about the format of the currently loading
;;; image file.
;;;
;;; loader :
;;;     A pixbuf loader.
;;;
;;; Returns :
;;;     A GdkPixbufFormat or NULL. The return value is owned by GdkPixbuf and
;;;     should not be freed.
;;;
;;; Since 2.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_loader_write
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_loader_write" %pixbuf-loader-write) :boolean
  (loader (g:object pixbuf-loader))
  (buf (:pointer :uchar))
  (count :size)
  (err :pointer))

(defun pixbuf-loader-write (loader buffer count)
 #+liber-documentation
 "@version{2025-03-01}
  @argument[loader]{a @class{gdk-pixbuf:pixbuf-loader} object}
  @argument[buffer]{a Lisp array for image data}
  @argument[count]{an integer for the length of @arg{buffer} in bytes}
  @begin{return}
    @em{True} if the write was successful, or @em{false} if the loader cannot
    parse the buffer.
  @end{return}
  @begin{short}
    This will cause a pixbuf loader to parse the next count bytes of an image.
  @end{short}
  It will return @em{true} if the data was loaded successfully, and @em{false}
  if an error occurred.
  @begin[Examples]{dictionary}
    A code fragment, which writes data into the pixbuf loader:
    @begin{pre}
;; Create the image stream and the GdkPixbufLoader
(setf stream
      (open (sys-path \"alphatest.png\")
            :element-type '(unsigned-byte 8)))
(setf loader (gdk-pixbuf:pixbuf-loader-new))

...

(let* ((buffer (make-array 128 :element-type '(unsigned-byte 8)))
       (len (read-sequence buffer stream)))
  ...
  ;; Load the buffer into GdkPixbufLoader
  (gdk-pixbuf:pixbuf-loader-write loader buffer 128)
  ... )
    @end{pre}
  @end{dictionary}
  @see-class{gdk-pixbuf:pixbuf-loader}"
  (glib:with-error (err)
    (let ((buf (cffi:foreign-alloc :uchar :initial-contents buffer)))
      (%pixbuf-loader-write loader buf count err))))

(export 'pixbuf-loader-write)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_loader_write_bytes ()
;;;
;;; gboolean
;;; gdk_pixbuf_loader_write_bytes (GdkPixbufLoader *loader,
;;;                                GBytes *buffer,
;;;                                GError **error);
;;;
;;; This will cause a pixbuf loader to parse a buffer inside a GBytes for an
;;; image. It will return TRUE if the data was loaded successfully, and FALSE
;;; if an error occurred. In the latter case, the loader will be closed, and
;;; will not accept further writes. If FALSE is returned, error will be set to
;;; an error from the GDK_PIXBUF_ERROR or G_FILE_ERROR domains.
;;;
;;; See also: gdk_pixbuf_loader_write()
;;;
;;; loader :
;;;     A pixbuf loader.
;;;
;;; buffer :
;;;     The image data as a GBytes
;;;
;;; error :
;;;     return location for errors
;;;
;;; Returns :
;;;     TRUE if the write was successful, or FALSE if the loader cannot parse
;;;     the buffer.
;;;
;;; Since 2.30
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_loader_set_size
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_loader_set_size" pixbuf-loader-set-size) :void
 #+liber-documentation
 "@version{#2025-03-01}
  @argument[loader]{a @class{gdk-pixbuf:pixbuf-loader} object}
  @argument[width]{an integer for the desired width of the image being loaded}
  @argument[height]{an integer for the desired height of the image being loaded}
  @begin{short}
    Causes the image to be scaled while it is loaded.
  @end{short}
  The desired image size can be determined relative to the original size of the
  image by calling the @fun{gdk-pixbuf:pixbuf-loader-set-size} function from a
  signal handler for the @code{\"size-prepared\"} signal.

  Attempts to set the desired image size are ignored after the emission of the
  @code{\"size-prepared\"} signal.
  @see-class{gdk-pixbuf:pixbuf-loader}"
  (loader (g:object pixbuf-loader))
  (width :int)
  (height :int))

(export 'pixbuf-loader-set-size)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_loader_get_pixbuf
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_loader_get_pixbuf" pixbuf-loader-pixbuf)
    (g:object pixbuf)
 #+liber-documentation
 "@version{2025-03-01}
  @argument[loader]{a @class{gdk-pixbuf:pixbuf-loader} object}
  @begin{return}
    The @class{gdk-pixbuf:pixbuf} object that the loader is creating, or
    @code{nil} if not enough data has been read to determine how to create the
    image buffer.
  @end{return}
  @begin{short}
    Queries the @class{gdk-pixbuf:pixbuf} object that a pixbuf loader is
    currently creating.
  @end{short}
  In general it only makes sense to call this function after the
  @code{\"area-prepared\"} signal has been emitted by the loader. This means
  that enough data has been read to know the size of the image that will be
  allocated. If the loader has not received enough data via the
  @fun{gdk-pixbuf:pixbuf-loader-write} function, then this function returns
  @code{nil}. The returned pixbuf will be the same in all future calls to the
  loader. Additionally, if the loader is an animation, it will return the
  \"static image\" of the animation, see the
  @fun{gdk-pixbuf:pixbuf-animation-static-image} function.
  @see-class{gdk-pixbuf:pixbuf-loader}
  @see-class{gdk-pixbuf:pixbuf}
  @see-function{gdk-pixbuf:pixbuf-loader-write}
  @see-function{gdk-pixbuf:pixbuf-animation-static-image}"
  (loader (g:object pixbuf-loader)))

(export 'pixbuf-loader-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_loader_get_animation
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_loader_get_animation" pixbuf-loader-animation)
    (g:object pixbuf-animation)
 #+liber-documentation
 "@version{#2025-03-01}
  @argument[loader]{a @class{gdk-pixbuf:pixbuf-loader} object}
  @begin{return}
    The @class{gdk-pixbuf:pixbuf-animation} object that the loader is loading,
    or @code{nil} if not enough data has been read to determine the information.
  @end{return}
  @begin{short}
    Queries the @class{gdk-pixbuf:pixbuf-animation} object that a pixbuf loader
    is currently creating.
  @end{short}
  In general it only makes sense to call this function after the
  @code{\"area-prepared\"} signal has been emitted by the loader. If the loader
  does not have enough bytes yet, has not emitted the @code{\"area-prepared\"}
  signal, this function will return @code{nil}.
  @see-class{gdk-pixbuf:pixbuf-loader}
  @see-class{gdk-pixbuf:pixbuf-animation}"
  (loader (g:object pixbuf-loader)))

(export 'pixbuf-loader-animation)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_loader_close
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_loader_close" %pixbuf-loader-close) :boolean
  (loader (g:object pixbuf-loader))
  (err :pointer))

(defun pixbuf-loader-close (loader)
 #+liber-documentation
 "@version{2025-03-01}
  @argument[loader]{a @class{gdk-pixbuf:pixbuf-loader} object}
  @begin{return}
    @em{True} if all image data written so far was successfully passed out via
    the @code{\"update-area\"} signal.
  @end{return}
  @begin{short}
    Informs a pixbuf loader that no further writes with the
    @fun{gdk-pixbuf:pixbuf-loader-write} function will occur, so that it can
    free its internal loading structures.
  @end{short}
  Also, tries to parse any data that has not yet been parsed.
  @see-class{gdk-pixbuf:pixbuf-loader}
  @see-function{gdk-pixbuf:pixbuf-loader-write}"
  (glib:with-error (err)
    (%pixbuf-loader-close loader err)))

(export 'pixbuf-loader-close)

;;; --- End of file gdk-pixbuf.loader.lisp -------------------------------------
