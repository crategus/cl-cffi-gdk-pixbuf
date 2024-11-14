;;; ----------------------------------------------------------------------------
;;; gdk-pixbuf.load.lisp
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
;;; File Loading
;;;
;;;     Loading a pixbuf from a file.
;;;
;;; Functions
;;;
;;;     gdk_pixbuf_new_from_file
;;;     gdk_pixbuf_new_from_file_at_size
;;;     gdk_pixbuf_new_from_file_at_scale
;;;     gdk_pixbuf_new_from_resource
;;;     gdk_pixbuf_new_from_resource_at_scale
;;;
;;;     gdk_pixbuf_get_file_info
;;;     gdk_pixbuf_get_file_info_async
;;;     gdk_pixbuf_get_file_info_finish
;;;
;;;     gdk_pixbuf_new_from_stream
;;;     gdk_pixbuf_new_from_stream_async
;;;     gdk_pixbuf_new_from_stream_finish
;;;     gdk_pixbuf_new_from_stream_at_scale
;;;     gdk_pixbuf_new_from_stream_at_scale_async
;;; ----------------------------------------------------------------------------

(in-package :gdk-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_new_from_file
;;; ----------------------------------------------------------------------------

;; TODO: Check the functionality of :already-referenced

(cffi:defcfun ("gdk_pixbuf_new_from_file" %pixbuf-new-from-file)
    (g:object pixbuf :already-referenced)
  (filename :string)
  (err :pointer))

(defun pixbuf-new-from-file (path)
 #+liber-documentation
 "@version{2024-5-30}
  @argument[path]{a pathname or namestring with the file to load, in the GLib
    file name encoding}
  @begin{return}
    The newly created @class{gdk-pixbuf:pixbuf} object, or @code{nil} if any of
    several error conditions occurred: the file could not be opened, there was
    no loader for the format of the file, there was not enough memory to
    allocate the image buffer, or the image file contained invalid data.
  @end{return}
  @begin{short}
    Creates a new pixbuf by loading an image from a file.
  @end{short}
  The file format is detected automatically.
  @see-class{gdk-pixbuf:pixbuf}"
  (glib:with-ignore-g-error (err)
    (%pixbuf-new-from-file (namestring path) err)))

(export 'pixbuf-new-from-file)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_new_from_file_at_size
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_new_from_file_at_size" %pixbuf-new-from-file-at-size)
    (g:object pixbuf :already-referenced)
  (filename :string)
  (width :int)
  (height :int)
  (err :pointer))

(defun pixbuf-new-from-file-at-size (path width height)
 #+liber-documentation
 "@version{2024-5-30}
  @argument[path]{a pathname or namestring with the file to load, in the GLib
    file name encoding}
  @argument[width]{an integer with the width the image should have or -1 to not
    constrain the width}
  @argument[height]{an integer with the height the image should have or -1 to
    not constrain the height}
  @begin{return}
    The newly created @class{gdk-pixbuf:pixbuf} object, or @code{nil} if any of
    several error conditions occurred: the file could not be opened, there was
    no loader for the format of the file, there was not enough memory to
    allocate the image buffer, or the image file contained invalid data.
  @end{return}
  @begin{short}
    Creates a new pixbuf by loading an image from a file.
  @end{short}
  The file format is detected automatically.

  The image will be scaled to fit in the requested size, preserving the aspect
  ratio of the image. Note that the returned pixbuf may be smaller than
  @arg{width} @code{x} @arg{height}, if the aspect ratio requires it. To load
  an image at the requested size, regardless of the aspect ratio, use the
  @fun{gdk-pixbuf:pixbuf-new-from-file-at-scale} function.
  @see-class{gdk-pixbuf:pixbuf}
  @see-function{gdk-pixbuf:pixbuf-new-from-file-at-scale}"
  (glib:with-ignore-g-error (err)
    (%pixbuf-new-from-file-at-size (namestring path) width height err)))

(export 'pixbuf-new-from-file-at-size)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_new_from_file_at_scale
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_new_from_file_at_scale"
               %pixbuf-new-from-file-at-scale)
    (g:object pixbuf :already-referenced)
  (filename :string)
  (width :int)
  (height :int)
  (preserve :boolean)
  (err :pointer))

(defun pixbuf-new-from-file-at-scale (path width height preserve)
 #+liber-documentation
 "@version{20245-30}
  @argument[path]{a pathname or namestring with the file to load, in the GLib
    file name encoding}
  @argument[width]{an integer with the width the image should have or -1 to not
    constrain the width}
  @argument[height]{an integer with the height the image should have or -1 to
    not constrain the height}
  @argument[preserve]{@em{true} to preserve the aspect ratio of the image}
  @begin{return}
    The newly created @class{gdk-pixbuf:pixbuf} object, or @code{nil} if any of
    several error conditions occurred: the file could not be opened, there was
    no loader for the format of the file, there was not enough memory to
    allocate the image buffer, or the image file contained invalid data.
  @end{return}
  @begin{short}
    Creates a new pixbuf by loading an image from a file.
  @end{short}
  The file format is detected automatically. The image will be scaled to fit in
  the requested size, optionally preserving the aspect ratio of the image.

  When preserving the aspect ratio, a width of -1 will cause the image to be
  scaled to the exact given height, and a height of -1 will cause the image to
  be scaled to the exact given width. When not preserving the aspect ratio, a
  width or height of -1 means to not scale the image at all in that dimension.
  @see-class{gdk-pixbuf:pixbuf}"
  (glib:with-ignore-g-error (err)
    (%pixbuf-new-from-file-at-scale (namestring path)
                                    width height preserve err)))

(export 'pixbuf-new-from-file-at-scale)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_new_from_resource
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_new_from_resource" %pixbuf-new-from-resource)
    (g:object pixbuf :already-referenced)
  (path :string)
  (err :pointer))

(defun pixbuf-new-from-resource (path)
 #+liber-documentation
 "@version{2024-5-30}
  @argument[path]{a string with the path of the resource file}
  @begin{return}
    The newly created @class{gdk-pixbuf:pixbuf} object, or @code{nil} if any of
    several error conditions occurred: the file could not be opened, the image
    format is not supported, there was not enough memory to allocate the image
    buffer, the stream contained invalid data, or the operation was cancelled.
  @end{return}
  @begin{short}
    Creates a new pixbuf by loading an image from a resource.
  @end{short}
  The file format is detected automatically.
  @see-class{gdk-pixbuf:pixbuf}"
  (glib:with-ignore-g-error (err)
    (%pixbuf-new-from-resource path err)))

(export 'pixbuf-new-from-resource)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_new_from_resource_at_scale
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_new_from_resource_at_scale"
               %pixbuf-new-from-resource-at-scale)
    (g:object pixbuf :already-referenced)
  (path :string)
  (width :int)
  (height :int)
  (preserve :boolean)
  (err :pointer))

(defun pixbuf-new-from-resource-at-scale (path width height preserve)
 #+liber-documentation
 "@version{2024-5-30}
  @argument[path]{a string with the path of the resource file}
  @argument[width]{an integer with the width the image should have or -1 to not
    constrain the width}
  @argument[height]{an integer with the height the image should have or -1 to
    not constrain the height}
  @argument[preserve]{@em{true} to preserve the aspect ratio of the image}
  @begin{return}
    The newly created @class{gdk-pixbuf:pixbuf} object, or @code{nil} if any of
    several error conditions occurred: the file could not be opened, the image
    format is not supported, there was not enough memory to allocate the image
    buffer, the stream contained invalid data, or the operation was cancelled.
  @end{return}
  @begin{short}
    Creates a new pixbuf by loading an image from an resource.
  @end{short}
  The file format is detected automatically.

  The image will be scaled to fit in the requested size, optionally preserving
  the aspect ratio of the image. When preserving the aspect ratio, a width of
  -1 will cause the image to be scaled to the exact given height, and a height
  of -1 will cause the image to be scaled to the exact given width. When not
  preserving the aspect ratio, a width or height of -1 means to not scale the
  image at all in that dimension.
  @see-class{gdk-pixbuf:pixbuf}"
  (glib:with-ignore-g-error (err)
    (%pixbuf-new-from-resource-at-scale path width height preserve err)))

(export 'pixbuf-new-from-resource-at-scale)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_file_info
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_get_file_info" %pixbuf-file-info)
    (:pointer (:struct pixbuf-format))
  (filename :string)
  (width (:pointer :int))
  (height (:pointer :int)))

(defun pixbuf-file-info (path)
 #+liber-documentation
 "@version{2024-5-30}
  @argument[path]{a pathname or namestring with the name of the file to
    identify}
  @begin{return}
    @code{format} -- a @symbol{gdk-pixbuf:pixbuf-format} instance describing
    the image format of the file or @code{nil} if the image format was not
    recognized @br{}
    @code{width} -- an integer with the width of the image, or @code{nil} @br{}
    @code{height} -- an integer with the height of the image, or @code{nil}
  @end{return}
  @begin{short}
    Parses an image file far enough to determine its format and size.
  @end{short}
  @see-class{gdk-pixbuf:pixbuf}
  @see-symbol{gdk-pixbuf:pixbuf-format}"
  (cffi:with-foreign-objects ((width :int) (height :int))
    (let ((format (%pixbuf-file-info (namestring path) width height)))
      (values format
              (cffi:mem-ref width :int)
              (cffi:mem-ref height :int)))))

(export 'pixbuf-file-info)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_file_info_async
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_get_file_info_async" %pixbuf-file-info-async) :void
  (file :string)
  (cancellable (g:object g:cancellable))
  (func :pointer)
  (data :pointer))

(defun pixbuf-file-info-async (path cancellable func)
 #+liber-documentation
 "@version{2024-5-30}
  @argument[path]{a pathname or namestring with the file to identify}
  @argument[cancellable]{a @class{g:cancellable} object, or @code{nil} to
    ignore}
  @argument[func]{a @symbol{g:async-ready-callback} callback function to call
    when the pixbuf is loaded}
  @begin{short}
    Asynchronously parses an image file far enough to determine its format and
    size.
  @end{short}
  For more details see the @fun{gdk-pixbuf:pixbuf-file-info} function, which is
  the synchronous version of this function.

  When the operation is finished, the @arg{func} callback function will be
  called in the main thread. You can then call the
  @fun{gdk-pixbuf:pixbuf-file-info-finish} function to get the result of the
  operation.
  @see-class{g:cancellable}
  @see-symbol{g:async-ready-callback}
  @see-function{gdk-pixbuf:pixbuf-file-info}
  @see-function{gdk-pixbuf:pixbuf-file-info-finish}"
  (let ((ptr (glib:allocate-stable-pointer func)))
    (%pixbuf-file-info-async (namestring path)
                             cancellable
                             (cffi:callback g:async-ready-callback)
                             ptr)))

(export 'pixbuf-file-info-async)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_file_info_finish
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_get_file_info_finish" %pixbuf-file-info-finish)
    (:pointer (:struct pixbuf-format))
  (result (g:object g:async-result))
  (width (:pointer :int))
  (height (:pointer :int))
  (err :pointer))

(defun pixbuf-file-info-finish (result)
 #+liber-documentation
 "@version{2024-5-30}
  @argument[result]{a @class{g:async-result} instance}
  @begin{return}
    @arg{format} -- a @symbol{gdk-pixbuf:pixbuf-format} instance describing the
      image format of the file or @code{nil} if the image format was no
      recognized @br{}
    @arg{width} -- an integer with the width of the image @br{}
    @arg{height} -- an integer with the height of the image
  @end{return}
  @begin{short}
    Finishes an asynchronous pixbuf parsing operation started with the
    @fun{gdk-pixbuf:pixbuf-file-info-async} function.
  @end{short}
  @see-class{g:async-result}
  @see-symbol{gdk-pixbuf:pixbuf-format}
  @see-function{gdk-pixbuf:pixbuf-file-info-async}"
  (glib:with-ignore-g-error (err)
    (cffi:with-foreign-objects ((width :int) (height :int))
      (let ((format (%pixbuf-file-info-finish result width height err)))
        (when format
          (values format
                  (cffi:mem-ref width :int)
                  (cffi:mem-ref height :int)))))))

(export 'pixbuf-file-info-finish)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_new_from_stream ()
;;;
;;; GdkPixbuf * gdk_pixbuf_new_from_stream (GInputStream *stream,
;;;                                         GCancellable *cancellable,
;;;                                         GError **error);
;;;
;;; Creates a new pixbuf by loading an image from an input stream.
;;;
;;; The file format is detected automatically. If NULL is returned, then error
;;; will be set. The cancellable can be used to abort the operation from another
;;; thread. If the operation was cancelled, the error GIO_ERROR_CANCELLED will
;;; be returned. Other possible errors are in the GDK_PIXBUF_ERROR and
;;; G_IO_ERROR domains.
;;;
;;; The stream is not closed.
;;;
;;; stream :
;;;     a GInputStream to load the pixbuf from
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore
;;;
;;; error :
;;;     Return location for an error
;;;
;;; Returns :
;;;     A newly created pixbuf, or NULL if any of several error conditions
;;;     occurred: the file could not be opened, the image format is not
;;;     supported, there was not enough memory to allocate the image buffer,
;;;     the stream contained invalid data, or the operation was cancelled.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_new_from_stream_async ()
;;;
;;; void
;;; gdk_pixbuf_new_from_stream_async (GInputStream *stream,
;;;                                   GCancellable *cancellable,
;;;                                   GAsyncReadyCallback callback,
;;;                                   gpointer user_data);
;;;
;;; Creates a new pixbuf by asynchronously loading an image from an input
;;; stream.
;;;
;;; For more details see gdk_pixbuf_new_from_stream(), which is the synchronous
;;; version of this function.
;;;
;;; When the operation is finished, callback will be called in the main thread.
;;; You can then call gdk_pixbuf_new_from_stream_finish() to get the result of
;;; the operation.
;;;
;;; stream :
;;;     a GInputStream from which to load the pixbuf
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore.
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the the pixbuf is loaded
;;;
;;; user_data :
;;;     the data to pass to the callback function
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_new_from_stream_finish ()
;;;
;;; GdkPixbuf *
;;; gdk_pixbuf_new_from_stream_finish (GAsyncResult *async_result,
;;;                                    GError **error);
;;;
;;; Finishes an asynchronous pixbuf creation operation started with
;;; gdk_pixbuf_new_from_stream_async().
;;;
;;; async_result :
;;;     a GAsyncResult
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     a GdkPixbuf or NULL on error. Free the returned object with
;;;     g_object_unref().
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_new_from_stream_at_scale ()
;;;
;;; GdkPixbuf * gdk_pixbuf_new_from_stream_at_scale
;;;                                             (GInputStream *stream,
;;;                                              gint width,
;;;                                              gint height,
;;;                                              gboolean preserve_aspect_ratio,
;;;                                              GCancellable *cancellable,
;;;                                              GError **error);
;;;
;;; Creates a new pixbuf by loading an image from an input stream.
;;;
;;; The file format is detected automatically. If NULL is returned, then error
;;; will be set. The cancellable can be used to abort the operation from another
;;; thread. If the operation was cancelled, the error GIO_ERROR_CANCELLED will
;;; be returned. Other possible errors are in the GDK_PIXBUF_ERROR and
;;; G_IO_ERROR domains.
;;;
;;; The image will be scaled to fit in the requested size, optionally preserving
;;; the image's aspect ratio. When preserving the aspect ratio, a width of -1
;;; will cause the image to be scaled to the exact given height, and a height
;;; of -1 will cause the image to be scaled to the exact given width. When not
;;; preserving aspect ratio, a width or height of -1 means to not scale the
;;; image at all in that dimension.
;;;
;;; The stream is not closed.
;;;
;;; stream :
;;;     a GInputStream to load the pixbuf from
;;;
;;; width :
;;;     The width the image should have or -1 to not constrain the width
;;;
;;; height :
;;;     The height the image should have or -1 to not constrain the height
;;;
;;; preserve_aspect_ratio :
;;;     TRUE to preserve the image's aspect ratio
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore
;;;
;;; error :
;;;     Return location for an error
;;;
;;; Returns :
;;;     A newly created pixbuf, or NULL if any of several error conditions
;;;     occurred: the file could not be opened, the image format is not
;;;     supported, there was not enough memory to allocate the image buffer,
;;;     the stream contained invalid data, or the operation was cancelled.
;;;
;;; Since 2.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_new_from_stream_at_scale_async ()
;;;
;;; void
;;; gdk_pixbuf_new_from_stream_at_scale_async
;;;                                (GInputStream *stream,
;;;                                 gint width,
;;;                                 gint height,
;;;                                 gboolean preserve_aspect_ratio,
;;;                                 GCancellable *cancellable,
;;;                                 GAsyncReadyCallback callback,
;;;                                 gpointer user_data);
;;;
;;; Creates a new pixbuf by asynchronously loading an image from an input
;;; stream.
;;;
;;; For more details see gdk_pixbuf_new_from_stream_at_scale(), which is the
;;; synchronous version of this function.
;;;
;;; When the operation is finished, callback will be called in the main thread.
;;; You can then call gdk_pixbuf_new_from_stream_finish() to get the result of
;;; the operation.
;;;
;;; stream :
;;;     a GInputStream from which to load the pixbuf
;;;
;;; width :
;;;     the width the image should have or -1 to not constrain the width
;;;
;;; height :
;;;     the height the image should have or -1 to not constrain the height
;;;
;;; preserve_aspect_ratio :
;;;     TRUE to preserve the image's aspect ratio
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore.
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the the pixbuf is loaded
;;;
;;; user_data :
;;;     the data to pass to the callback function
;;;
;;; Since 2.24
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk-pixbuf.load.lisp ---------------------------------------
