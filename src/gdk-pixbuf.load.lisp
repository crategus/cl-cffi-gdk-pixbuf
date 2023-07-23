;;; ----------------------------------------------------------------------------
;;; gdk-pixbuf.load.lisp
;;;
;;; The documentation of this file is taken from the GDK-PixBuf Reference Manual
;;; Version 2.36 and modified to document the Lisp binding to the GDK-PixBuf
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
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
;;;     gdk_pixbuf_get_file_info
;;;     gdk_pixbuf_get_file_info_async
;;;     gdk_pixbuf_get_file_info_finish
;;;     gdk_pixbuf_new_from_resource
;;;     gdk_pixbuf_new_from_resource_at_scale
;;;     gdk_pixbuf_new_from_stream
;;;     gdk_pixbuf_new_from_stream_async
;;;     gdk_pixbuf_new_from_stream_finish
;;;     gdk_pixbuf_new_from_stream_at_scale
;;;     gdk_pixbuf_new_from_stream_at_scale_async
;;;
;;; Description
;;;
;;; The gdk-pixbuf library provides a simple mechanism for loading an image
;;; from a file in synchronous fashion. This means that the library takes
;;; control of the application while the file is being loaded; from the user's
;;; point of view, the application will block until the image is done loading.
;;;
;;; This interface can be used by applications in which blocking is acceptable
;;; while an image is being loaded. It can also be used to load small images in
;;; general. Applications that need progressive loading can use the
;;; GdkPixbufLoader functionality instead.
;;; ----------------------------------------------------------------------------

(in-package :gdk-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_new_from_file ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_new_from_file" %pixbuf-new-from-file)
    (g:object pixbuf :already-referenced)
  (filename :string)
  (err :pointer))

(defun pixbuf-new-from-file (path)
 #+liber-documentation
 "@version{2023-1-26}
  @argument[path]{a pathname or namestring with the file to load, in the GLib
    file name encoding}
  @begin{return}
    A newly created @class{gdk-pixbuf:pixbuf} object, or @code{nil} if any of
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
;;; gdk_pixbuf_new_from_file_at_size ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_new_from_file_at_size" %pixbuf-new-from-file-at-size)
    (g:object pixbuf)
  (filename :string)
  (width :int)
  (height :int)
  (err :pointer))

(defun pixbuf-new-from-file-at-size (path width height)
 #+liber-documentation
 "@version{2023-1-26}
  @argument[path]{a pathname or namestring with the file to load, in the GLib
    file name encoding}
  @argument[width]{an integer with the width the image should have or -1 to not
    constrain the width}
  @argument[height]{an integer with the height the image should have or -1 to
    not constrain the height}
  @begin{return}
    A newly created @class{gdk-pixbuf:pixbuf} object, or @code{nil} if any of
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
;;; gdk_pixbuf_new_from_file_at_scale ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_new_from_file_at_scale"
               %pixbuf-new-from-file-at-scale) (g:object pixbuf)
  (filename :string)
  (width :int)
  (height :int)
  (preserve :boolean)
  (err :pointer))

(defun pixbuf-new-from-file-at-scale (path width height preserve)
 #+liber-documentation
 "@version{2023-1-26}
  @argument[path]{a pathname or namestring with the file to load, in the GLib
    file name encoding}
  @argument[width]{an integer with the width the image should have or -1 to not
    constrain the width}
  @argument[height]{an integer with the height the image should have or -1 to
    not constrain the height}
  @argument[preserve]{@em{true} to preserve the aspect ratio of the image}
  @begin{return}
    A newly created @class{gdk-pixbuf:pixbuf} object, or @code{nil} if any of
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
;;; gdk_pixbuf_get_file_info () -> pixbuf-file-info
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_get_file_info" %pixbuf-file-info)
    (:pointer (:struct pixbuf-format))
  (filename :string)
  (width (:pointer :int))
  (height (:pointer :int)))

(defun pixbuf-file-info (path)
 #+liber-documentation
 "@version{2023-1-26}
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
;;; gdk_pixbuf_get_file_info_async ()
;;;
;;; void
;;; gdk_pixbuf_get_file_info_async (const gchar *filename,
;;;                                 GCancellable *cancellable,
;;;                                 GAsyncReadyCallback callback,
;;;                                 gpointer user_data);
;;;
;;; Asynchronously parses an image file far enough to determine its format and
;;; size.
;;;
;;; For more details see gdk_pixbuf_get_file_info(), which is the synchronous
;;; version of this function.
;;;
;;; When the operation is finished, callback will be called in the main thread.
;;; You can then call gdk_pixbuf_get_file_info_finish() to get the result of
;;; the operation.
;;;
;;; filename :
;;;     The name of the file to identify
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
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_get_file_info_finish ()
;;;
;;; GdkPixbufFormat *
;;; gdk_pixbuf_get_file_info_finish (GAsyncResult *async_result,
;;;                                  gint *width,
;;;                                  gint *height,
;;;                                  GError **error);
;;;
;;; Finishes an asynchronous pixbuf parsing operation started with
;;; gdk_pixbuf_get_file_info_async().
;;;
;;; async_result :
;;;     a GAsyncResult
;;;
;;; width :
;;;     Return location for the width of the image, or NULL.
;;;
;;; height :
;;;     Return location for the height of the image, or NULL.
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     A GdkPixbufFormat describing the image format of the file or NULL if
;;;     the image format wasn't recognized. The return value is owned by
;;;     GdkPixbuf and should not be freed.
;;;
;;; Since 2.32
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_new_from_resource ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_new_from_resource" %pixbuf-new-from-resource)
    (g:object pixbuf)
  (path :string)
  (err :pointer))

(defun pixbuf-new-from-resource (path)
 #+liber-documentation
 "@version{#2021-7-24}
  @argument[path]{a string with the path of the resource file}
  @begin{return}
    A newly created @class{gdk-pixbuf:pixbuf} object, or @code{nil} if any of
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
;;; gdk_pixbuf_new_from_resource_at_scale ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_new_from_resource_at_scale"
               %pixbuf-new-from-resource-at-scale) (g:object pixbuf)
  (path :string)
  (width :int)
  (height :int)
  (preserve :boolean)
  (err :pointer))

(defun pixbuf-new-from-resource-at-scale (path width height preserve)
 #+liber-documentation
 "@version{#2021-7-24}
  @argument[path]{a string with the path of the resource file}
  @argument[width]{an integer with the width the image should have or -1 to not
    constrain the width}
  @argument[height]{an integer with the height the image should have or -1 to
    not constrain the height}
  @argument[preserve]{@em{true} to preserve the aspect ratio of the image}
  @begin{return}
    A newly created @class{gdk-pixbuf:pixbuf} object, or @code{nil} if any of
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
