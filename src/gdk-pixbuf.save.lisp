;;; ----------------------------------------------------------------------------
;;; gdk-pixbuf.save.lisp
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
;;; File saving
;;;
;;;     Saving a pixbuf to a file.
;;;
;;; Functions
;;;
;;;     gdk_pixbuf_savev
;;;     gdk_pixbuf_save
;;;     GdkPixbufSaveFunc
;;;     gdk_pixbuf_save_to_callback
;;;     gdk_pixbuf_save_to_callbackv
;;;     gdk_pixbuf_save_to_buffer
;;;     gdk_pixbuf_save_to_bufferv
;;;     gdk_pixbuf_save_to_stream
;;;     gdk_pixbuf_save_to_streamv
;;;     gdk_pixbuf_save_to_stream_async
;;;     gdk_pixbuf_save_to_streamv_async
;;;     gdk_pixbuf_save_to_stream_finish
;;; ----------------------------------------------------------------------------

(in-package :gdk-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_save
;;; gdk_pixbuf_savev                                        not exported
;;; ----------------------------------------------------------------------------

;; TODO: Replace the examples with Lisp code

;; This implementation does not support the arguments error and Varargs.

(cffi:defcfun ("gdk_pixbuf_savev" %pixbuf-savev) :boolean
  (pixbuf (g:object pixbuf))
  (filename :string)
  (type :string)
  (option-keys (:pointer (:pointer :char)))
  (option-values (:pointer (:pointer :char)))
  (error :pointer))

(defun pixbuf-save (pixbuf filename type)
 #+liber-documentation
 "@version{#2024-6-29}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object}
  @argument[filename]{a string with the name of file to save}
  @argument[type]{a string with the name of file format}
  @return{The boolean whether an error was set.}
  @begin{short}
    Saves pixbuf to a file in format @arg{type}.
  @end{short}
  By default, \"jpeg\", \"png\", \"ico\" and \"bmp\" are possible file formats
  to save in, but more formats may be installed. The list of all writable
  formats can be determined in the following way:
  @begin{pre}
void add_if_writable (GdkPixbufFormat *data, GSList **list)
{
  if (gdk_pixbuf_format_is_writable (data))
    *list = g_slist_prepend (*list, data);
@}

GSList *formats = gdk_pixbuf_get_formats ();
GSList *writable_formats = NULL;
g_slist_foreach (formats, add_if_writable, &writable_formats);
g_slist_free (formats);
  @end{pre}
  If error is set, @code{nil} will be returned. Possible errors include those
  in the @code{GDK_PIXBUF_ERROR} domain and those in the @code{G_FILE_ERROR}
  domain.

  The variable argument list should be NULL-terminated; if not empty, it
  should contain pairs of strings that modify the save parameters.
  For example:
  @begin{pre}
gdk_pixbuf_save (pixbuf, handle, \"jpeg\", &error,
                \"quality\", \"100\", NULL);
  @end{pre}
  Currently only few parameters exist. JPEG images can be saved with a
  \"quality\" parameter; its value should be in the range [0,100]. JPEG and PNG
  density can be set by setting the \"x-dpi\" and \"y-dpi\" parameters to the
  appropriate values in dots per inch.

  Text chunks can be attached to PNG images by specifying parameters of the
  form \"tEXt::key\", where key is an ASCII string of length 1-79. The values
  are UTF-8 encoded strings. The PNG compression level can be specified using
  the \"compression\" parameter; it's value is in an integer in the range of
  [0,9].

  ICC color profiles can also be embedded into PNG and TIFF images. The
  \"icc-profile\" value should be the complete ICC profile encoded into base64.
  @begin{pre}
gchar *contents;
gchar *contents_encode;
gsize length;
g_file_get_contents (\"/home/hughsie/.color/icc/L225W.icm\",
                     &contents, &length, NULL);
contents_encode = g_base64_encode ((const guchar *) contents, length);
gdk_pixbuf_save (pixbuf, handle, \"png\", &error,
                 \"icc-profile\", contents_encode,
                 NULL);
  @end{pre}
  TIFF images recognize: (1) a \"bits-per-sample\" option (integer) which can
  be either 1 for saving bi-level CCITTFAX4 images, or 8 for saving 8-bits per
  sample; (2) a \"compression\" option (integer) which can be 1 for no
  compression, 2 for Huffman, 5 for LZW, 7 for JPEG and 8 for DEFLATE (see the
  libtiff documentation and tiff.h for all supported codec values); (3) an
  \"icc-profile\" option (zero-terminated string) containing a base64 encoded
  ICC color profile.

  ICO images can be saved in depth 16, 24, or 32, by using the \"depth\"
  parameter. When the ICO saver is given \"x_hot\" and \"y_hot\" parameters, it
  produces a CUR instead of an ICO.
  @see-class{gdk-pixbuf:pixbuf}"
  (%pixbuf-savev pixbuf
                 (etypecase filename
                   (string filename)
                   (pathname (namestring filename)))
                 type
                 (cffi:null-pointer)
                 (cffi:null-pointer)
                 (cffi:null-pointer)))

(export 'pixbuf-save)

;;; ----------------------------------------------------------------------------
;;; GdkPixbufSaveFunc ()
;;;
;;; gboolean (*GdkPixbufSaveFunc) (const gchar *buf,
;;;                                gsize count,
;;;                                GError **error,
;;;                                gpointer data);
;;;
;;; Specifies the type of the function passed to gdk_pixbuf_save_to_callback().
;;; It is called once for each block of bytes that is "written" by
;;; gdk_pixbuf_save_to_callback(). If successful it should return TRUE. If an
;;; error occurs it should set error and return FALSE, in which case
;;; gdk_pixbuf_save_to_callback() will fail with the same error.
;;;
;;; buf :
;;;     bytes to be written
;;;
;;; count :
;;;     number of bytes in buf.
;;;
;;; error :
;;;     A location to return an error
;;;
;;; data :
;;;     user data passed to gdk_pixbuf_save_to_callback()
;;;
;;; Returns :
;;;     TRUE if successful, FALSE (with error set) if failed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_save_to_callback ()
;;;
;;; gboolean gdk_pixbuf_save_to_callback (GdkPixbuf *pixbuf,
;;;                                       GdkPixbufSaveFunc save_func,
;;;                                       gpointer user_data,
;;;                                       const char *type,
;;;                                       GError **error,
;;;                                       ...);
;;;
;;; Saves pixbuf in format type by feeding the produced data to a callback.
;;; Can be used when you want to store the image to something other than a file,
;;; such as an in-memory buffer or a socket. If error is set, FALSE will be
;;; returned. Possible errors include those in the GDK_PIXBUF_ERROR domain and
;;; whatever the save function generates.
;;;
;;; See gdk_pixbuf_save() for more details.
;;;
;;; pixbuf :
;;;     a GdkPixbuf.
;;;
;;; save_func :
;;;     a function that is called to save each block of data that the save
;;;     routine generates
;;;
;;; user_data :
;;;     user data to pass to the save function.
;;;
;;; type :
;;;     name of file format.
;;;
;;; error :
;;;     return location for error, or NULL
;;;
;;; Varargs :
;;;     list of key-value save options
;;;
;;; Returns :
;;;     whether an error was set
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_save_to_callbackv ()
;;;
;;; gboolean gdk_pixbuf_save_to_callbackv (GdkPixbuf *pixbuf,
;;;                                        GdkPixbufSaveFunc save_func,
;;;                                        gpointer user_data,
;;;                                        const char *type,
;;;                                        char **option_keys,
;;;                                        char **option_values,
;;;                                        GError **error);
;;;
;;; Saves pixbuf to a callback in format type, which is currently "jpeg",
;;; "png", "tiff", "ico" or "bmp". If error is set, FALSE will be returned.
;;; See gdk_pixbuf_save_to_callback() for more details.
;;;
;;; pixbuf :
;;;     a GdkPixbuf.
;;;
;;; save_func :
;;;     a function that is called to save each block of data that the save
;;;     routine generates.
;;;
;;; user_data :
;;;     user data to pass to the save function
;;;
;;; type :
;;;     name of file format.
;;;
;;; option_keys :
;;;     name of options to set, NULL-terminated.
;;;
;;; option_values :
;;;     values for named options.
;;;
;;; error :
;;;     return location for error, or NULL
;;;
;;; Returns :
;;;     whether an error was set
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_save_to_buffer ()
;;;
;;; gboolean gdk_pixbuf_save_to_buffer (GdkPixbuf *pixbuf,
;;;                                     gchar **buffer,
;;;                                     gsize *buffer_size,
;;;                                     const char *type,
;;;                                     GError **error,
;;;                                     ...);
;;;
;;; Saves pixbuf to a new buffer in format type, which is currently "jpeg",
;;; "png", "tiff", "ico" or "bmp". This is a convenience function that
;;; uses gdk_pixbuf_save_to_callback() to do the real work. Note that the
;;; buffer is not nul-terminated and may contain embedded nuls. If error is
;;; set, FALSE will be returned and buffer will be set to NULL. Possible errors
;;; include those in the GDK_PIXBUF_ERROR domain.
;;;
;;; See gdk_pixbuf_save() for more details.
;;;
;;; pixbuf :
;;;     a GdkPixbuf.
;;;
;;; buffer :
;;;     location to receive a pointer to the new buffer.
;;;
;;; buffer_size :
;;;     location to receive the size of the new buffer.
;;;
;;; type :
;;;     name of file format.
;;;
;;; error :
;;;     return location for error, or NULL.
;;;
;;; Varargs :
;;;     list of key-value save options
;;;
;;; Returns :
;;;     whether an error was set
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_save_to_bufferv ()
;;;
;;; gboolean gdk_pixbuf_save_to_bufferv (GdkPixbuf *pixbuf,
;;;                                      gchar **buffer,
;;;                                      gsize *buffer_size,
;;;                                      const char *type,
;;;                                      char **option_keys,
;;;                                      char **option_values,
;;;                                      GError **error);
;;;
;;; Saves pixbuf to a new buffer in format type, which is currently "jpeg",
;;; "tiff", "png", "ico" or "bmp". See gdk_pixbuf_save_to_buffer() for more
;;; details.
;;;
;;; pixbuf :
;;;     a GdkPixbuf.
;;;
;;; buffer :
;;;     location to receive a pointer to the new buffer.
;;;
;;; buffer_size :
;;;     location to receive the size of the new buffer.
;;;
;;; type :
;;;     name of file format.
;;;
;;; option_keys :
;;;     name of options to set, NULL-terminated.
;;;
;;; option_values :
;;;     values for named options.
;;;
;;; error :
;;;     return location for error, or NULL.
;;;
;;; Returns :
;;;     whether an error was set
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_save_to_stream ()
;;;
;;; gboolean gdk_pixbuf_save_to_stream (GdkPixbuf *pixbuf,
;;;                                     GOutputStream *stream,
;;;                                     const char *type,
;;;                                     GCancellable *cancellable,
;;;                                     GError **error,
;;;                                     ...);
;;;
;;; Saves pixbuf to an output stream.
;;;
;;; Supported file formats are currently "jpeg", "tiff", "png", "ico" or "bmp".
;;; See gdk_pixbuf_save_to_buffer() for more details.
;;;
;;; The cancellable can be used to abort the operation from another thread. If
;;; the operation was cancelled, the error GIO_ERROR_CANCELLED will be returned.
;;; Other possible errors are in the GDK_PIXBUF_ERROR and G_IO_ERROR domains.
;;;
;;; The stream is not closed.
;;;
;;; pixbuf :
;;;     a GdkPixbuf
;;;
;;; stream :
;;;     a GOutputStream to save the pixbuf to
;;;
;;; type :
;;;     name of file format
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore
;;;
;;; error :
;;;     return location for error, or NULL
;;;
;;; Varargs :
;;;     list of key-value save options
;;;
;;; Returns :
;;;     TRUE if the pixbuf was saved successfully, FALSE if an error was set.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_save_to_streamv ()
;;;
;;; gboolean
;;; gdk_pixbuf_save_to_streamv (GdkPixbuf *pixbuf,
;;;                             GOutputStream *stream,
;;;                             const char *type,
;;;                             char **option_keys,
;;;                             char **option_values,
;;;                             GCancellable *cancellable,
;;;                             GError **error);
;;;
;;; Saves pixbuf to an output stream.
;;;
;;; Supported file formats are currently "jpeg", "tiff", "png", "ico" or "bmp".
;;; See gdk_pixbuf_save_to_stream() for more details.
;;;
;;; pixbuf :
;;;     a GdkPixbuf
;;;
;;; stream :
;;;     a GOutputStream to save the pixbuf to
;;;
;;; type :
;;;     name of file format
;;;
;;; option_keys :
;;;     name of options to set, NULL-terminated.
;;;
;;; option_values :
;;;     values for named options.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore.
;;;
;;; error :
;;;     return location for error, or NULL.
;;;
;;; Returns :
;;;     TRUE if the pixbuf was saved successfully, FALSE if an error was set.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_save_to_stream_async ()
;;;
;;; void
;;; gdk_pixbuf_save_to_stream_async (GdkPixbuf *pixbuf,
;;;                                  GOutputStream *stream,
;;;                                  const gchar *type,
;;;                                  GCancellable *cancellable,
;;;                                  GAsyncReadyCallback callback,
;;;                                  gpointer user_data,
;;;                                  ...);
;;;
;;; Saves pixbuf to an output stream asynchronously.
;;;
;;; For more details see gdk_pixbuf_save_to_stream(), which is the synchronous
;;; version of this function.
;;;
;;; When the operation is finished, callback will be called in the main thread.
;;; You can then call gdk_pixbuf_save_to_stream_finish() to get the result of
;;; the operation.
;;;
;;; pixbuf :
;;;     a GdkPixbuf
;;;
;;; stream :
;;;     a GOutputStream to which to save the pixbuf
;;;
;;; type :
;;;     name of file format
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
;;; ... :
;;;     list of key-value save options
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_save_to_streamv_async ()
;;;
;;; void
;;; gdk_pixbuf_save_to_streamv_async (GdkPixbuf *pixbuf,
;;;                                   GOutputStream *stream,
;;;                                   const gchar *type,
;;;                                   gchar **option_keys,
;;;                                   gchar **option_values,
;;;                                   GCancellable *cancellable,
;;;                                   GAsyncReadyCallback callback,
;;;                                   gpointer user_data);
;;;
;;; Saves pixbuf to an output stream asynchronously.
;;;
;;; For more details see gdk_pixbuf_save_to_streamv(), which is the synchronous
;;; version of this function.
;;;
;;; When the operation is finished, callback will be called in the main thread.
;;; You can then call gdk_pixbuf_save_to_stream_finish() to get the result of
;;; the operation.
;;;
;;; pixbuf :
;;;     a GdkPixbuf
;;;
;;; stream :
;;;     a GOutputStream to which to save the pixbuf
;;;
;;; type :
;;;     name of file format
;;;
;;; option_keys :
;;;     name of options to set, NULL-terminated.
;;;
;;; option_values :
;;;     values for named options.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore.
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the the pixbuf is loaded
;;;
;;; user_data :
;;;     the data to pass to the callback function
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_save_to_stream_finish ()
;;;
;;; gboolean
;;; gdk_pixbuf_save_to_stream_finish (GAsyncResult *async_result,
;;;                                   GError **error);
;;;
;;; Finishes an asynchronous pixbuf save operation started with
;;; gdk_pixbuf_save_to_stream_async().
;;;
;;; async_result :
;;;     a GAsyncResult
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     TRUE if the pixbuf was saved successfully, FALSE if an error was set.
;; -----------------------------------------------------------------------------

;;; --- End of file gdk-pixbuf.save.lisp ---------------------------------------
