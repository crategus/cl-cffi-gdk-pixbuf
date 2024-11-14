;;; ----------------------------------------------------------------------------
;;; gdk-pixbuf.animation.lisp
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
;;; Animations
;;;
;;;     Animated images.
;;;
;;; Types and Values
;;;
;;;     GdkPixbufAnimation
;;;     GdkPixbufAnimationIter
;;;     GdkPixbufSimpleAnim
;;;
;;; Functions
;;;
;;;     gdk_pixbuf_animation_iter_get_pixbuf
;;;     gdk_pixbuf_animation_iter_get_delay_time
;;;     gdk_pixbuf_animation_iter_advance
;;;     gdk_pixbuf_animation_iter_on_currently_loading_frame
;;;
;;;     gdk_pixbuf_animation_new_from_file
;;;     gdk_pixbuf_animation_new_from_resource
;;;     gdk_pixbuf_animation_new_from_stream
;;;     gdk_pixbuf_animation_new_from_stream_async
;;;     gdk_pixbuf_animation_new_from_stream_finish
;;;     gdk_pixbuf_animation_ref
;;;     gdk_pixbuf_animation_unref
;;;     gdk_pixbuf_animation_get_width
;;;     gdk_pixbuf_animation_get_height
;;;     gdk_pixbuf_animation_get_iter
;;;     gdk_pixbuf_animation_is_static_image
;;;     gdk_pixbuf_animation_get_static_image
;;;
;;;     gdk_pixbuf_simple_anim_new
;;;     gdk_pixbuf_simple_anim_add_frame
;;;     gdk_pixbuf_simple_anim_set_loop
;;;     gdk_pixbuf_simple_anim_get_loop
;;;
;;; Properties
;;;
;;;     loop
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ├── GdkPixbufAnimation
;;;     │   ╰── GdkPixbufSimpleAnim
;;;     ╰── GdkPixbufAnimationIter
;;;
;;; ----------------------------------------------------------------------------

(in-package :gdk-pixbuf)

;;; ----------------------------------------------------------------------------
;;; GdkPixbufAnimationIter
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GdkPixbufAnimationIter" pixbuf-animation-iter
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_pixbuf_animation_iter_get_type")
  nil)

#+liber-documentation
(setf (documentation 'pixbuf-animation-iter 'type)
 "@version{#2024-11-11}
  @begin{short}
    An opaque object representing an iterator which points to a certain position
    in an animation.
  @end{short}
  @see-class{gdk-pixbuf:pixbuf-animation}")

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_animation_iter_get_pixbuf
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_animation_iter_get_pixbuf"
               pixbuf-animation-iter-pixbuf) (g:object pixbuf)
 #+liber-documentation
 "@version{#2024-11-11}
  @argument[iter]{a @class{gdk-pixbuf:pixbuf-animation-iter} iterator}
  @return{The @class{gdk-pixbuf:pixbuf} object to be displayed.}
  @begin{short}
    Gets the current pixbuf which should be displayed.
  @end{short}
  The pixbuf might not be the same size as the animation itself which can be
  get with the @fun{gdk-pixbuf:pixbuf-animation-width} and the
  @fun{gdk-pixbuf:pixbuf-animation-height} functions. This pixbuf should be
  displayed for @fun{gdk-pixbuf:pixbuf-animation-iter-delay-time} milliseconds.

  The caller of this function does not own a reference to the returned pixbuf.
  The returned pixbuf will become invalid when the iterator advances to the next
  frame, which may happen anytime you call the
  @fun{gdk-pixbuf:pixbuf-animation-iter-advance} function. Copy the pixbuf to
  keep it, do not just add a reference, as it may get recycled as you advance
  the iterator.
  @see-class{gdk-pixbuf:pixbuf-animation-iter}
  @see-class{gdk-pixbuf:pixbuf}
  @see-function{gdk-pixbuf:pixbuf-animation-width}
  @see-function{gdk-pixbuf:pixbuf-animation-height}
  @see-function{gdk-pixbuf:pixbuf-animation-iter-delay-time}
  @see-function{gdk-pixbuf:pixbuf-animation-iter-advance}"
  (iter (g:object pixbuf-animation-iter)))

(export 'pixbuf-animation-iter-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_animation_iter_get_delay_time
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_animation_iter_get_delay_time"
               pixbuf-animation-iter-delay-time) :int
 #+liber-documentation
 "@version{#2024-11-11}
  @argument[iter]{a @class{gdk-pixbuf:pixbuf-animation-iter} iterator}
  @return{The integer with the delay time in milliseconds.}
  @begin{short}
    Gets the number of milliseconds the current pixbuf should be displayed, or
    -1 if the current pixbuf should be displayed forever.
  @end{short}
  The @fun{g:timeout-add} function conveniently takes a timeout in milliseconds,
  so you can use a timeout to schedule the next update.
  @see-class{gdk-pixbuf:pixbuf-animation-iter}
  @see-function{g:timeout-add}"
  (iter (g:object pixbuf-animation-iter)))

(export 'pixbuf-animation-iter-delay-time)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_animation_iter_advance
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_animation_iter_advance"
               pixbuf-animation-iter-advance) :boolean
 #+liber-documentation
 "@version{#2024-11-11}
  @argument[iter]{a @class{gdk-pixbuf:pixbuf-animation-iter} iterator}
  @argument[time]{an unsigned integer with the current time}
  @return{@em{True} if the image may need updating.}
  @begin{short}
    Possibly advances an animation to a new frame.
  @end{short}
  Chooses the frame based on the start time passed to the
  @fun{gdk-pixbuf:pixbuf-animation-iter} function.

  The @arg{time} argument would normally come from the
  @code{g_get_current_time()} function, and must be greater than or equal to the
  time passed to the @fun{gdk-pixbuf:pixbuf-animation-iter} function, and must
  increase or remain unchanged each time the
  @fun{gdk-pixbuf:pixbuf-animation-iter-pixbuf} function is called. That is, you
  cannot go backward in time, animations only play forward.

  As a shortcut, pass @code{nil} for the current time and the
  @code{g_get_current_time()} function will be invoked on your behalf. So you
  only need to explicitly pass @arg{time} if you are doing something odd like
  playing the animation at double speed.

  If this function returns @em{false}, there is no need to update the animation
  display, assuming the display had been rendered prior to advancing. If
  @em{true}, you need to call the @fun{gdk-pixbuf:pixbuf-animation-iter-pixbuf}
  function and update the display with the new pixbuf.
  @see-class{gdk-pixbuf:pixbuf-animation-iter}
  @see-function{gdk-pixbuf:pixbuf-animation-iter-pixbuf}"
  (iter (g:object pixbuf-animation-iter))
  (time :uint64))

(export 'pixbuf-animation-iter-advance)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_animation_iter_on_currently_loading_frame
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_animation_iter_on_currently_loading_frame"
               pixbuf-animation-iter-on-currently-loading-frame) :boolean
 #+liber-documentation
 "@version{#2024-11-11}
  @argument[iter]{a @class{gdk-pixbuf:pixbuf-animation-iter} iterator}
  @return{@em{True} if the frame we are on is partially loaded, or the last
    frame.}
  @begin{short}
    Used to determine how to respond to the @code{\"area-updated\"} signal on
    the @class{gdk-pixbuf:pixbuf-loader} object when loading an animation.
  @end{short}
  The @code{\"area-updated\"} signal is emitted for an area of the frame
  currently streaming in to the loader. So if you are on the currently loading
  frame, you need to redraw the screen for the updated area.
  @see-class{gdk-pixbuf:pixbuf-animation-iter}
  @see-class{gdk-pixbuf:pixbuf-loader}"
  (iter (g:object pixbuf-animation-iter)))

(export 'pixbuf-animation-iter-on-currently-loading-frame)

;;; ----------------------------------------------------------------------------
;;; GdkPixbufAnimation
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GdkPixbufAnimation" pixbuf-animation
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_pixbuf_animation_get_type")
  ((loop
    pixbuf-animation-loop
    "loop" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'pixbuf-animation 'type)
 "@version{#2024-6-29}
  @begin{short}
    The @class{gdk-pixbuf:pixbuf} library provides a simple mechanism to load
    and represent animations.
  @end{short}
  An animation is conceptually a series of frames to be displayed over time.
  The animation may not be represented as a series of frames internally. For
  example, it may be stored as a sprite and instructions for moving the sprite
  around a background. To display an animation you do not need to understand
  its representation, however. You just ask the @class{gdk-pixbuf:pixbuf} object
  what should be displayed at a given point in time.
  @see-constructor{gdk-pixbuf:pixbuf-animation-new-from-file}
  @see-constructor{gdk-pixbuf:pixbuf-animation-new-from-resource}
  @see-slot{gdk-pixbuf:pixbuf-animation-loop}
  @see-class{gdk-pixbuf:pixbuf}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "loop" 'pixbuf-animation) t)
 "The @code{loop} property of type @code{:boolean} (Read / Write) @br{}
  Whether the animation should loop when it reaches the end. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'pixbuf-animation-loop)
      "Accessor"
      (documentation 'pixbuf-animation-loop 'function)
 "@version{#2024-6-29}
  @syntax{(gdk-pixbuf:pixbuf-animation-loop object) => loop}
  @syntax{(setf (gdk-pixbuf:pixbuf-animation-loop object) loop)}
  @argument[object]{a @class{gdk-pixbuf:pixbuf-animation} object}
  @argument[loop]{a boolean whether the animation should loop}
  @begin{short}
    Accessor of the @slot[gdk-pixbuf:pixbuf-animation]{loop} of the
    @class{gdk-pixbuf:pixbuf-animation} class.
  @end{short}
  Whether the animation should loop when it reaches the end.
  @see-class{gdk-pixbuf:pixbuf-animation}")

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_animation_new_from_file
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_animation_new_from_file"
               %pixbuf-animation-new-from-file) (g:object pixbuf-animation)
  (filename :string)
  (error :pointer))

(defun pixbuf-animation-new-from-file (path)
 #+liber-documentation
 "@version{2024-6-29}
  @argument[path]{a pathname or namestring with the file to load, in the GLib
    file name encoding}
  @begin{return}
    The newly created @class{gdk-pixbuf:pixbuf-animation} object, or @code{nil}
    if any of several error conditions ocurred: the file could not be opened,
    there was no loader for the format of the file, there was not enough memory
    to allocate the image buffer, or the image file contained invalid data.
  @end{return}
  @begin{short}
    Creates a new animation by loading it from a file.
  @end{short}
  The file format is detected automatically. If the format of the file does not
  support multi-frame images, then an animation with a single frame will be
  created. Possible errors are in the @code{GDK_PIXBUF_ERROR} and
  @code{G_FILE_ERROR} domains.
  @see-class{gdk-pixbuf:pixbuf-animation}"
  (glib:with-g-error (err)
    (%pixbuf-animation-new-from-file (namestring path) err)))

(export 'pixbuf-animation-new-from-file)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_animation_new_from_resource
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_animation_new_from_resource"
               %pixbuf-animation-new-from-resource) (g:object pixbuf-animation)
  (resource :string)
  (err :pointer))

(defun pixbuf-animation-new-from-resource (resource)
 #+liber-documentation
 "@version{#2024-6-29}
  @argument[resource]{a string with the path of the resource file}
  @begin{return}
    The newly created @class{gdk-pixbuf:pixbuf-animation} object, or @code{nil}
    if any of several error conditions occurred: the file could not be opened,
    the image format is not supported, there was not enough memory to allocate
    the image buffer, the stream contained invalid data, or the operation was
    cancelled.
  @end{return}
  @begin{short}
    Creates a new pixbuf animation by loading an image from an resource.
  @end{short}
  The file format is detected automatically.
  @see-class{gdk-pixbuf:pixbuf-animation}"
  (glib:with-g-error (err)
    (%pixbuf-animation-new-from-resource resource err)))

(export 'pixbuf-animation-new-from-resource)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_animation_new_from_stream ()
;;;
;;; GdkPixbufAnimation *
;;; gdk_pixbuf_animation_new_from_stream (GInputStream *stream,
;;;                                       GCancellable *cancellable,
;;;                                       GError **error);
;;;
;;; Creates a new animation by loading it from an input stream.
;;;
;;; The file format is detected automatically. If NULL is returned, then error
;;; will be set. The cancellable can be used to abort the operation from another
;;; thread. If the operation was cancelled, the error G_IO_ERROR_CANCELLED will
;;; be returned. Other possible errors are in the GDK_PIXBUF_ERROR and
;;; G_IO_ERROR domains.
;;;
;;; The stream is not closed.
;;;
;;; stream :
;;;     a GInputStream to load the pixbuf from
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; error :
;;;     Return location for an error
;;;
;;; Returns :
;;;     A newly-created pixbuf, or NULL if any of several error conditions
;;;     occurred: the file could not be opened, the image format is not
;;;     supported, there was not enough memory to allocate the image buffer,
;;;     the stream contained invalid data, or the operation was cancelled.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_animation_new_from_stream_async ()
;;;
;;; void
;;; gdk_pixbuf_animation_new_from_stream_async (GInputStream *stream,
;;;                                             GCancellable *cancellable,
;;;                                             GAsyncReadyCallback callback,
;;;                                             gpointer user_data);
;;;
;;; Creates a new animation by asynchronously loading an image from an input
;;; stream.
;;;
;;; For more details see gdk_pixbuf_new_from_stream(), which is the synchronous
;;; version of this function.
;;;
;;; When the operation is finished, callback will be called in the main thread.
;;; You can then call gdk_pixbuf_animation_new_from_stream_finish() to get the
;;; result of the operation.
;;;
;;; stream :
;;;     a GInputStream from which to load the animation
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore. [allow-none]
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the the pixbuf is loaded
;;;
;;; user_data :
;;;     the data to pass to the callback function
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_animation_new_from_stream_finish ()
;;;
;;; GdkPixbufAnimation *
;;; gdk_pixbuf_animation_new_from_stream_finish (GAsyncResult *async_result,
;;;                                              GError **error);
;;;
;;; Finishes an asynchronous pixbuf animation creation operation started with
;;; gdk_pixbuf_animation_new_from_stream_async().
;;;
;;; async_result :
;;;     a GAsyncResult
;;;
;;; error :
;;;     a GError, or NULL
;;;
;;; Returns :
;;;     a GdkPixbufAnimation or NULL on error. Free the returned object with
;;;     g_object_unref().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_animation_ref ()
;;;
;;; GdkPixbufAnimation *
;;; gdk_pixbuf_animation_ref (GdkPixbufAnimation *animation)
;;;
;;; Warning
;;;
;;; gdk_pixbuf_animation_ref has been deprecated since version 2.0 and should
;;; not be used in newly written code. Use g_object_ref().
;;;
;;; Adds a reference to an animation.
;;;
;;; animation :
;;;     An animation.
;;;
;;; Returns :
;;;     The same as the animation argument.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_animation_unref ()
;;;
;;; void gdk_pixbuf_animation_unref (GdkPixbufAnimation *animation);
;;;
;;; Warning
;;;
;;; gdk_pixbuf_animation_unref has been deprecated since version 2.0 and should
;;; not be used in newly written code. Use g_object_unref().
;;;
;;; Removes a reference from an animation.
;;;
;;; animation :
;;;     An animation.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_animation_get_width
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_animation_get_width" pixbuf-animation-width) :int
 #+liber-documentation
 "@version{#2024-11-11}
  @argument[animation]{a @class{gdk-pixbuf:pixbuf-animation} object}
  @return{The integer with the width of the bounding box of the animation.}
  @begin{short}
    Queries the width of the bounding box of a pixbuf animation.
  @end{short}
  @see-class{gdk-pixbuf:animation}"
  (animation (g:object pixbuf-animation)))

(export 'pixbuf-animation-width)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_animation_get_height
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_animation_get_height" pixbuf-animation-height) :int
 #+liber-documentation
 "@version{#2024-11-11}
  @argument[animation]{a @class{gdk-pixbuf:pixbuf-animation} object}
  @return{The integer with the height of the bounding box of the animation.}
  @begin{short}
    Queries the width of the bounding box of a pixbuf animation.
  @end{short}
  @see-class{gdk-pixbuf:animation}"
  (animation (g:object pixbuf-animation)))

(export 'pixbuf-animation-height)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_animation_get_iter
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_animation_get_iter" pixbuf-animation-iter)
    (g:object pixbuf-animation-iter)
 #+liber-documentation
 "@version{#2024-11-11}
  @argument[animation]{a @class{gdk-pixbuf:pixbuf-animation} object}
  @argument[start]{an unsigned integer with the time the animations starts
    playing}
  @return{The @class{gdk-pixbuf:pixbuf-animation-iter} object with the iterator
    to move over the animation.}
  @begin{short}
    Get an iterator for displaying an animation.
  @end{short}
  The iterator provides the frames that should be displayed at a given time.

  The @arg{time} argument would normally come from the
  @code{g_get_current_time()} function, and marks the beginning of animation
  playback. After creating an iterator, you should immediately display the
  pixbuf returned by the @fun{gdk-pixbuf:pixbuf-animation-iter-pixbuf} function.
  Then, you should install a timeout, with the @fun{g:timeout-add} function, or
  by some other mechanism ensure that you will update the image after
  @fun{gdk-pixbuf:pixbuf-animation-iter-delay-time} milliseconds. Each time the
  image is updated, you should reinstall the timeout with the new, possibly
  changed delay time.

  As a shortcut, if the @arg{time} argument is @code{nil}, the result of the
  @code{g_get_current_time()} function will be used automatically.

  To update the image, that is possibly change the result of the
  @fun{gdk-pixbuf:pixbuf-animation-iter-pixbuf} function to a new frame of the
  animation, call the @fun{gdk-pixbuf:pixbuf-animation-iter-advance} function.

  If you are using the @class{gdk-pixbuf:pixbuf-loader} API, in addition to
  updating the image after the delay time, you should also update it whenever
  you receive the @code{\"area-updated\"} signal and the
  @fun{gdk-pixbuf:pixbuf-animation-iter-on-currently-loading-frame} function
  returns @em{true}. In this case, the frame currently being fed into the loader
  has received new data, so needs to be refreshed. The delay time for a frame
  may also be modified after an @code{\"area-updated\"} signal, for example, if
  the delay time for a frame is encoded in the data after the frame itself. So
  your timeout should be reinstalled after any @code{\"area-updated\"} signal.

  A delay time of -1 is possible, indicating 'infinite'.
  @see-class{gdk-pixbuf:pixbuf-animation}
  @see-class{gdk-pixbuf:pixbuf-animation-iter}"
  (animation (g:object pixbuf-animation))
  (time :uint64))

(export 'pixbuf-animation-iter)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_animation_is_static_image
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_animation_is_static_image"
               pixbuf-animation-is-static-image) :boolean
 #+liber-documentation
 "@version{#2024-11-11}
  @argument[animation]{a @class{gdk-pixbuf:pixbuf-animation} object}
  @return{@em{True} if the animation was really just an image.}
  @begin{short}
    If you load a file with the @fun{gdk-pixbuf:pixbuf-animation-new-from-file}
    function and it turns out to be a plain, unanimated image, then this
    function will return @em{true}.
  @end{short}
  Use the @fun{gdk-pixbuf:pixbuf-animation-static-image} function to retrieve
  the image.
  @see-class{gdk-pixbuf:pixbuf-animation}
  @see-function{gdk-pixbuf:pixbuf-animation-new-from-file}
  @see-function{gdk-pixbuf:pixbuf-animation-static-image}"
  (animation (g:object pixbuf-animation)))

(export 'pixbuf-animation-is-static-image)

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_animation_get_static_image
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_pixbuf_animation_get_static_image"
               pixbuf-animation-static-image) (g:object pixbuf)
 #+liber-documentation
 "@version{#2024-6-29}
  @argument[animation]{a @class{gdk-pixbuf:pixbuf-animation} object}
  @return{Unanimated @class{gdk-pixbuf:pixbuf} image representing the
    animation.}
  @begin{short}
    If an animation is really just a plain image, has only one frame, this
    function returns that image.
  @end{short}
  If the animation is an animation, this function returns a reasonable thing to
  display as a static unanimated image, which might be the first frame, or
  something more sophisticated. If an animation has not loaded any frames yet,
  this function will return @code{nil}.
  @see-class{gdk-pixbuf:pixbuf}
  @see-class{gdk-pixbuf:pixbuf-animation}"
  (animation (g:object pixbuf-animation)))

(export 'pixbuf-animation-static-image)

;;; ----------------------------------------------------------------------------
;;; GdkPixbufSimpleAnim
;;;
;;; typedef struct _GdkPixbufSimpleAnim GdkPixbufSimpleAnim;
;;;
;;; An opaque struct representing a simple animation.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_simple_anim_new ()
;;;
;;; GdkPixbufSimpleAnim *
;;; gdk_pixbuf_simple_anim_new (gint width, gint height, gfloat rate);
;;;
;;; Creates a new, empty animation.
;;;
;;; width :
;;;     the width of the animation
;;;
;;; height :
;;;     the height of the animation
;;;
;;; rate :
;;;     the speed of the animation, in frames per second
;;;
;;; Returns :
;;;     a newly allocated GdkPixbufSimpleAnim
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_simple_anim_add_frame ()
;;;
;;; void
;;; gdk_pixbuf_simple_anim_add_frame (GdkPixbufSimpleAnim *animation,
;;;                                   GdkPixbuf *pixbuf);
;;;
;;; Adds a new frame to animation. The pixbuf must have the dimensions specified
;;; when the animation was constructed.
;;;
;;; animation :
;;;     a GdkPixbufSimpleAnim
;;;
;;; pixbuf :
;;;     the pixbuf to add
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_simple_anim_set_loop ()
;;;
;;; void
;;; gdk_pixbuf_simple_anim_set_loop (GdkPixbufSimpleAnim *animation,
;;;                                  gboolean loop);
;;;
;;; Sets whether animation should loop indefinitely when it reaches the end.
;;;
;;; animation :
;;;     a GdkPixbufSimpleAnim
;;;
;;; loop :
;;;     whether to loop the animation
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_pixbuf_simple_anim_get_loop ()
;;;
;;; gboolean gdk_pixbuf_simple_anim_get_loop (GdkPixbufSimpleAnim *animation);
;;;
;;; Gets whether animation should loop indefinitely when it reaches the end.
;;;
;;; animation :
;;;     a GdkPixbufSimpleAnim
;;;
;;; Returns :
;;;     TRUE if the animation loops forever, FALSE otherwise
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk-pixbuf.animation.lisp ----------------------------------
