;;; ----------------------------------------------------------------------------
;;; gdk-pixbuf.package.lisp
;;;
;;; The documentation in this file is taken from the GDK-PixBuf Reference Manual
;;; Version 2.42 and modified to document the Lisp binding to the GDK-PixBuf
;;; library, see <http://www.gtk.org>. The API documentation of the Lisp binding
;;; is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
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

(defpackage :gdk-pixbuf
  (:use :common-lisp)
  (:import-from :cffi)
  (:import-from #:glib)
  (:import-from #:gobject))

;;; ----------------------------------------------------------------------------

#+sbcl
(when (and (find-package "SB-INT")
           (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-INT")))
  (funcall (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-INT"))
           :traps nil))

;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (find-package :gdk-pixbuf) t)
 "GDK-Pixbuf is a library for image loading and manipulation. The GDK-Pixbuf
  documentation contains both the programmer's guide and the API reference.
  This is the API documentation of a Lisp binding to the GDK-Pixbuf library.
  @begin[Library version numbers]{section}
    These macros and variables let you check the version of GDK-Pixbuf you are
    linking against.
    @about-symbol{+version+}
    @about-symbol{+major-version+}
    @about-symbol{+minor-version+}
    @about-symbol{+micro-version+}
  @end{section}
  @begin[The GdkPixbuf object]{section}
    @about-symbol{colorspace}
    @about-symbol{pixbuf-alpha-mode}
    @about-class{pixbuf}
    @about-generic{pixbuf-bits-per-sample}
    @about-generic{pixbuf-colorspace}
    @about-generic{pixbuf-has-alpha}
    @about-generic{pixbuf-height}
    @about-generic{pixbuf-n-channels}
    @about-generic{pixbuf-pixel-bytes}
    @about-generic{pixbuf-pixels}
    @about-generic{pixbuf-rowstride}
    @about-generic{pixbuf-width}
    @about-function{pixbuf-pixels-with-length}
    @about-function{pixbuf-byte-length}
    @about-function{pixbuf-option}
    @about-function{pixbuf-remove-option}
    @about-function{pixbuf-options}
    @about-function{pixbuf-copy-options}
    @about-function{pixbuf-read-pixels}
  @end{section}
  @begin[File Loading]{section}
    @begin[Introduction to fileloading]{subsection}
      The GDK-Pixbuf library provides a simple mechanism for loading an image
      from a file in synchronous fashion. This means that the library takes
      control of the application while the file is being loaded. From the user's
      point of view, the application will block until the image is done loading.

      This interface can be used by applications in which blocking is acceptable
      while an image is being loaded. It can also be used to load small images
      in general. Applications that need progressive loading can use the
      @class{gdk-pixbuf:pixbuf-loader} API functionality instead.
    @end{subsection}
    @begin[Functions for file loading]{subsection}
      @about-function{pixbuf-new-from-file}
      @about-function{pixbuf-new-from-file-at-size}
      @about-function{pixbuf-new-from-file-at-scale}
      @about-function{pixbuf-new-from-resource}
      @about-function{pixbuf-new-from-resource-at-scale}
      @about-function{pixbuf-file-info}
      @about-function{pixbuf-file-info-async}
      @about-function{pixbuf-file-info-finish}
      @about-function{pixbuf-new-from-stream}
      @about-function{pixbuf-new-from-stream-async}
      @about-function{pixbuf-new-from-stream-finish}
      @about-function{pixbuf-new-from-stream-at-scale}
      @about-function{pixbuf-new-from-stream-at-scale-async}
    @end{subsection}
  @end{section}
  @begin[File Saving]{section}
    @begin[Introduction to file saving]{subsection}
      These functions allow to save a @class{gdk-pixbuf:pixbuf} object in a
      number of file formats. The formatted data can be written to a file or to
      a memory buffer. The @class{gdk-pixbuf:pixbuf} library can also call a
      user-defined callback on the data, which allows to write the image
      to a socket or store it in a database.
    @end{subsection}
    @begin[Functions for file saving]{subsection}
      @about-function{pixbuf-save}
      @about-function{pixbuf-save-to-callback}
      @about-function{pixbuf-save-to-callbackv}
      @about-function{pixbuf-save-to-buffer}
      @about-function{pixbuf-save-to-bufferv}
      @about-function{pixbuf-save-to-stream}
      @about-function{pixbuf-save-to-streamv}
      @about-function{pixbuf-save-to-stream-async}
      @about-function{pixbuf-save-to-streamv-async}
      @about-function{pixbuf-save-to-stream-finish}
    @end{subsection}
  @end{section}
  @begin[Image Data in Memory]{section}
    @begin[Introduction to image data in memory]{subsection}
      The most basic way to create a pixbuf is to wrap an existing pixel buffer
      with a @class{gdk-pixbuf:pixbuf} object. You can use the
      @fun{gdk-pixbuf:pixbuf-new-from-data} function to do this. You need to
      specify the destroy notification function that will be called when the
      data buffer needs to be freed. This will happen when a
      @class{gdk-pixbuf:pixbuf} object is finalized by the reference counting
      functions If you have a chunk of static data compiled into your
      application, you can pass in @code{nil} as the destroy notification
      function so that the data will not be freed.

      The @fun{gdk-pixbuf:pixbuf-new} function can be used as a convenience to
      create a pixbuf with an empty buffer. This is equivalent to allocating a
      data buffer using @code{malloc()} and then wrapping it with the
      @fun{gdk-pixbuf:pixbuf-new-from-data} function. The
      @fun{gdk-pixbuf:pixbuf-new} function will compute an optimal rowstride so
      that rendering can be performed with an efficient algorithm.

      You can also copy an existing pixbuf with the @fun{gdk-pixbuf:pixbuf-copy}
      function. The copy function will actually duplicate the pixel data in
      memory and create a new @class{gdk-pixbuf:pixbuf} instance for it.
      @end{subsection}
    @begin[Functions for image data in memory]{subsection}
      @about-function{pixbuf-new}
      @about-function{pixbuf-new-from-bytes}
      @about-function{pixbuf-from-data}
      @about-function{pixbuf-new-from-xpm-data}
      @about-function{pixbuf-new-from-inline}
      @about-function{pixbuf-new-subpixbuf}
      @about-function{pixbuf-copy}
    @end{subsection}
  @end{section}
  @begin[Scaling]{section}
    @begin[Introduction to scaling]{subsection}
      The @class{gdk-pixbuf:pixbuf} library contains functions to scale pixbufs,
      to scale pixbufs and composite against an existing image, and to scale
      pixbufs and composite against a solid color or checkerboard. Compositing
      a checkerboard is a common way to show an image with an alpha channel in
      image-viewing and editing software.

      Since the full-featured @fun{gdk-pixbuf:pixbuf-scale},
      @fun{gdk-pixbuf:pixbuf-composite}, and
      @fun{gdk-pixbuf:pixbuf-composite-color} functions are rather complex to
      use and have many arguments, two simple convenience functions are
      provided, the @fun{gdk-pixbuf:pixbuf-scale-simple} and
      @fun{gdk-pixbuf:pixbuf-composite-color-simple} functions which create a
      new pixbuf of a given size, scale an original image to fit, and then
      return the new pixbuf.

      If the destination pixbuf was created from a readonly source, these
      operations will force a copy into a mutable buffer.

      Scaling and compositing functions take advantage of MMX hardware
      acceleration on systems where MMX is supported. If the
      @class{gdk-pixbuf:pixbuf} library is built with the Sun mediaLib library,
      these functions are instead accelerated using mediaLib, which provides
      hardware acceleration on Intel, AMD, and Sparc chipsets. If desired,
      mediaLib support can be turned off by setting the
      @code{GDK_DISABLE_MEDIALIB} environment variable.

      The following example demonstrates handling an expose event by rendering
      the appropriate area of a source image, which is scaled to fit the widget,
      onto the window of the widget. The source image is rendered against a
      checkerboard, which provides a visual representation of the alpha channel
      if the image has one. If the image does not have an alpha channel, calling
      the @fun{gdk-pixbuf:pixbuf-composite-color} function has exactly the same
      effect as calling the @fun{gdk-pixbuf:pixbuf-scale} function.

      @b{Example:} Handling an expose event.
      @begin{pre}
gboolean
expose_cb (GtkWidget *widget, GdkEventExpose *event, gpointer data)
{
  GdkPixbuf *dest;

  dest = gdk_pixbuf_new (GDK_COLORSPACE_RGB, FALSE, 8,
                         event->area.width, event->area.height);

  gdk_pixbuf_composite_color (pixbuf, dest,
           0, 0,
           event->area.width, event->area.height,
           -event->area.x, -event->area.y,
           (double) widget->allocation.width / gdk_pixbuf_get_width (pixbuf),
           (double) widget->allocation.height / gdk_pixbuf_get_height (pixbuf),
           GDK_INTERP_BILINEAR, 255,
           event->area.x, event->area.y, 16, 0xaaaaaa, 0x555555);

  gdk_draw_pixbuf (widget->window, widget->style->fg_gc[GTK_STATE_NORMAL],
                   dest, 0, 0,
                   event->area.x, event->area.y,
                   event->area.width, event->area.height,
                   GDK_RGB_DITHER_NORMAL, event->area.x, event->area.y);

  gdk_pixbuf_unref (dest);

  return TRUE;
@}
      @end{pre}
    @end{subsection}
    @begin[Functions for scaling]{subsection}
      @about-symbol{pixbuf-interp-type}
      @about-symbol{pixbuf-rotation}
      @about-function{pixbuf-scale-simple}
      @about-function{pixbuf-scale}
      @about-function{pixbuf-composite-color-simple}
      @about-function{pixbuf-composite}
      @about-function{pixbuf-composite-color}
      @about-function{pixbuf-rotate-simple}
      @about-function{pixbuf-flip}
    @end{subsection}
  @end{section}
  @begin[Utilities]{section}
    @begin[Introduction to utilities]{subsection}
      Utility and miscellaneous convenience functions. These functions provide
      miscellaneous utilities for manipulating pixbufs. The pixel data in
      pixbufs may of course be manipulated directly by applications, but
      several common operations can be performed by these functions instead.
    @end{subsection}
    @begin[Functions for utilities]{subsection}
      @about-function{pixbuf-add-alpha}
      @about-function{pixbuf-copy-area}
      @about-function{pixbuf-saturate-and-pixelate}
      @about-function{pixbuf-apply-embedded-orientation}
      @about-function{pixbuf-fill}
    @end{subsection}
  @end{section}
  @begin[Animations]{section}
    @begin[GdkPixbufAnimationIter]{subsection}
      @about-class{pixbuf-animation-iter}
      @about-function{pixbuf-animation-iter-pixbuf}
      @about-function{pixbuf-animation-iter-delay-time}
      @about-function{pixbuf-animation-iter-advance}
      @about-function{pixbuf-animation-iter-on-currently-loading-frame}
    @end{subsection}
    @begin[GdkPixbufAnimation]{subsection}
      @about-class{pixbuf-animation}
      @about-function{pixbuf-animation-new-from-file}
      @about-function{pixbuf-animation-new-from-resource}
      @about-function{pixbuf-animation-width}
      @about-function{pixbuf-animation-height}
      @about-function{pixbuf-animation-iter}
      @about-function{pixbuf-animation-is-static-image}
      @about-function{pixbuf-animation-static-image}
    @end{subsection}
    @begin[GdkPixbufSimpleAnim]{subsection}
      @about-class{pixbuf-simple-anim}
      @about-generic{pixbuf-simple-anim-loop}
      @about-function{pixbuf-simple-anim-new}
      @about-function{pixbuf-simple-anim-add-frame}
    @end{subsection}
  @end{section}
  @begin[GdkPixbufLoader]{section}
    @about-class{pixbuf-loader}
    @about-function{pixbuf-loader-new}
    @about-function{pixbuf-loader-new-with-type}
    @about-function{pixbuf-loader-new-with-mime-type}
    @about-function{pixbuf-loader-get-format}
    @about-function{pixbuf-loader-write}
    @about-function{pixbuf-loader-write-bytes}
    @about-function{pixbuf-loader-set-size}
    @about-function{pixbuf-loader-pixbuf}
    @about-function{pixbuf-loader-animation}
    @about-function{pixbuf-loader-close}
  @end{section}
  @begin[Module Interface]{section}
    @about-symbol{pixbuf-format-flags}
    @about-type{pixbuf-module-pattern}
    @about-symbol{pixbuf-module}
    @about-symbol{pixbuf-animation-class}
    @about-symbol{pixbuf-animation-iter-class}
    @about-symbol{pixbuf-format}
    @about-function{pixbuf-formats}
    @about-function{pixbuf-format-copy}
    @about-function{pixbuf-format-free}
    @about-function{pixbuf-format-name}
    @about-function{pixbuf-format-description}
    @about-function{pixbuf-format-mime-types}
    @about-function{pixbuf-format-extensions}
    @about-function{pixbuf-format-is-save-option-supported}
    @about-function{pixbuf-format-is-writable}
    @about-function{pixbuf-format-is-scalable}
    @about-function{pixbuf-format-is-disabled}
    @about-function{pixbuf-format-set-disabled}
    @about-function{pixbuf-format-license}
  @end{section}")

;;; --- End of file gdk-pixbuf.package.lisp ------------------------------------
