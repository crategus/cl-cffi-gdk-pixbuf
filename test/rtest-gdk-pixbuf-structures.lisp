(in-package :gdk-pixbuf-test)

(def-suite gdk-pixbuf-structures :in gdk-pixbuf-suite)
(in-suite gdk-pixbuf-structures)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkColorspace

(test colorspace
  ;; Check the type
  (is (g:type-is-enum "GdkColorspace"))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkColorspace")
          (g:gtype (cffi:foreign-funcall "gdk_colorspace_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gdk-pixbuf:colorspace
          (glib:symbol-for-gtype "GdkColorspace")))
  ;; Check the names
  (is (equal '("GDK_COLORSPACE_RGB")
             (mapcar #'gobject:enum-item-name
                     (gobject:get-enum-items "GdkColorspace"))))
  ;; Check the values
  (is (equal '(0)
             (mapcar #'gobject:enum-item-value
                     (gobject:get-enum-items "GdkColorspace"))))
  ;; Check the nick names
  (is (equal '("rgb")
             (mapcar #'gobject:enum-item-nick
                     (gobject:get-enum-items "GdkColorspace"))))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GdkColorspace"
                                     GDK-COLORSPACE
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gdk_colorspace_get_type")
                                     (:RGB 0))
             (gobject:get-g-type-definition "GdkColorspace"))))

;;;     GdkPixbufAlphaMode

;;;     GdkPixbuf

(test pixbuf-class
  ;; Type check
  (is (g:type-is-object "GdkPixbuf"))
  ;; Check the registered name
  (is (eq 'gdk-pixbuf:pixbuf
          (glib:symbol-for-gtype "GdkPixbuf")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkPixbuf")
          (g:gtype (cffi:foreign-funcall "gdk_pixbuf_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkPixbuf")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g:type-name (g:type-children "GdkPixbuf"))))
  ;; Check the interfaces
  (is (equal '("GIcon" "GLoadableIcon")
             (mapcar #'g:type-name (g:type-interfaces "GdkPixbuf"))))
  ;; Check the class properties
  (is (equal '("bits-per-sample" "colorspace" "has-alpha" "height" "n-channels"
               "pixel-bytes" "pixels" "rowstride" "width")
             (list-properties "GdkPixbuf")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GdkPixbuf" GDK-PIXBUF
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                        ("GIcon" "GLoadableIcon") :TYPE-INITIALIZER
                        "gdk_pixbuf_get_type")
                       ((BITS-PER-SAMPLE GDK-PIXBUF-BITS-PER-SAMPLE
                         "bits-per-sample" "gint" T NIL)
                        (COLORSPACE GDK-PIXBUF-COLORSPACE "colorspace"
                         "GdkColorspace" T NIL)
                        (HAS-ALPHA GDK-PIXBUF-HAS-ALPHA "has-alpha" "gboolean"
                         T NIL)
                        (HEIGHT GDK-PIXBUF-HEIGHT "height" "gint" T NIL)
                        (N-CHANNELS GDK-PIXBUF-N-CHANNELS "n-channels" "gint" T
                         NIL)
                        (PIXEL-BYTES GDK-PIXBUF-PIXEL-BYTES "pixel-bytes"
                         "GBytes" T NIL)
                        (PIXELS GDK-PIXBUF-PIXELS "pixels" "gpointer" T NIL)
                        (ROWSTRIDE GDK-PIXBUF-ROWSTRIDE "rowstride" "gint" T
                         NIL)
                        (WIDTH GDK-PIXBUF-WIDTH "width" "gint" T NIL)))
             (gobject:get-g-type-definition "GdkPixbuf"))))

;;; --- Properties -------------------------------------------------------------

(test pixbuf-properties
  (let ((pixbuf (gdk-pixbuf:pixbuf-new-from-file
                    (sys-path "resource/ducky.png"))))
    (is (= 8 (gdk-pixbuf:pixbuf-bits-per-sample pixbuf)))
    (is (eq :rgb (gdk-pixbuf:pixbuf-colorspace pixbuf)))
    (is-true (gdk-pixbuf:pixbuf-has-alpha pixbuf))
    (is (= 537 (gdk-pixbuf:pixbuf-height pixbuf)))
    (is (= 4 (gdk-pixbuf:pixbuf-n-channels pixbuf)))
    (is (typep (gdk-pixbuf:pixbuf-pixel-bytes pixbuf) 'g:bytes))
    (is (cffi:pointerp (gdk-pixbuf:pixbuf-pixels pixbuf)))
    (is (eq 1956 (gdk-pixbuf:pixbuf-rowstride pixbuf)))
    (is (= 489 (gdk-pixbuf:pixbuf-width pixbuf)))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_pixbuf_get_pixels_with_length
;;;     gdk_pixbuf_get_byte_length
;;;     gdk_pixbuf_get_option
;;;     gdk_pixbuf_set_option
;;;     gdk_pixbuf_remove_option
;;;     gdk_pixbuf_get_options
;;;     gdk_pixbuf_copy_options
;;;     gdk_pixbuf_read_pixels

;;; --- 2023-5-29 --------------------------------------------------------------
