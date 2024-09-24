(in-package :gdk-pixbuf-test)

(def-suite gdk-pixbuf-structures :in gdk-pixbuf-test)
(in-suite gdk-pixbuf-structures)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkColorspace

(test gdk-pixbuf-colorspace
  ;; Check type
  (is (g:type-is-enum "GdkColorspace"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkColorspace")
          (g:gtype (cffi:foreign-funcall "gdk_colorspace_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk-pixbuf:colorspace
          (glib:symbol-for-gtype "GdkColorspace")))
  ;; Check names
  (is (equal '("GDK_COLORSPACE_RGB")
             (glib-test:list-enum-item-names "GdkColorspace")))
  ;; Check values
  (is (equal '(0)
             (glib-test:list-enum-item-values "GdkColorspace")))
  ;; Check nick names
  (is (equal '("rgb")
             (glib-test:list-enum-item-nicks "GdkColorspace")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GdkColorspace" GDK-PIXBUF:COLORSPACE
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "gdk_colorspace_get_type")
                                    (:RGB 0))
             (gobject:get-gtype-definition "GdkColorspace"))))

;;;     GdkPixbuf

(test gdk-pixbuf-class
  ;; Check type
  (is (g:type-is-object "GdkPixbuf"))
  ;; Check registered name
  (is (eq 'gdk-pixbuf:pixbuf
          (glib:symbol-for-gtype "GdkPixbuf")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkPixbuf")
          (g:gtype (cffi:foreign-funcall "gdk_pixbuf_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkPixbuf")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GdkPixbuf")))
  ;; Check interfaces
  (is (equal '("GIcon" "GLoadableIcon")
             (glib-test:list-interfaces "GdkPixbuf")))
  ;; Check properties
  (is (equal '("bits-per-sample" "colorspace" "has-alpha" "height" "n-channels"
               "pixel-bytes" "pixels" "rowstride" "width")
             (glib-test:list-properties "GdkPixbuf")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GdkPixbuf")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkPixbuf" GDK-PIXBUF:PIXBUF
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES ("GIcon" "GLoadableIcon")
                        :TYPE-INITIALIZER "gdk_pixbuf_get_type")
                       ((BITS-PER-SAMPLE PIXBUF-BITS-PER-SAMPLE
                         "bits-per-sample" "gint" T NIL)
                        (COLORSPACE PIXBUF-COLORSPACE
                         "colorspace" "GdkColorspace" T NIL)
                        (HAS-ALPHA PIXBUF-HAS-ALPHA
                         "has-alpha" "gboolean" T NIL)
                        (HEIGHT PIXBUF-HEIGHT "height" "gint" T NIL)
                        (N-CHANNELS PIXBUF-N-CHANNELS "n-channels" "gint" T NIL)
                        (PIXEL-BYTES PIXBUF-PIXEL-BYTES
                         "pixel-bytes" "GBytes" T NIL)
                        (PIXELS PIXBUF-PIXELS "pixels" "gpointer" T NIL)
                        (ROWSTRIDE PIXBUF-ROWSTRIDE "rowstride" "gint" T NIL)
                        (WIDTH PIXBUF-WIDTH "width" "gint" T NIL)))
             (gobject:get-gtype-definition "GdkPixbuf"))))

;;; --- Properties -------------------------------------------------------------

(test gdk-pixbuf-properties
  (let* ((path (glib-sys:sys-path "test/resource/ducky.png"))
         (pixbuf (gdk-pixbuf:pixbuf-new-from-file path)))
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

;;; 2024-9-18
