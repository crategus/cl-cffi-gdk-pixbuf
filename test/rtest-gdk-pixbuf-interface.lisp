(in-package :gdk-pixbuf-test)

(def-suite gdk-pixbuf-interface :in gdk-pixbuf-test)
(in-suite gdk-pixbuf-interface)

(defparameter gdk-pixbuf-interface
              '(gdk-pixbuf:pixbuf-formats
                gdk-pixbuf:pixbuf-format-name
                gdk-pixbuf:pixbuf-format-description
                gdk-pixbuf:pixbuf-format-mime-types
                gdk-pixbuf:pixbuf-format-extensions
                gdk-pixbuf:pixbuf-format-is-save-option-supported
                gdk-pixbuf:pixbuf-format-is-writable
                gdk-pixbuf:pixbuf-format-is-scalable
                gdk-pixbuf:pixbuf-format-is-disabled
                gdk-pixbuf:pixbuf-format-set-disabled
                gdk-pixbuf:pixbuf-format-license))

(export 'gdk-pixbuf-interface)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkPixbufFormatFlags
;;;     GdkPixbufModulePattern
;;;     GdkPixbufModule
;;;     GdkPixbufAnimationClass
;;;     GdkPixbufAnimationIterClass
;;;     GdkPixbufFormat

;;; --- Functions --------------------------------------------------------------

;;;     gdk_pixbuf_get_formats

(test gdk-pixbuf-formats
  (is (every #'cffi:pointerp (gdk-pixbuf:pixbuf-formats)))
  #+crategus
  (is (equal '("ani" "avif" "bmp" "gif" "heif/avif" "icns" "ico" "jpeg" "png"
               "pnm" "qtif" "svg" "tga" "tiff" "webp" "wmf" "xbm" "xpm")
             (sort (mapcar #'gdk-pixbuf:pixbuf-format-name
                           (gdk-pixbuf:pixbuf-formats))
                   #'string<)))
  #+windows
  (is (equal '("ani" "bmp" "emf" "gif" "icns" "ico" "jpeg" "jxl" "png" "pnm"
               "qtif" "svg" "tga" "tiff" "wmf" "xbm" "xpm")
             (sort (mapcar #'gdk-pixbuf:pixbuf-format-name
                           (gdk-pixbuf:pixbuf-formats))
                   #'string<))))

;;;     gdk_pixbuf_format_copy
;;;     gdk_pixbuf_format_free

;;;     gdk_pixbuf_format_get_name
;;;     gdk_pixbuf_format_get_description
;;;     gdk_pixbuf_format_get_mime_types
;;;     gdk_pixbuf_format_get_extensions
;;;     gdk_pixbuf_format_is_save_option_supported
;;;     gdk_pixbuf_format_is_writable
;;;     gdk_pixbuf_format_is_scalable
;;;     gdk_pixbuf_format_is_disabled
;;;     gdk_pixbuf_format_set_disabled
;;;     gdk_pixbuf_format_get_license

(test gdk-pixbuf-format-infos.1
  (let* ((path (glib-sys:sys-path "test/resource/ducky.png"))
         (format (gdk-pixbuf:pixbuf-file-info path)))
    (is (string= "png" (gdk-pixbuf:pixbuf-format-name format)))
    (is (string= "PNG" (gdk-pixbuf:pixbuf-format-description format)))
    (is (equal '("image/png") (gdk-pixbuf:pixbuf-format-mime-types format)))
    (is (equal '("png") (gdk-pixbuf:pixbuf-format-extensions format)))
    (is-true (gdk-pixbuf:pixbuf-format-is-save-option-supported format
                                                                "compression"))
    (is-true (gdk-pixbuf:pixbuf-format-is-writable format))
    (is-false (gdk-pixbuf:pixbuf-format-is-scalable format))
    (is-false (gdk-pixbuf:pixbuf-format-is-disabled format))
    (is (string= "LGPL" (gdk-pixbuf:pixbuf-format-license format)))))

(test gdk-pixbuf-format-infos.2
  (let* ((path (glib-sys:sys-path "test/resource/floppybuddy.gif"))
         (format (gdk-pixbuf:pixbuf-file-info path)))
    (is (string= "gif" (gdk-pixbuf:pixbuf-format-name format)))
    (is (string= "GIF" (gdk-pixbuf:pixbuf-format-description format)))
    (is (equal '("image/gif") (gdk-pixbuf:pixbuf-format-mime-types format)))
    (is (equal '("gif") (gdk-pixbuf:pixbuf-format-extensions format)))
    (is-false (gdk-pixbuf:pixbuf-format-is-save-option-supported format
                                                                 "compression"))
    (is-false (gdk-pixbuf:pixbuf-format-is-writable format))
    (is-false (gdk-pixbuf:pixbuf-format-is-scalable format))
    (is-false (gdk-pixbuf:pixbuf-format-is-disabled format))
    (is (string= "LGPL" (gdk-pixbuf:pixbuf-format-license format)))))

;;;     GdkPixbufModuleFillVtableFunc
;;;     GdkPixbufModuleFillInfoFunc
;;;     GdkPixbufModuleSizeFunc
;;;     GdkPixbufModulePreparedFunc
;;;     GdkPixbufModuleUpdatedFunc

;;; 2024-6-16
