(in-package :gdk-pixbuf-test)

(def-suite gdk-pixbuf-load :in gdk-pixbuf-suite)
(in-suite gdk-pixbuf-load)

;;; --- Functions --------------------------------------------------------------

;;;     gdk_pixbuf_new_from_file

(test gdk-pixbuf-new-from-file
  (is-false (gdk-pixbuf:pixbuf-new-from-file "unknown"))
  (is (typep (gdk-pixbuf:pixbuf-new-from-file (sys-path "resource/ducky.png"))
      'gdk-pixbuf:pixbuf)))

;;;     gdk_pixbuf_new_from_file_at_size

(test gdk-pixbuf-new-from-file-at-size
  (is-false (gdk-pixbuf:pixbuf-new-from-file-at-size "unkonwn" 128 128))
  (is (typep (gdk-pixbuf:pixbuf-new-from-file-at-size
                 (sys-path "resource/ducky.png") 128 128)
             'gdk-pixbuf:pixbuf)))

;;;     gdk_pixbuf_new_from_file_at_scale

(test gdk-pixbuf-new-from-file-at-scale
  (is-false (gdk-pixbuf:pixbuf-new-from-file-at-scale "unkonwn" 128 128 nil))
  (is (typep (gdk-pixbuf:pixbuf-new-from-file-at-scale
                 (sys-path"resource/ducky.png") 128 128 nil)
             'gdk-pixbuf:pixbuf))
  (is (typep (gdk-pixbuf:pixbuf-new-from-file-at-scale
                 (sys-path "resource/ducky.png") 128 128 t)
             'gdk-pixbuf:pixbuf)))

;;;     gdk_pixbuf_get_file_info

(test gdk-pixbuf-file-info.1
  (multiple-value-bind (format width height)
      (gdk-pixbuf:pixbuf-file-info (sys-path "resource/floppybuddy.gif"))
    (is (= 80 width))
    (is (= 70 height))
    (is (string= "gif" (gdk-pixbuf:pixbuf-format-name format)))
    (is (string= "GIF" (gdk-pixbuf:pixbuf-format-description format)))
    (is (equal '("image/gif") (gdk-pixbuf:pixbuf-format-mime-types format)))
    (is (equal '("gif") (gdk-pixbuf:pixbuf-format-extensions format)))))

(test gdk-pixbuf-file-info.2
  (multiple-value-bind (format width height)
      (gdk-pixbuf:pixbuf-file-info (sys-path "resource/ducky.png"))
    (is (= 489 width))
    (is (= 537 height))
    (is (string= "png" (gdk-pixbuf:pixbuf-format-name format)))
    (is (string= "PNG" (gdk-pixbuf:pixbuf-format-description format)))
    (is (equal '("image/png") (gdk-pixbuf:pixbuf-format-mime-types format)))
    (is (equal '("png") (gdk-pixbuf:pixbuf-format-extensions format)))))

;;;     gdk_pixbuf_get_file_info_async
;;;     gdk_pixbuf_get_file_info_finish

;;;     gdk_pixbuf_new_from_resource

(test gdk-pixbuf-new-from-resource
  (let ((path (sys-path "resource/rtest-resource.gresource")))
    (with-g-resources (resource path)
    (is-false (gdk-pixbuf:pixbuf-new-from-resource "unknown"))
    (is (typep (gdk-pixbuf:pixbuf-new-from-resource "/com/crategus/test/ducky.png")
               'gdk-pixbuf:pixbuf))
    (is (typep (gdk-pixbuf:pixbuf-new-from-resource "/com/crategus/test/floppybuddy.gif")
               'gdk-pixbuf:pixbuf)))))

;;;     gdk_pixbuf_new_from_resource_at_scale

(test gdk-pixbuf-new-from-resource-at-scale
  (let ((path (sys-path "resource/rtest-resource.gresource")))
    (with-g-resources (resource path)
      (is-false (gdk-pixbuf:pixbuf-new-from-resource-at-scale "unknown" 128 128 t))
      (is (typep (gdk-pixbuf:pixbuf-new-from-resource-at-scale
                      "/com/crategus/test/ducky.png"
                      128
                      128
                      t)
                 'gdk-pixbuf:pixbuf)))))

;;;     gdk_pixbuf_new_from_stream
;;;     gdk_pixbuf_new_from_stream_async
;;;     gdk_pixbuf_new_from_stream_finish
;;;     gdk_pixbuf_new_from_stream_at_scale
;;;     gdk_pixbuf_new_from_stream_at_scale_async

;;; --- 2023-5-5 ---------------------------------------------------------------
