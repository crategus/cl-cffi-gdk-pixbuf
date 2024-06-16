(in-package :gdk-pixbuf-test)

(def-suite gdk-pixbuf-load :in gdk-pixbuf-test)
(in-suite gdk-pixbuf-load)

(defparameter gdk-pixbuf-load
              '(gdk-pixbuf:pixbuf-new-from-file
                gdk-pixbuf:pixbuf-new-from-file-at-size
                gdk-pixbuf:pixbuf-new-from-file-at-scale
                gdk-pixbuf:pixbuf-new-from-resource
                gdk-pixbuf:pixbuf-new-from-resource-at-scale
                gdk-pixbuf:pixbuf-file-info
                gdk-pixbuf:pixbuf-file-info-async
                gdk-pixbuf:pixbuf-file-info-finish))

(export 'gdk-pixbuf-load)

;;; --- Functions --------------------------------------------------------------

;;;     gdk_pixbuf_new_from_file

(test gdk-pixbuf-new-from-file
  (let ((path (glib-sys:sys-path "test/resource/ducky.png")))
    (is-false (gdk-pixbuf:pixbuf-new-from-file "unknown"))
    (is (typep (gdk-pixbuf:pixbuf-new-from-file path) 'gdk-pixbuf:pixbuf))))

;;;     gdk_pixbuf_new_from_file_at_size

(test gdk-pixbuf-new-from-file-at-size
  (let ((path (glib-sys:sys-path "test/resource/ducky.png")))
    (is-false (gdk-pixbuf:pixbuf-new-from-file-at-size "unkonwn" 128 128))
    (is (typep (gdk-pixbuf:pixbuf-new-from-file-at-size path 128 128)
               'gdk-pixbuf:pixbuf))))

;;;     gdk_pixbuf_new_from_file_at_scale

(test gdk-pixbuf-new-from-file-at-scale
  (let ((path (glib-sys:sys-path "test/resource/ducky.png")))
    (is-false (gdk-pixbuf:pixbuf-new-from-file-at-scale "unkonwn" 128 128 nil))
    (is (typep (gdk-pixbuf:pixbuf-new-from-file-at-scale
                   (glib-sys:sys-path "test/resource/ducky.png") 128 128 nil)
               'gdk-pixbuf:pixbuf))
    (is (typep (gdk-pixbuf:pixbuf-new-from-file-at-scale path 128 128 t)
               'gdk-pixbuf:pixbuf))))

;;;     gdk_pixbuf_new_from_resource

(test gdk-pixbuf-new-from-resource
  (let ((path (glib-sys:sys-path "test/rtest-gdk-pixbuf.gresource"))
        (ducky "/com/crategus/gdk-pixbuf/ducky.png")
        (floppy "/com/crategus/gdk-pixbuf/floppybuddy.gif"))
    (gio:with-resource (resource path)
      (is-false (gdk-pixbuf:pixbuf-new-from-resource "unknown"))
      (is (typep (gdk-pixbuf:pixbuf-new-from-resource ducky) 'gdk-pixbuf:pixbuf))
      (is (typep (gdk-pixbuf:pixbuf-new-from-resource floppy) 'gdk-pixbuf:pixbuf)))))

;;;     gdk_pixbuf_new_from_resource_at_scale

(test gdk-pixbuf-new-from-resource-at-scale
  (let ((path (glib-sys:sys-path "test/rtest-gdk-pixbuf.gresource")))
    (gio:with-resource (resource path)
      (is-false (gdk-pixbuf:pixbuf-new-from-resource-at-scale "unknown" 128 128 t))
      (is (typep (gdk-pixbuf:pixbuf-new-from-resource-at-scale
                      "/com/crategus/gdk-pixbuf/ducky.png"
                      128
                      128
                      t)
                 'gdk-pixbuf:pixbuf)))))

;;;     gdk_pixbuf_get_file_info

(test gdk-pixbuf-file-info.1
  (let ((path (glib-sys:sys-path "test/resource/floppybuddy.gif")))
    (multiple-value-bind (format width height)
        (gdk-pixbuf:pixbuf-file-info path)
      (is (= 80 width))
      (is (= 70 height))
      (is (string= "gif" (gdk-pixbuf:pixbuf-format-name format)))
      (is (string= "GIF" (gdk-pixbuf:pixbuf-format-description format)))
      (is (equal '("image/gif") (gdk-pixbuf:pixbuf-format-mime-types format)))
      (is (equal '("gif") (gdk-pixbuf:pixbuf-format-extensions format))))))

(test gdk-pixbuf-file-info.2
  (let ((path (glib-sys:sys-path "test/resource/ducky.png")))
    (multiple-value-bind (format width height)
        (gdk-pixbuf:pixbuf-file-info path)
      (is (= 489 width))
      (is (= 537 height))
      (is (string= "png" (gdk-pixbuf:pixbuf-format-name format)))
      (is (string= "PNG" (gdk-pixbuf:pixbuf-format-description format)))
      (is (equal '("image/png") (gdk-pixbuf:pixbuf-format-mime-types format)))
      (is (equal '("png") (gdk-pixbuf:pixbuf-format-extensions format))))))

;;;     gdk_pixbuf_get_file_info_async
;;;     gdk_pixbuf_get_file_info_finish

;; TODO: Improve this test, pixbuf-file-info-finish is not called

(test gdk-pixbuf-file-info-async
  (let ((path (glib-sys:sys-path "test/resource/ducky.png"))
        result msg)

    (gdk-pixbuf:pixbuf-file-info-async
            path
            nil
            (lambda (source result1)
              (declare (ignore source))
              (setf result (gdk-pixbuf:pixbuf-file-info-finish result1))
              (setf msg "in Callback")
              (is-false result)))

    (sleep 1)

    (is-false msg)
    (is-false result)

))

;;;     gdk_pixbuf_new_from_stream
;;;     gdk_pixbuf_new_from_stream_async
;;;     gdk_pixbuf_new_from_stream_finish
;;;     gdk_pixbuf_new_from_stream_at_scale
;;;     gdk_pixbuf_new_from_stream_at_scale_async

;;; 2024-6-16
