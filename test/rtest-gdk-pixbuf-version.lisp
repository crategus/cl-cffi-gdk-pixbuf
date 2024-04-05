(in-package :gdk-pixbuf-test)

(def-suite gdk-pixbuf-version :in gdk-pixbuf-suite)
(in-suite gdk-pixbuf-version)

;;;     gdk_pixbuf_version
;;;     gdk_pixbuf_major_version
;;;     gdk_pixbuf_minor_version
;;;     gdk_pixbuf_micro_version

(test gdk-pixbuf-version
  (is (string= "2.42.10" gdk-pixbuf:+version+))
  (is (=  2 gdk-pixbuf:+major-version+))
  (is (= 42 gdk-pixbuf:+minor-version+))
  (is (= 10 gdk-pixbuf:+micro-version+)))

;;; 2024-4-2
