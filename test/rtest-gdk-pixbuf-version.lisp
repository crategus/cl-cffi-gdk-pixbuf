(in-package :gdk-pixbuf-test)

(def-suite gdk-pixbuf-version :in gdk-pixbuf-suite)
(in-suite gdk-pixbuf-version)

;;;     gdk_pixbuf_version
;;;     gdk_pixbuf_major_version
;;;     gdk_pixbuf_minor_version
;;;     gdk_pixbuf_micro_version

(test pixbuf-version
  (is (string= "2.42.8" gdk-pixbuf:+gdk-pixbuf-version+))
  (is (=  2 gdk-pixbuf:+gdk-pixbuf-major-version+))
  (is (= 42 gdk-pixbuf:+gdk-pixbuf-minor-version+))
  (is (=  8 gdk-pixbuf:+gdk-pixbuf-micro-version+)))

;;; 2022-11-28
