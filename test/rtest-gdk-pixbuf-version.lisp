(in-package :gdk-pixbuf-test)

(def-suite gdk-pixbuf-version-suite :in gdk-pixbuf-test)
(in-suite gdk-pixbuf-version-suite)

;;;     gdk_pixbuf_version
;;;     gdk_pixbuf_major_version
;;;     gdk_pixbuf_minor_version
;;;     gdk_pixbuf_micro_version

#+crategus
(test gdk-pixbuf-version
  (is (string= "2.42.12" gdk-pixbuf:+version+))
  (is (=  2 gdk-pixbuf:+major-version+))
  (is (= 42 gdk-pixbuf:+minor-version+))
  (is (= 12 gdk-pixbuf:+micro-version+)))

#+windows
(test gdk-pixbuf-version
  (is (string= "2.42.12" gdk-pixbuf:+version+))
  (is (=  2 gdk-pixbuf:+major-version+))
  (is (= 42 gdk-pixbuf:+minor-version+))
  (is (= 12 gdk-pixbuf:+micro-version+)))

;;; 2024-10-13
