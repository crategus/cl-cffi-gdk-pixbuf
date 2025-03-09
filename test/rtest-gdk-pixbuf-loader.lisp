(in-package :gdk-pixbuf-test)

(def-suite gdk-pixbuf-loader :in gdk-pixbuf-test)
(in-suite gdk-pixbuf-loader)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkPixbufLoader

(test g-object-class
  ;; Check type
  (is (g:type-is-object "GdkPixbufLoader"))
  ;; Check registered name
  (is (eq 'gdk-pixbuf:pixbuf-loader
          (glib:symbol-for-gtype "GdkPixbufLoader")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkPixbufLoader")
          (g:gtype (cffi:foreign-funcall "gdk_pixbuf_loader_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkPixbufLoader")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GdkPixbufLoader")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GdkPixbufLoader")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GdkPixbufLoader")))
  ;; Check signals
  (is (equal '("area-prepared" "area-updated" "closed" "size-prepared")
             (glib-test:list-signals "GdkPixbufLoader")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkPixbufLoader" GDK-PIXBUF:PIXBUF-LOADER
                      (:SUPERCLASS GOBJECT:OBJECT :EXPORT T :INTERFACES NIL
                       :TYPE-INITIALIZER "gdk_pixbuf_loader_get_type")
                      NIL)
             (gobject:get-gtype-definition "GdkPixbufLoader"))))

;;; --- Signals ----------------------------------------------------------------

;;;     area-prepared

(test gdk-pixbuf-loader-area-prepared-signal
  (let* ((name "area-prepared")
         (gtype (g:gtype "GdkPixbufLoader"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     area-updated

(test gdk-pixbuf-loader-area-updated-signal
  (let* ((name "area-updated")
         (gtype (g:gtype "GdkPixbufLoader"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("gint" "gint" "gint" "gint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     closed

(test gdk-pixbuf-loader-closed-signal
  (let* ((name "closed")
         (gtype (g:gtype "GdkPixbufLoader"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     size-prepared

(test gdk-pixbuf-loader-size-prepared-signal
  (let* ((name "size-prepared")
         (gtype (g:gtype "GdkPixbufLoader"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("gint" "gint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_pixbuf_loader_new

(test gdk-pixbuf-loader-new
  (glib-test:with-check-memory (loader)
    (is (typep (setf loader
                     (gdk-pixbuf:pixbuf-loader-new)) 'gdk-pixbuf:pixbuf-loader))))

;;;     gdk_pixbuf_loader_new_with_type
;;;     gdk_pixbuf_loader_new_with_mime_type
;;;     gdk_pixbuf_loader_get_format
;;;     gdk_pixbuf_loader_write
;;;     gdk_pixbuf_loader_write_bytes
;;;     gdk_pixbuf_loader_set_size
;;;     gdk_pixbuf_loader_get_pixbuf
;;;     gdk_pixbuf_loader_get_animation
;;;     gdk_pixbuf_loader_close

;;; 2025-3-1
