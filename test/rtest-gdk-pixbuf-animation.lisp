(in-package :gdk-pixbuf-test)

(def-suite gdk-pixbuf-animation :in gdk-pixbuf-test)
(in-suite gdk-pixbuf-animation)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkPixbufAnimationIter

(test gdk-pixbuf-animation-iter-class
  ;; Check type
  (is (g:type-is-object "GdkPixbufAnimationIter"))
  ;; Check registered name
  (is (eq 'gdk-pixbuf:pixbuf-animation-iter
          (glib:symbol-for-gtype "GdkPixbufAnimationIter")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkPixbufAnimationIter")
          (g:gtype (cffi:foreign-funcall "gdk_pixbuf_animation_iter_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkPixbufAnimationIter")))
  ;; Check children
  (is (equal '("GdkPixbufGifAnimIter" "GdkPixbufNonAnimIter"
               "GdkPixbufScaledAnimIter")
             (glib-test:list-children "GdkPixbufAnimationIter")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GdkPixbufAnimationIter")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GdkPixbufAnimationIter")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GdkPixbufAnimationIter")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkPixbufAnimationIter"
                                      GDK-PIXBUF:PIXBUF-ANIMATION-ITER
                      (:SUPERCLASS GOBJECT:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gdk_pixbuf_animation_iter_get_type")
                      NIL)
             (gobject:get-gtype-definition "GdkPixbufAnimationIter"))))

;;;     GdkPixbufAnimation

(test gdk-pixbuf-animation-class
  ;; Check type
  (is (g:type-is-object "GdkPixbufAnimation"))
  ;; Check registered name
  (is (eq 'gdk-pixbuf:pixbuf-animation
          (glib:symbol-for-gtype "GdkPixbufAnimation")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkPixbufAnimation")
          (g:gtype (cffi:foreign-funcall "gdk_pixbuf_animation_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkPixbufAnimation")))
  ;; Check children
  (is (equal '("GdkPixbufGifAnim" "GdkPixbufNonAnim" "GdkPixbufScaledAnim"
               "GdkPixbufSimpleAnim")
             (glib-test:list-children "GdkPixbufAnimation")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GdkPixbufAnimation")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GdkPixbufAnimation")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GdkPixbufAnimation")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkPixbufAnimation" GDK-PIXBUF:PIXBUF-ANIMATION
                      (:SUPERCLASS GOBJECT:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gdk_pixbuf_animation_get_type")
                      NIL)
             (gobject:get-gtype-definition "GdkPixbufAnimation"))))

;;;     GdkPixbufSimpleAnim

(test gdk-pixbuf-simple-anim-class
  ;; Check type
  (is (g:type-is-object "GdkPixbufSimpleAnim"))
  ;; Check registered name
  (is (eq 'gdk-pixbuf:pixbuf-simple-anim
          (glib:symbol-for-gtype "GdkPixbufSimpleAnim")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkPixbufSimpleAnim")
          (g:gtype (cffi:foreign-funcall "gdk_pixbuf_simple_anim_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GdkPixbufAnimation")
          (g:type-parent "GdkPixbufSimpleAnim")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GdkPixbufSimpleAnim")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GdkPixbufSimpleAnim")))
  ;; Check properties
  (is (equal '("loop")
             (glib-test:list-properties "GdkPixbufSimpleAnim")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GdkPixbufSimpleAnim")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkPixbufSimpleAnim"
                                      GDK-PIXBUF:PIXBUF-SIMPLE-ANIM
                      (:SUPERCLASS GDK-PIXBUF:PIXBUF-ANIMATION
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gdk_pixbuf_simple_anim_get_type")
                      ((LOOP PIXBUF-SIMPLE-ANIM-LOOP "loop" "gboolean" T T)))
             (gobject:get-gtype-definition "GdkPixbufSimpleAnim"))))

;;; --- Properties -------------------------------------------------------------

;;;     loop

;;; --- Functions --------------------------------------------------------------

;;;     gdk_pixbuf_animation_iter_get_pixbuf
;;;     gdk_pixbuf_animation_iter_get_delay_time
;;;     gdk_pixbuf_animation_iter_advance
;;;     gdk_pixbuf_animation_iter_on_currently_loading_frame

;;;     gdk_pixbuf_animation_new_from_file
;;;     gdk_pixbuf_animation_new_from_resource
;;;     gdk_pixbuf_animation_new_from_stream
;;;     gdk_pixbuf_animation_new_from_stream_async
;;;     gdk_pixbuf_animation_new_from_stream_finish
;;;     gdk_pixbuf_animation_ref
;;;     gdk_pixbuf_animation_unref
;;;     gdk_pixbuf_animation_get_width
;;;     gdk_pixbuf_animation_get_height
;;;     gdk_pixbuf_animation_get_iter
;;;     gdk_pixbuf_animation_is_static_image
;;;     gdk_pixbuf_animation_get_static_image

;;;     gdk_pixbuf_simple_anim_new

(test gdk-pixbuf-simple-anim-new
  (glib-test:with-check-memory (animation)
    (is (typep (setf animation
                     (gdk-pixbuf:pixbuf-simple-anim-new 200 100 10))
               'gdk-pixbuf:pixbuf-simple-anim))))

;;;     gdk_pixbuf_simple_anim_add_frame

;;; 2025-3-2
