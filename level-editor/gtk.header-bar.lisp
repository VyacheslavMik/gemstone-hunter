(in-package :gtk)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkHeaderBar" 'gtk-header-bar))

(define-g-object-class "GtkHeaderBar" gtk-header-bar
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface"
		"GtkBuildable")
   :type-initializer "gtk_header_bar_get_type")
  ((custom-title
    gtk-header-bar-custom-title
    "custom-title" "GtkWidget" t t)
   (decoration-layout
    gtk-header-bar-decoration-layout
    "decoration-layout" "gchar" t t)
   (decoration-layout-set
    gtk-header-bar-decoration-layout-set
    "decoration-layout-set" "gboolean" t t)
   (has-subtitle
    gtk-header-bar-has-subtitle
    "has-subtitle" "gboolean" t t)
   (show-close-button
    gtk-header-bar-show-close-button
    "show-close-button" "gboolean" t t)
   (spacing
    gtk-header-bar-spacing
    "spacing" "gint" t t)
   (subtitle
    gtk-header-bar-subtitle
    "subtitle" "gchar" t t)
   (title
    gtk-header-bar-title
    "title" "gchar" t t)))

(declaim (inline gtk-header-bar-new))

(defun gtk-header-bar-new ()
  (make-instance 'gtk-header-bar))

(export 'gtk-header-bar-new)

(define-child-property "GtkHeaderBar"
                       gtk-header-bar-pack-type
                       "pack-type" "GtkPackType" t t t)

(define-child-property "GtkHeaderBar"
                       gtk-header-bar-position
                       "position" "gint" t t t)

(defcfun ("gtk_header_bar_set_title" gtk-header-bar-set-title) :void
  (bar (g-object gtk-header-bar))
  (title :string))

(export 'gtk-header-bar-set-title)

(defcfun ("gtk_header_bar_pack_start" gtk-header-bar-pack-start) :void
  (bar (g-object gtk-header-bar))
  (child (g-object gtk-widget)))

(export 'gtk-header-bar-pack-start)
