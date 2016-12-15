(in-package :gtk)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (register-object-type "GtkListBox" 'gtk-list-box))

(define-g-object-class "GtkListBox" gtk-list-box
  (:superclass gtk-container
   :export t
   :interfaces ("AtkImplementorIface"
	        "GtkBuildable")
   :type-initializer "gtk_list_box_get_type")
  ((activate-on-single-click
    gtk-list-box-activate-on-single-click
    "activate-on-single-click" "gboolean" t t)
   (selection-mode
    gtk-list-box-selection-mode
    "selection-mode" "GtkSelectionMode" t t)
   (activatable
    gtk-list-box-activatable
    "activatable" "gboolean" t t)
   (selectable
    gtk-list-box-selectable
    "selectable" "gboolean" t t)))

(declaim (inline gtk-list-box-new))

(defun gtk-list-box-new ()
  (make-instance 'gtk-list-box))

(export 'gtk-list-box-new)

(defcfun ("gtk_list_box_insert" gtk-list-box-insert) :void
  (box (g-object gtk-list-box))
  (child (g-object gtk-widget))
  (position :int))

(export 'gtk-list-box-insert)

(defcfun ("gtk_list_box_row_get_index" gtk-list-box-row-get-index) :int
  (row (g-object gtk-bin)))

(export 'gtk-list-box-row-get-index)
