(push #p"projects/game-engine/" asdf:*central-registry*)
(push #p"projects/gemstone-hunter/" asdf:*central-registry*)

(asdf:oos 'asdf:load-op 'tile-engine)
(asdf:oos 'asdf:load-op 'level-editor)
(asdf:oos 'asdf:load-op 'gemstone-hunter)

#+(and win32 sbcl)
(defun win32-workaround ()
  ;; First time `within-main-loop` throw divide-by-zero
  ;; error. Handle it.
  (sb-int:with-float-traps-masked (:divide-by-zero)
    (gtk:within-main-loop
      (let ( ;; Create a toplevel window.
	    (window (gtk:gtk-window-new :toplevel)))
	;; Signal handler for the window to handle the signal "destroy".
	(gobject:g-signal-connect window "destroy"
				  (lambda (widget)
				    (declare (ignore widget))
				    (gtk:leave-gtk-main)))
	;; Destroy window after 100 milliseconds
	(glib:g-timeout-add 100 (lambda () (gtk:gtk-widget-destroy window)))
	;; Show the window.
	(gtk:gtk-widget-show-all window)))))

#+(and win32 sbcl)
(win32-workaround)
