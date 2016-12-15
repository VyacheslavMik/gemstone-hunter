(in-package #:level-editor)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *debug* t))

;; helper macros and functions
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))

  (defun symb (&rest args)
    (values (intern (apply #'mkstr args)))))
;; helper macros and functions end

(defparameter *hdc* nil)
(defparameter *gl-context* nil)
(defparameter *hwnd* nil)
(defparameter *window-destroyed* nil)
(defparameter *window-destroying* nil)
(defparameter *stdout* *standard-output*)
(defparameter *window* nil)
(defparameter *current-code* nil)
(defparameter *map-number* nil)

(defmacro log-msg (control-string &rest format-arguments)
  (when *debug*
    (let ((s (gensym)))
      `(let ((*standard-output* *stdout*))
	 (write-line (with-output-to-string (,s)
		       (format ,s ,control-string ,@format-arguments)))))))

(defcfun ("gdk_win32_window_get_handle" gdk-win32-window-get-handle) :pointer
  (window :pointer))

(defcfun ("gtk_widget_get_window" gtk-widget-get-window) :pointer
  (widget :pointer))

(defcfun ("gdk_pixbuf_new_from_file" gdk-pixbuf-new-from-file)
    (g-object gdk-pixbuf:gdk-pixbuf)
  (filename :string)
  (error :pointer))

(defcfun ("gdk_pixbuf_new_subpixbuf" gdk-pixbuf-new-subpixbuf)
    (g-object gdk-pixbuf:gdk-pixbuf)
  (src-pixbuf (g-object gdk-pixbuf:gdk-pixbuf))
  (src-x :int)
  (src-y :int)
  (src-width :int)
  (src-height :int))

(defun get-map-path (map-number)
  (get-file-path *maps-directories* (format nil "MAP~a.MAP" map-number)))

(defun create-gl-context (widget)
  (let* (gdk-window hwnd)
    (log-msg "test : ~a" (pointer widget))

    (setf gdk-window (gtk-widget-get-window (pointer widget)))
    (log-msg "test2 : ~a" gdk-window)
    (when (null-pointer-p gdk-window) (return-from create-gl-context nil))

    (setf hwnd (gdk-win32-window-get-handle gdk-window))
    (log-msg "test3 : ~a" hwnd)
    (when (null-pointer-p hwnd) (return-from create-gl-context nil))
    
    (setf *hdc* (glop-win32:get-dc hwnd))
    (when (null-pointer-p *hdc*) (return-from create-gl-context nil))
    
    (glop-win32:choose-pixel-format *hdc*)
    (let ((gl-context (glop-wgl:wgl-create-context *hdc*)))
      (glop-wgl:wgl-make-current *hdc* gl-context)
      gl-context)))

(defun timer-tick ()
  (if *window-destroyed*
      (progn
	(log-msg "window destroyed - leave gtk main and stop timer")
	(gtk:leave-gtk-main)
	nil)
      (progn
	(handler-case
	    (when (or (not *window-destroying*) (and *hdc* *gl-context* *event-box*))
	      (gl:clear :color-buffer-bit)
	      
	      (update)
	      (draw)

	      (gl:flush)
	      (glop-win32:swap-buffers *hdc*))
	  (gl:opengl-error (err)
	    (log-msg "OpenGL error : ~a" (cl-opengl-bindings::opengl-error.error-code err))
	    nil)
	  ;; (t (datum)
	  ;;   (log-msg "Some error : ~a" datum)
	  ;;   nil)
	  )
	t)))

(defun destroy-window (widget)
  (declare (ignore widget))
  (setf *window-destroying* t)
  (when *gl-context*
    (glop-wgl:wgl-delete-context *gl-context*)
    (setf *gl-context* nil))
  (when (and *hdc* *hwnd*)
    (glop-win32::%release-dc *hwnd* *hdc*)
    (setf *hwnd* nil)
    (setf *hdc* nil))
  (when *event-box*
    (setf *event-box* nil))
  (setf *window-destroyed* t)
  (setf *window* nil)
  (log-msg "--------------window-destroyed--------------"))

(defmacro def-menu-click (menu-name &body body)
  `(defun ,(symb (symbol-name menu-name) "-CLICK") (widget)
     (declare (ignorable widget))
     ,@body))

(def-menu-click load-map
  (let ((dialog (gtk:gtk-message-dialog-new *window*
					    '(:modal)
					    :question
					    :ok-cancel
					    "Map changed. Are you really load map?")))
    (when (or (not *changed*) (eq (gtk:gtk-dialog-run dialog) :ok))
      (with-open-file (stream (get-map-path *map-number*))
	(tile-map-load-map stream))
      (setf *changed* nil))
    (gtk:gtk-widget-destroy dialog)))

(def-menu-click exit
  (gtk:gtk-widget-destroy *window*))

(def-menu-click save-map
  (with-open-file (stream (get-map-path *map-number*) :direction :output :if-exists :supersede)
    (tile-map-save-map stream))
  (setf *changed* nil))

(def-menu-click clear-map
  (tile-map-clear-map))

(def-menu-click background
  (when (gtk:gtk-check-menu-item-active widget)
    (setf *draw-layer* 0)))

(def-menu-click interactive
  (when (gtk:gtk-check-menu-item-active widget)
    (setf *draw-layer* 1)))

(def-menu-click foreground
  (when (gtk:gtk-check-menu-item-active widget)
    (setf *draw-layer* 2)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gen-menu-item (menu-item menu gsep &optional (group-p nil) (first-group-item nil))
    (let ((gmenu-item (gensym)))
      (values
       (if (eq (car menu-item) :menu-item)
	   (if group-p
	       (if first-group-item
		   `(,gmenu-item (gtk:gtk-radio-menu-item-new-with-label-from-widget ,first-group-item ,(cadr menu-item)))
		   `(,gmenu-item (gtk:gtk-radio-menu-item-new-with-label nil ,(cadr menu-item))))
	       `(,gmenu-item (gtk:gtk-menu-item-new-with-label ,(cadr menu-item)))))
       (if (eq (car menu-item) :menu-item)
	   (if (and group-p (not first-group-item))
	       `((gtk:gtk-menu-shell-append ,menu ,gmenu-item)
		 (g-signal-connect ,gmenu-item "activate" ,(caddr menu-item))
		 (gtk:gtk-check-menu-item-set-active ,gmenu-item t))
	       `((gtk:gtk-menu-shell-append ,menu ,gmenu-item)
		 (g-signal-connect ,gmenu-item "activate" ,(caddr menu-item))))
	   `((gtk:gtk-menu-shell-append ,menu ,gsep)))
       gmenu-item)))

  (defun gen-menu-item-group (menu-item-group menu gsep)
    (let (let-bindings code first-group-item)
      (dolist (menu-item (cdr menu-item-group))
	(multiple-value-bind (b c gmenu-item) (gen-menu-item menu-item menu gsep t first-group-item)
	  (unless first-group-item
	    (setf first-group-item gmenu-item))
	  (when b (push b let-bindings))
	  (setf code (append code c))))
      (values let-bindings code)))

  (defun gen-menu (menu gsep)
    (let (let-bindings code (gmenu (gensym)) (gitem (gensym)))
      (push `(,gmenu (gtk:gtk-menu-new)) let-bindings)
      (push `(,gitem (gtk:gtk-menu-item-new-with-label ,(cadr menu))) let-bindings)

      (push `(setf (gtk:gtk-menu-item-submenu ,gitem) ,gmenu) code)

      (dolist (menu-item (cddr menu))
	(multiple-value-bind (b c)
	    (if (eq (car menu-item) :menu-item-group)
		(gen-menu-item-group menu-item gmenu gsep)
		(gen-menu-item menu-item gmenu gsep))
	  (when b
	    (if (eq (car menu-item) :menu-item-group)
		(map nil (lambda (item)
			   (push item let-bindings)) b)
		(push b let-bindings)))
	  (setf code (append code c))))
      (values let-bindings code gitem)))

  (defun gen-menu-bar (menu-bar)
    (let (let-bindings body (gmenu-bar (gensym)) (gsep (gensym)))
      (push `(,gmenu-bar (gtk:gtk-menu-bar-new)) let-bindings)
      (push `(,gsep (gtk:gtk-separator-menu-item-new)) let-bindings)
      (dolist (menu (cdr menu-bar))
	(multiple-value-bind (bindings code gmenu) (gen-menu menu gsep)
	  (setf let-bindings (append let-bindings bindings))
	  (setf body (append body code))
	  (setf body (append body `((gtk:gtk-menu-shell-append ,gmenu-bar ,gmenu))))))
      (setf body (append body `(,gmenu-bar)))
      `(let* ,let-bindings
	 ,@body))))

(defmacro defmenu (name menu-bar)
  `(defun ,(symb "MAKE-" (symbol-name name)) ()
     ,(gen-menu-bar menu-bar)))

(defmenu main-menu
    (:menu-bar
     (:menu "File"
	    (:menu-item "Load Map" #'load-map-click)
	    (:menu-item "Save Map" #'save-map-click)
	    (:separator)
	    (:menu-item "Exit" #'exit-click))
     (:menu "Tools"
	    (:menu-item "Clear Map" #'clear-map-click))
     (:menu "Layer"
	    (:menu-item-group
	     (:menu-item "Background" #'background-click)
	     (:menu-item "Interactive" #'interactive-click)
	     (:menu-item "Foreground" #'foreground-click)))))

(defun change-adjustment (adjustment maximum)
  (gtk:gtk-adjustment-set-upper adjustment maximum)
  (gtk:gtk-adjustment-set-value adjustment 0))

(defun event-box-size-allocate (allocation v-adjustment h-adjustment)
  (when (and *event-box* *hdc*)
    (setup-ortho-projection
		 (gdk:gdk-rectangle-width allocation)
		 (gdk:gdk-rectangle-height allocation))

    (setf (camera-view-port-width) (gdk:gdk-rectangle-width allocation))
    (setf (camera-view-port-height) (gdk:gdk-rectangle-height allocation))

    (camera-move (make-vector-2))

    (change-adjustment v-adjustment (- (rectangle-height (camera-world-rectangle)) (camera-view-port-height)))
    (change-adjustment h-adjustment (- (rectangle-width (camera-world-rectangle)) (camera-view-port-width)))))

(defun load-platform-tiles (list-box)
  (let* ((platform-tiles (gdk-pixbuf-new-from-file
			  (get-texture-path "PlatformTiles.png")
			  (cffi:null-pointer)))
	 (width (gdk-pixbuf:gdk-pixbuf-width platform-tiles))
	 (height (gdk-pixbuf:gdk-pixbuf-height platform-tiles))
	 (tile-count 0))
    (dotimes (y (/ height +tile-map-tile-height+))
      (dotimes (x (/ width +tile-map-tile-width+))
	(when (< tile-count 29)
	  (let ((image (gtk:gtk-image-new-from-pixbuf
			(gdk-pixbuf-new-subpixbuf
			 platform-tiles
			 (* x +tile-map-tile-width+)
			 (* y +tile-map-tile-height+)
			 +tile-map-tile-width+
			 +tile-map-tile-height+))))
	    (let ((box (gtk:gtk-box-new :vertical 5))
		  (label (gtk:gtk-label-new (case tile-count (0 "Empty : 0") (1 "White : 1") (t (format nil "~a" tile-count))))))
	      (gtk:gtk-box-pack-start box image)
	      (gtk:gtk-box-pack-start box label)
	      
	      (setf (gtk:gtk-widget-margin-left box) 5)
	      (setf (gtk:gtk-widget-margin-top box) 5)
	      (setf (gtk:gtk-widget-margin-right box) 5)
	      (setf (gtk:gtk-widget-margin-bottom box) 5)
      
	      (gtk:gtk-list-box-insert list-box box tile-count)
	      (incf tile-count))))))))

(defun fill-code-values (combo-box)
  (flet ((add-item (s)
	   (gtk:gtk-combo-box-text-append-text combo-box s)))
    (add-item "Gemstone")
    (add-item "Enemy")
    (add-item "Lethal")
    (add-item "EnemyBlocking")
    (add-item "Start")
    (add-item "Clear")
    (add-item "Custom")))

(defun fill-map-numbers (combo-box)
  (dotimes (x 100)
    (gtk:gtk-combo-box-text-append-text
     combo-box
     (format nil "~3,'0d" x))))

(defun event-box-motion-notify-event (widget event)
  (declare (ignore widget))
  (setf (car *mouse-state*) (gdk:gdk-event-motion-x event)
	(cadr *mouse-state*) (gdk:gdk-event-motion-y event))
  (unless (equal *hover-code-value* (gtk:gtk-label-get-text *current-code*))
    (gtk:gtk-label-set-text *current-code* *hover-code-value*))
  nil)

(defun event-box-button-event (widget event)
  (declare (ignore widget))
  (when (or (eq (gdk:gdk-event-button-type event) :button-press)
	    (eq (gdk:gdk-event-button-type event) :button-release))
    (when (eq (gdk:gdk-event-button-button event) 1)
      (setf (caddr *mouse-state*) (gdk:gdk-event-button-type event)))
    (when (eq (gdk:gdk-event-button-button event) 3)
      (setf (cadddr *mouse-state*) (gdk:gdk-event-button-type event))))
  nil)

(defun main ()
  (gtk:within-main-loop
    (setf *hdc* nil)
    (setf *event-box* nil)
    (setf *gl-context* nil)
    (setf *event-box* nil)
    (setf *window* nil)
    (setf *window-destroyed* nil)
    (setf *window-destroying* nil)
    (setf *h-adjustment* nil)
    (setf *v-adjustment* nil)
    (setf *mouse-state* (copy-list *default-mouse-state*))
    (setf *current-code* nil)
    (setf *map-number* "000")
    
    (log-msg "--------------start--------------")
    (let* ((grid (gtk:gtk-grid-new))
	   (list-box (gtk:gtk-list-box-new))
	   (h-adjustment (gtk:gtk-adjustment-new 0 0 100 1 0 0))
	   (h-scroll (gtk:gtk-scrollbar-new :horizontal h-adjustment))
	   (v-adjustment (gtk:gtk-adjustment-new 0 0 100 1 0 0))
	   (v-scroll (gtk:gtk-scrollbar-new :vertical v-adjustment))
	   (scroll-window (gtk:gtk-scrolled-window-new))
	   (right-click-mode-grid (gtk:gtk-grid-new))
	   (radio-button-1 (gtk:gtk-radio-button-new-with-label nil "Toggle Passable"))
	   (radio-button-2 (gtk:gtk-radio-button-new-with-label-from-widget radio-button-1 "Code"))
	   (code-box (gtk:gtk-box-new :horizontal 5))
	   (entry (gtk:gtk-entry-new))
	   (current-code (gtk:gtk-label-new "---"))
	   (code-values (gtk:gtk-combo-box-text-new))
	   (map-number-label (gtk:gtk-label-new "Map Number:"))
	   (map-number-box (gtk:gtk-box-new :horizontal 5))
	   (map-number-combo-box (gtk:gtk-combo-box-text-new)))

      (setf *h-adjustment* h-adjustment)
      (setf *v-adjustment* v-adjustment)

      (setf *window* (gtk:gtk-window-new :toplevel))
      (setf *event-box* (gtk:gtk-event-box-new))
      (setf *current-code* current-code)

      (gtk:gtk-window-set-default-geometry *window* 700 671)
      (setf (gtk:gtk-window-title *window*) "MapEditor")

      (gtk:gtk-container-add code-box radio-button-2)
      (gtk:gtk-container-add code-box entry)

      (gtk:gtk-container-add map-number-box map-number-label)
      (gtk:gtk-container-add map-number-box map-number-combo-box)

      (fill-code-values code-values)
      (fill-map-numbers map-number-combo-box)

      (setf (gtk:gtk-combo-box-active map-number-combo-box) 0)

      (gtk:gtk-combo-box-set-wrap-width map-number-combo-box 5)

      (setf (gtk:gtk-widget-margin-top current-code) 5)
      (setf (gtk:gtk-widget-margin-top code-values) 5)
      (setf (gtk:gtk-widget-margin-top map-number-box) 5)

      (gtk:gtk-grid-attach right-click-mode-grid (gtk:gtk-label-new "Right Click Mode") 0 0 1 1)
      (gtk:gtk-grid-attach right-click-mode-grid radio-button-1                         0 1 1 1)
      (gtk:gtk-grid-attach right-click-mode-grid code-box                               0 2 1 1)
      (gtk:gtk-grid-attach right-click-mode-grid current-code                           0 3 1 1)
      (gtk:gtk-grid-attach right-click-mode-grid code-values                            0 4 1 1)
      (gtk:gtk-grid-attach right-click-mode-grid map-number-box                         0 5 1 1)

      (setf (gtk:gtk-widget-margin-top right-click-mode-grid) 5)
      (setf (gtk:gtk-widget-margin-left right-click-mode-grid) 5)
      (setf (gtk:gtk-widget-margin-right right-click-mode-grid) 5)

      (setf (gtk:gtk-widget-expand *event-box*) t)

      (gtk:gtk-container-add scroll-window list-box)

      (load-platform-tiles list-box)
      
      (gtk:gtk-container-add *window* grid)
      (gtk:gtk-grid-attach grid (make-main-menu)            0 0 3 1)
      (gtk:gtk-grid-attach grid scroll-window               0 1 1 1)
      (gtk:gtk-grid-attach grid right-click-mode-grid       0 2 1 1)
      (gtk:gtk-grid-attach grid *event-box*                 1 1 1 2)
      (gtk:gtk-grid-attach grid v-scroll                    2 1 1 1)
      (gtk:gtk-grid-attach grid h-scroll                    1 3 1 1)

      (setf (gtk:gtk-widget-events *event-box*) '(:pointer-motion-mask :button-press-mask))
      
      (gtk:gtk-widget-show-all *window*)
      
      (setf *gl-context* (create-gl-context *event-box*))
      (g-signal-connect *window* "destroy" #'(lambda (widget)
					       (declare (ignore widget))
					       (setf *window-destroying* t)))
      (g-signal-connect *window* "destroy" #'destroy-window :after t)
      (g-signal-connect *event-box* "size-allocate" (lambda (widget allocation)
						      (declare (ignore widget))
						      (funcall #'event-box-size-allocate
							       allocation
							       v-adjustment
							       h-adjustment)))
      (g-signal-connect list-box "row-activated" (lambda (widget row)
						   (declare (ignore widget))
						   (setf *draw-tile* (gtk:gtk-list-box-row-get-index row))))
      (g-signal-connect radio-button-1 "toggled" (lambda (widget)
						   (setf *editing-code* (not (gtk:gtk-toggle-button-active widget)))))
      (g-signal-connect entry "changed" (lambda (widget)
					  (setf *current-code-value* (gtk:gtk-entry-text widget))))
      (g-signal-connect code-values "changed" (lambda (widget)
						(flet ((set-code (code)
							 (setf (gtk:gtk-entry-text entry) code)))
						  (setf (gtk:gtk-widget-sensitive entry) nil)
						  (switch ((gtk:gtk-combo-box-text-get-active-text widget) :test #'equal)
						    ("Gemstone" (set-code "GEM"))
						    ("Enemy" (set-code "ENEMY"))
						    ("Lethal" (set-code "DEAD"))
						    ("EnemyBlocking" (set-code "BLOCK"))
						    ("Start" (set-code "START"))
						    ("Clear" (set-code ""))
						    ("Custom"
						     (set-code "")
						     (setf (gtk:gtk-widget-sensitive entry) t))))))
      (g-signal-connect map-number-combo-box "changed" (lambda (widget)
							 (let ((map-number (gtk:gtk-combo-box-text-get-active-text widget)))
							   (when map-number
							     (setf *map-number* map-number)))))
      (g-signal-connect *event-box* "motion-notify-event" #'event-box-motion-notify-event)
      (g-signal-connect *event-box* "button-press-event" #'event-box-button-event)
      (g-signal-connect *event-box* "button-release-event" #'event-box-button-event)

      (init-game (gdk:gdk-rectangle-width (gtk:gtk-widget-get-allocation *event-box*))
		 (gdk:gdk-rectangle-height (gtk:gtk-widget-get-allocation *event-box*)))

      (setf *tile-map-editor-mode* t)

      (glib:g-timeout-add 16 #'timer-tick))))
