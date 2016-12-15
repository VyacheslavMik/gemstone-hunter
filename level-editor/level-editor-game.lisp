(in-package #:level-editor)

(defun get-suitable-directory (directories)
  (or (find-if #'probe-file directories)
      (error "Suitable directory not found")))

(defun get-file-path (directories filename)
  (concatenate 'string (get-suitable-directory directories) filename))

(defun get-texture-path (filename)
  (get-file-path *images-directories* filename))

(defun setup-ortho-projection (width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:viewport 0 0 width height)
  ;; this sets up the world so the screen coordinates go from 0,0 at lower
  ;; left to width,height at uppper right
  (gl:ortho 0 width height 0 0 1)
  (gl:matrix-mode :modelview))

(defun init-game (width height)
  (setf *editing-code* nil)
  (setf *current-code-value* "")
  (setf *draw-layer* 0)
  (setf *last-mouse-state* (copy-list *mouse-state*))
  (setf *draw-tile* 0)
  (setf *hover-code-value* "")
  (setf *changed* nil)
  (setf *tile-map-editor-mode* t)

  (sdl:init-video)
  (load-default-font "Vera.ttf" 8)
  (setup-ortho-projection width height)
  (tile-map-initialize (load-a-texture (get-texture-path "PlatformTiles.png")))
  (setf (camera-view-port-width) (gdk:gdk-rectangle-width (gtk:gtk-widget-get-allocation *event-box*)))
  (setf (camera-view-port-height) (gdk:gdk-rectangle-height (gtk:gtk-widget-get-allocation *event-box*)))
  (setf (camera-world-rectangle) (make-rectangle :width (* +tile-map-tile-width+ +tile-map-map-width+)
						 :height (* +tile-map-tile-height+ +tile-map-map-height+))))

(defun mouse-x ()
  (car *mouse-state*))

(defun mouse-y ()
  (cadr *mouse-state*))

(defun mouse-left-button ()
  (caddr *mouse-state*))

(defun mouse-right-button ()
  (cadddr *mouse-state*))

(defun update ()
  (setf (camera-position)
	(make-vector-2
	 :x (->float (gtk:gtk-adjustment-value *h-adjustment*))
	 :y (->float (gtk:gtk-adjustment-value *v-adjustment*))))

  (let ((mouse-loc (camera-screen-to-world (make-vector-2 :x (mouse-x) :y (mouse-y)))))
    (when (rectangle-contains (camera-world-rectangle) mouse-loc)
      (when (eq (mouse-left-button) :button-press)
	(tile-map-set-tile-at-cell (tile-map-get-cell-by-pixel-x (vector-2-x mouse-loc))
				   (tile-map-get-cell-by-pixel-y (vector-2-y mouse-loc))
				   *draw-layer*
				   *draw-tile*)
	(setf *changed* t))

      (when (and (eq (mouse-right-button) :button-press)
		 (eq (cadddr *last-mouse-state*) :button-release))
	(if *editing-code*
	    (setf (code-value (tile-map-get-map-square-at-cell (tile-map-get-cell-by-pixel-x (vector-2-x mouse-loc))
							       (tile-map-get-cell-by-pixel-y (vector-2-y mouse-loc)))) *current-code-value*)
	    (toggle-passable (tile-map-get-map-square-at-cell (tile-map-get-cell-by-pixel-x (vector-2-x mouse-loc))
							      (tile-map-get-cell-by-pixel-y (vector-2-y mouse-loc)))))
	(setf *changed* t))

      (setf *hover-code-value* (code-value (tile-map-get-map-square-at-cell (tile-map-get-cell-by-pixel-x (vector-2-x mouse-loc))
									    (tile-map-get-cell-by-pixel-y (vector-2-y mouse-loc)))))))

  (setf *last-mouse-state* (copy-list *mouse-state*)))

(defun draw ()
  (gl:clear-color 0 0 0 1)
  (with-deferred-draw
    (tile-map-draw)))
