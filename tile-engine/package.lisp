(in-package #:cl-user)

(defpackage #:tile-engine
  (:use #:cl
	#:game-engine)
  (:export
   ;; camera
   #:camera-view-port-width
   #:camera-view-port-height
   #:camera-move
   #:camera-world-rectangle
   #:camera-position
   #:camera-screen-to-world
   #:camera-world-to-screen

   ;; tile map constants
   #:+tile-map-tile-width+
   #:+tile-map-tile-height+
   #:+tile-map-map-width+
   #:+tile-map-map-height+
   #:+tile-map-tile-width+
   #:+tile-map-tile-height+

   ;; tile map params
   #:*tile-map-editor-mode*

   ;; tile map functions
   #:tile-map-draw
   #:tile-map-initialize
   #:tile-map-set-tile-at-cell
   #:tile-map-get-cell-by-pixel-x
   #:tile-map-get-cell-by-pixel-y
   #:tile-map-get-map-square-at-cell
   #:tile-map-save-map
   #:tile-map-load-map
   #:tile-map-clear-map
   #:tile-map-cell-code-value
   #:tile-map-cell-is-passable?
   #:tile-map-get-cell-by-pixel

   ;; map square macros
   #:toggle-passable
   #:code-value))
