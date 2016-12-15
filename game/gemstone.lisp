(in-package #:gemstone-hunter)

(defclass gemstone (game-object)
  ())

(defun make-gemstone (cell-x cell-y)
  (let ((obj (make-instance 'gemstone)))
    (with-slots (world-location frame-width frame-height animations draw-depth enabled) obj
      (setf (vector-2-x world-location) (->float (* cell-x +tile-map-tile-width+))
	    (vector-2-y world-location) (->float (* cell-y +tile-map-tile-height+))
	    frame-width +tile-map-tile-width+
	    frame-height +tile-map-tile-height+)

      (let ((animation (make-animation-strip (load-a-texture (get-texture-path "Gem.png")) 48 "idle")))
	(setf (loop-animation animation) t)
	(setf (frame-length animation) 0.15)
	(setf (gethash "idle" animations) animation))

      (play-animation obj "idle")

      (setf draw-depth 0.875
	    (collision-rectangle obj) (make-rectangle :x 9 :y 24 :width 30 :height 24)
	    enabled t)
      obj)))

      
	    
