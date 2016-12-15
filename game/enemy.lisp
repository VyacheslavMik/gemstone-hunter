(in-package #:gemstone-hunter)

(defclass enemy (game-object)
  ((dead :accessor dead :initform nil)

   (fall-speed :accessor fall-speed :initform (make-vector-2 :y 20.0))
   (walk-speed :accessor walk-speed :initform 60.0)
   (facing-left :accessor facing-left :initform t)))

(defun load-enemy-animation-texture (type name)
  (load-a-texture
   (get-texture-path (format nil "Sprites/Monster~a/~a.png" type name))))

(defun make-enemy (cell-x cell-y type)
  (let ((obj (make-instance 'enemy)))
    (let ((idle-animation (make-animation-strip (load-enemy-animation-texture type "Idle") 48 "idle"))
	  (run-animation (make-animation-strip (load-enemy-animation-texture type "Run") 48 "run"))
	  (die-animation (make-animation-strip (load-enemy-animation-texture type "Die") 48 "die")))
      (setf (loop-animation idle-animation) t

	    (frame-length run-animation) 0.1
	    (loop-animation run-animation) t
	  
	    (loop-animation die-animation) nil)

      (with-slots (animations frame-width frame-height world-location enabled code-based-blocks) obj
	(setf (gethash "idle" animations) idle-animation
	      (gethash "run" animations) run-animation
	      (gethash "die" animations) die-animation

	      frame-width 48
	      frame-height 48

	      (collision-rectangle obj) (make-rectangle :x 9 :y 1 :width 30 :height 46)

	      world-location (make-vector-2 :x (->float (* cell-x +tile-map-tile-width+))
					    :y (->float (* cell-y +tile-map-tile-height+)))
	      enabled t
	      code-based-blocks t))

      (play-animation obj "run")

      obj)))

(defmethod update-obj ((obj enemy) total-seconds)
  (let ((old-location (copy (world-location obj))))
    (when (not (dead obj))
      (setf (velocity obj) (make-vector-2 :x 0.0 :y (vector-2-y (velocity obj))))
      (let ((direction (make-vector-2 :x 1.0 :y 0.0)))
	(setf (flipped obj) t)
	(when (facing-left obj)
	  (setf direction (make-vector-2 :x -1.0 :y 0.0))
	  (setf (flipped obj) nil))
	(setf direction (multiply direction (walk-speed obj)))
	(setf (velocity obj) (add (velocity obj) direction))
	(setf (velocity obj) (add (velocity obj) (fall-speed obj)))))

    (call-next-method obj total-seconds)

    (if (not (dead obj))
	(when (equals? old-location (world-location obj))
	  (setf (facing-left obj) (not (facing-left obj))))
	(multiple-value-bind (value win) (gethash (current-animation obj) (animations obj))
	  (when (and win (finished-playing value))
	    (setf (enabled obj) nil))))))
