(in-package #:gemstone-hunter)

(defparameter *respawn-location* nil)

(defclass player (game-object)
  ((fall-speed :accessor fall-speed :initform (make-vector-2 :x 0.0 :y 20.0))
   (move-scale :accessor move-scale :initform 180.0)
   (dead :accessor dead :initform nil)
   (score :accessor score :initform 0)
   (lives-remaining :accessor lives-remaining :initform 3)))

(defun load-player-animation-texture (name)
  (load-a-texture
   (get-texture-path (format nil "Sprites/Player/~a.png" name))))

(defun make-player ()
  (let ((obj (make-instance 'player))
	(idle-animation (make-animation-strip (load-player-animation-texture "Idle") 48 "idle"))
	(run-animation (make-animation-strip (load-player-animation-texture "Run") 48 "run"))
	(jump-animation (make-animation-strip (load-player-animation-texture "Jump") 48 "jump"))
	(die-animation (make-animation-strip (load-player-animation-texture "Die") 48 "die")))
    (setf (loop-animation idle-animation) t
	  (loop-animation run-animation) t
	  (loop-animation jump-animation) nil
	  (frame-length jump-animation) 0.08
	  (next-animation jump-animation) "idle"
	  (loop-animation die-animation) nil)
    (with-slots (animations frame-width frame-height draw-depth enabled code-based-blocks) obj
      (setf (gethash "idle" animations) idle-animation
	    (gethash "run" animations) run-animation
	    (gethash "jump" animations) jump-animation
	    (gethash "die" animations) die-animation

	    frame-width 48
	    frame-height 48

	    (collision-rectangle obj) (make-rectangle :x 9 :y 1 :width 30 :height 46)

	    draw-depth 0.825
	    enabled t
	    code-based-blocks nil))

    (play-animation obj "idle")

    obj))

(defmethod jump ((obj player))
  (with-slots (velocity) obj
    (setf (vector-2-y velocity) -500.0)))

(defmethod kill ((obj player))
  (with-slots (lives-remaining velocity dead) obj
    (play-animation obj "die")
    (decf lives-remaining)
    (setf (vector-2-x velocity) 0.0
	  dead t)))

(defmethod revive ((obj player))
  (play-animation obj "idle")
  (setf (dead obj) nil))

(defmethod check-level-transition ((obj player))
  (let ((center-cell (tile-map-get-cell-by-pixel (world-center obj))))
    (when (and (not (string= (tile-map-cell-code-value center-cell) ""))
	       (string= "T_" (subseq (tile-map-cell-code-value center-cell) 0 2)))
      (let ((code (split-string (tile-map-cell-code-value center-cell) #\_)))
	(when (not (= (length code) 4))
	  (return-from check-level-transition))

	(load-level (parse-integer (cadr code)))

	(setf (world-location obj) (make-vector-2 :x (->float (* (parse-integer (caddr code)) +tile-map-tile-width+))
						  :y (->float (* (parse-integer (cadddr code)) +tile-map-tile-height+))))
	(setf *respawn-location* (copy (world-location obj)))
	(setf (velocity obj) (make-vector-2))))))

(defmethod reposition-camera ((obj player))
  (let ((screen-loc-x (vector-2-x (camera-world-to-screen (world-location obj)))))
    (when (> screen-loc-x 500)
      (camera-move (make-vector-2 :x (->float (- screen-loc-x 500)) :y 0.0)))

    (when (< screen-loc-x 200)
      (camera-move (make-vector-2 :x (->float (- screen-loc-x 200)) :y 0.0)))))

(defmethod update-obj ((obj player) total-seconds)
  (with-slots (dead velocity flipped on-ground fall-speed move-scale current-animation) obj
    (unless dead
      (let ((new-animation "idle"))
	(setf velocity (make-vector-2 :x 0.0 :y (vector-2-y velocity)))

	(when (key-pressed? :sdl-key-left)
	  (setf flipped nil
		new-animation "run"
		velocity (make-vector-2 :x (- move-scale) :y (vector-2-y velocity))))

	(when (key-pressed? :sdl-key-right)
	  (setf flipped t
		new-animation "run"
		velocity (make-vector-2 :x move-scale :y (vector-2-y velocity))))

	(when (and (key-pressed? :sdl-key-space) on-ground)
	  (jump obj)
	  (setf new-animation "jump"))

	(when (key-pressed? :sdl-key-up)
	  (check-level-transition obj))

	(when (string= current-animation "jump")
	  (setf new-animation "jump"))

	(when (not (string= current-animation new-animation))
	  (play-animation obj new-animation))))

    (setf velocity (add velocity fall-speed))

    (reposition-camera obj)

    (call-next-method obj total-seconds)))
