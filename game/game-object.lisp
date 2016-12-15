(in-package #:gemstone-hunter)

(defclass game-object ()
  ((world-location :accessor world-location :initform (make-vector-2))
   (velocity :accessor velocity :initform (make-vector-2))
   (frame-width :accessor frame-width :initform 0)
   (frame-height :accessor frame-height :initform 0)

   (enabled :accessor enabled :initform nil)
   (flipped :accessor flipped :initform nil)
   (on-ground :accessor on-ground :initform nil)

   (p-collision-rectangle :accessor p-collision-rectangle :initform (make-rectangle))
   (collide-width :accessor collide-width :initform 0)
   (collide-height :accessor collide-height :initform 0)
   (code-based-blocks :accessor code-based-blocks :initform t)

   (draw-depth :accessor draw-depth :initform 0.85)
   (animations :accessor animations :initform (make-hash-table :test 'equal))
   (current-animation :accessor current-animation :initform nil)))

(defmethod world-center ((obj game-object))
  (with-slots (world-location frame-width frame-height) obj
    (make-vector-2 :x (->float (+ (vector-2-x world-location) (/ frame-width 2)))
		   :y (->float (+ (vector-2-y world-location) (/ frame-height 2))))))

(defmethod world-rectangle ((obj game-object))
  (with-slots (world-location frame-width frame-height) obj
    (make-rectangle :x (->int (vector-2-x world-location))
		    :y (->int (vector-2-y world-location))
		    :width frame-width
		    :height frame-height)))

(defmethod collision-rectangle ((obj game-object))
  (with-slots (world-location p-collision-rectangle) obj
    (make-rectangle :x (+ (->int (vector-2-x world-location)) (rectangle-x p-collision-rectangle))
		    :y (+ (->int (vector-2-y world-location)) (rectangle-y p-collision-rectangle))
		    :width (rectangle-width p-collision-rectangle)
		    :height (rectangle-height p-collision-rectangle))))

(defmethod (setf collision-rectangle) (new-value (obj game-object))
  (setf (p-collision-rectangle obj) new-value))

(defmethod play-animation ((obj game-object) name)
  (when name
    (multiple-value-bind (value win) (gethash name (animations obj))
      (when win
	(setf (current-animation obj) name)
	(play value))))) 

(defmethod update-animation ((obj game-object) game-time)
  (let ((animation (gethash (current-animation obj) (animations obj))))
    (if (finished-playing animation)
	(play-animation obj (next-animation animation))
	(update-obj animation game-time))))

(defmethod horizontal-collision-test ((obj game-object) (move-amount vector-2))
  (when (= (vector-2-x move-amount) 0)
    (return-from horizontal-collision-test move-amount))

  (let ((after-move-rect (collision-rectangle obj))
	(corner-1) (corner-2) (map-cell-1) (map-cell-2))
    (setf after-move-rect
	  (rectangle-offset
	   after-move-rect
	   (->int (vector-2-x move-amount)) 0))

    (if (< (vector-2-x move-amount) 0)
	(setf corner-1 (make-vector-2 :x (->float (rectangle-left after-move-rect))
				      :y (->float (+ (rectangle-top after-move-rect) 1)))
	      corner-2 (make-vector-2 :x (->float (rectangle-left after-move-rect))
				      :y (->float (- (rectangle-bottom after-move-rect) 1))))	
	(setf corner-1 (make-vector-2 :x (->float (rectangle-right after-move-rect))
				      :y (->float (+ (rectangle-top after-move-rect) 1)))
	      corner-2 (make-vector-2 :x (->float (rectangle-right after-move-rect))
				      :y (->float (- (rectangle-bottom after-move-rect) 1)))))
    
    (setf map-cell-1 (tile-map-get-cell-by-pixel corner-1)
	  map-cell-2 (tile-map-get-cell-by-pixel corner-2))

    (when (or (not (tile-map-cell-is-passable? map-cell-1))
	      (not (tile-map-cell-is-passable? map-cell-2)))
      (setf (vector-2-x move-amount) 0.0)
      (setf (vector-2-x (velocity obj)) 0.0))

    (when (code-based-blocks obj)
      (when (or (string= (tile-map-cell-code-value map-cell-1) "BLOCK")
		(string= (tile-map-cell-code-value map-cell-2) "BLOCK"))
	(setf (vector-2-x move-amount) 0.0)
	(setf (vector-2-x (velocity obj)) 0.0)))

    move-amount))

(defmethod vertical-collision-test ((obj game-object) (move-amount vector-2))
  (when (= (vector-2-y move-amount) 0)
    (return-from vertical-collision-test move-amount))

  (let ((after-move-rect (collision-rectangle obj))
	(corner-1) (corner-2) (map-cell-1) (map-cell-2))
    (setf after-move-rect
	  (rectangle-offset
	   after-move-rect
	   (->int (vector-2-x move-amount))
	   (->int (vector-2-y move-amount))))

    (if (< (vector-2-y move-amount) 0)
	(setf corner-1 (make-vector-2 :x (->float (+ (rectangle-left after-move-rect) 1))
				      :y (->float (rectangle-top after-move-rect)))
	      corner-2 (make-vector-2 :x (->float (- (rectangle-right after-move-rect) 1))
				      :y (->float (rectangle-top after-move-rect))))
	(setf corner-1 (make-vector-2 :x (->float (+ (rectangle-left after-move-rect) 1))
				      :y (->float (rectangle-bottom after-move-rect)))
	      corner-2 (make-vector-2 :x (->float (- (rectangle-right after-move-rect) 1))
				      :y (->float (rectangle-bottom after-move-rect)))))
    
    (setf map-cell-1 (tile-map-get-cell-by-pixel corner-1)
	  map-cell-2 (tile-map-get-cell-by-pixel corner-2))

    (when (or (not (tile-map-cell-is-passable? map-cell-1))
	      (not (tile-map-cell-is-passable? map-cell-2)))
      (when (> (vector-2-y move-amount) 0)
	(setf (on-ground obj) t))
      (setf (vector-2-y move-amount) 0.0)
      (setf (vector-2-y (velocity obj)) 0.0))

    (when (code-based-blocks obj)
      (when (or (string= (tile-map-cell-code-value map-cell-1) "BLOCK")
		(string= (tile-map-cell-code-value map-cell-2) "BLOCK"))
	(when (> (vector-2-y move-amount) 0)
	  (setf (on-ground obj) t))
	(setf (vector-2-y move-amount) 0.0)
	(setf (vector-2-y (velocity obj)) 0.0)))

    move-amount))

(defmethod update-obj ((obj game-object) game-time)
  (when (enabled obj)
    (update-animation obj game-time)

    (when (not (= (vector-2-y (velocity obj)) 0))
      (setf (on-ground obj) nil))

    (let ((move-amount (multiply (velocity obj) game-time))
	  (new-position))
      (setf move-amount (horizontal-collision-test obj move-amount))
      (setf move-amount (vertical-collision-test obj move-amount))

      (setf new-position (add (world-location obj) move-amount))
      (setf new-position (make-vector-2 :x (->float (clamp (vector-2-x new-position)
							   0
							   (- (rectangle-width (camera-world-rectangle)) (frame-width obj))))
					:y (->float (clamp (vector-2-y new-position)
							   (* 2 (- +tile-map-tile-height+))
							   (- (rectangle-height (camera-world-rectangle)) (frame-height obj))))))
      (setf (world-location obj) new-position))))

(defmethod draw-obj ((obj game-object))
  (when (enabled obj)
    (multiple-value-bind (animation win) (gethash (current-animation obj) (animations obj))
      (when win
	(let ((effect (if (flipped obj) :flip-horizontally :none))
	      (camera-world-screen (camera-world-to-screen (world-rectangle obj))))
	  (draw-tex-rect (texture animation)
			 (make-vector-2 :x (->float (rectangle-x camera-world-screen))
					:y (->float (rectangle-y camera-world-screen)))
			 (frame-rectangle animation)
			 (make-white-color)
			 0.0
			 (make-vector-2)
			 nil
			 effect
			 (draw-depth obj)))))))
