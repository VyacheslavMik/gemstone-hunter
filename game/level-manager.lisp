(in-package #:gemstone-hunter)

(defparameter *player* nil)
(defparameter *current-level* 0)
(defparameter *gemstones* nil)
(defparameter *enemies* nil)
(defparameter *maps-directories* '("projects/gemstone-hunter/game/maps/"
				   "maps/"))

(defun get-level-path (level-number)
  (format nil "~aMAP~3,'0d.MAP" (get-suitable-directory *maps-directories*) level-number))

(defun create-enemy (x y)
  (let ((type (case (random 5)
		(1 'B)
		(2 'C)
		(3 'D)
		(t 'A))))
    (make-enemy x y type)))

(defun load-level (level-number)
  (with-open-file (stream (get-level-path level-number))
    (tile-map-load-map stream))

  (setf *gemstones* nil
	*enemies* nil)

  (loop for x from 0 below +tile-map-map-width+
     do (loop for y from 0 below +tile-map-map-height+
	   do (progn
		(when (string= (tile-map-cell-code-value x y) "START")
		  (setf (world-location *player*) (make-vector-2 :x (->float (* x +tile-map-tile-width+))
								 :y (->float (* y +tile-map-tile-height+)))))
		
		(when (string= (tile-map-cell-code-value x y) "GEM")
		  (setf *gemstones* (append *gemstones* (list (make-gemstone x y)))))

		(when (string= (tile-map-cell-code-value x y) "ENEMY")
		  (setf *enemies* (append *enemies* (list (create-enemy x y))))))))

  (setf *current-level* level-number)
  (setf *respawn-location* (copy (world-location *player*))))

(defun reload-level ()
  (let ((save-respawn (copy *respawn-location*)))
    (load-level *current-level*)
    (setf *respawn-location* save-respawn
	  (world-location *player*) (copy *respawn-location*))))

(defun check-current-cell-code ()
  (let ((code (tile-map-cell-code-value (tile-map-get-cell-by-pixel (world-location *player*)))))
    (when (string= code "DEAD")
      (kill *player*))))

(defun level-manager-draw ()
  (map nil #'draw-obj *gemstones*)
  (map nil #'draw-obj *enemies*))

(defun level-manager-update (total-seconds)
  (unless (dead *player*)
    (check-current-cell-code)

    (setf *gemstones*
	  (remove-if (lambda (gem)
		       (update-obj gem total-seconds)
		       (if (intersects? (collision-rectangle *player*) (collision-rectangle gem))
			   (progn
			     (incf (score *player*) 10)
			     t)
			   nil))
		     *gemstones*))
    (setf *enemies*
	  (remove-if (lambda (enemy)
		       (update-obj enemy total-seconds)

		       (let ((res nil))
			 (if (not (dead enemy))
			     (if (intersects? (collision-rectangle *player*) (collision-rectangle enemy))
				 (if (< (vector-2-y (world-center *player*)) (vector-2-y (world-location enemy)))
				     (progn
				       (jump *player*)
				       (incf (score *player*) 5)
				       (play-animation enemy "die")
				       (setf (dead enemy) t))
				     (kill *player*)))
			     (when (not (enabled enemy))
			       (setf res t)))
			 res))
		     *enemies*))))
