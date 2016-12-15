(in-package #:gemstone-hunter)

(defparameter *score-position* (make-vector-2 :x 20.0 :y 578.0))
(defparameter *game-state* nil)
(defparameter *game-over-position* (make-vector-2 :x 350.0 :y 300.0))
(defparameter *lives-position* (make-vector-2 :x 600.0 :y 578.0))

(defparameter *death-timer* 0.0)
(defparameter *death-delay* 5.0)

(defparameter *title-screen* nil)

(defun start-new-game ()
  (revive *player*)
  (setf (lives-remaining *player*) 3)
  (setf (score *player*) 0)
  (setf (world-location *player*) (make-vector-2))

  (load-level 0))

(defun init ()
  (load-default-font "Vera.ttf" 18)
  (setf *game-state* 'title-screen)
  (setf *tile-map-editor-mode* nil)
  (setf *respawn-location* nil)
  (setf *death-timer* 0.0)
  (tile-map-initialize (load-a-texture (get-texture-path "PlatformTiles.png")))
  (setf *title-screen* (load-a-texture (get-texture-path "TitleScreen.png")))
  (setf (camera-world-rectangle) (make-rectangle :width (* 160 48) :height (* 12 48)))
  (setf (camera-position) (make-vector-2))
  (setf (camera-view-port-width) 800)
  (setf (camera-view-port-height) 600)
  
  (setf *player* (make-player)))

(defun draw ()
  (clear-color (make-black-color))

  (with-deferred-draw
    (when (eq *game-state* 'title-screen)
      (draw-tex-rect *title-screen* (make-vector-2) nil (make-white-color) 0 (make-vector-2)))

    (when (member *game-state* '(playing player-dead game-over))
      (tile-map-draw)
      (draw-obj *player*)
      (level-manager-draw)

      (draw-string (format nil "Score: ~d" (score *player*))
      		   *score-position*
      		   (make-white-color)
      		   :centered nil)
      (draw-string (format nil "Lives Remaining: ~d" (lives-remaining *player*))
      		   *lives-position*
      		   (make-white-color)
      		   :centered nil))

    (when (eq *game-state* 'game-over)
      (draw-string "G A M E  O V E R !"
    		   *game-over-position*
    		   (make-white-color)))))

(defun update (total-seconds)
  (when (and (eq *game-state* 'title-screen)
	     (key-pressed? :sdl-key-space))
    (start-new-game)
    (setf *game-state* 'playing))

  (when (eq *game-state* 'playing)
    (update-obj *player* total-seconds)
    (level-manager-update total-seconds)

    (when (dead *player*)
      (if (> (lives-remaining *player*) 0)
	  (setf *game-state* 'player-dead
		*death-timer* 0.0)
	  (setf *game-state* 'game-over
		*death-timer* 0.0))))

  (when (eq *game-state* 'player-dead)
    (update-obj *player* total-seconds)
    (level-manager-update total-seconds)
    (incf *death-timer* total-seconds)

    (when (> *death-timer* *death-delay*)
      (setf (world-location *player*) (make-vector-2))
      (reload-level)
      (revive *player*)
      (setf *game-state* 'playing)))

  (when (eq *game-state* 'game-over)
    (incf *death-timer* total-seconds)

    (when (> *death-timer* *death-delay*)
      (setf *game-state* 'title-screen))))

(defun run-game ()
  (setf *title-caption* "Gemstone Hunter")
  (setf *icon-caption* "Gemstone Hunter")

  (main-loop #'init #'draw #'update))
