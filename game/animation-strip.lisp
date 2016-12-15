(in-package #:gemstone-hunter)

(defclass animation-strip ()
  ((texture :accessor texture :initarg :texture)
   (frame-width :accessor frame-width :initarg :frame-width)
   (frame-height :accessor frame-height :initarg :frame-height)

   (frame-timer :accessor frame-timer :initform 0.0)
   (frame-delay :accessor frame-delay :initform 0.05)

   (current-frame :accessor current-frame :initform 0)

   (loop-animation :accessor loop-animation :initform t)
   (finished-playing :accessor finished-playing :initform nil)

   (name :accessor name :initarg :name)

   (next-animation :accessor next-animation :initform nil)))

(defmethod frame-count ((obj animation-strip))
  (->int (/ (texture-width (texture obj)) (frame-width obj))))

(defmethod frame-length ((obj animation-strip))
  (frame-delay obj))

(defmethod (setf frame-length) (new-value (obj animation-strip))
  (setf (frame-delay obj) new-value))

(defmethod frame-rectangle ((obj animation-strip))
  (make-rectangle :x (* (current-frame obj) (frame-width obj))
		  :y 0
		  :width (frame-width obj)
		  :height (frame-height obj)))

(defun make-animation-strip (texture frame-width name)
  (make-instance
   'animation-strip
   :name name
   :texture texture
   :frame-width frame-width
   :frame-height (texture-heigth texture)))

(defmethod play ((obj animation-strip))
  (setf (current-frame obj) 0)
  (setf (finished-playing obj) nil))

(defmethod update-obj ((obj animation-strip) game-time)
  (incf (frame-timer obj) game-time)
  (when (> (frame-timer obj) (frame-delay obj))
    (incf (current-frame obj))

    (when (>= (current-frame obj) (frame-count obj))
      (if (loop-animation obj)
	  (setf (current-frame obj) 0)
	  (progn
	    (setf (current-frame obj) (1- (frame-count obj)))
	    (setf (finished-playing obj) t))))

    (setf (frame-timer obj) 0.0)))
