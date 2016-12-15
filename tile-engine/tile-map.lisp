(in-package #:tile-engine)

(defconstant +tile-map-tile-width+ 48)
(defconstant +tile-map-tile-height+ 48)
(defconstant +tile-map-map-width+ 160)
(defconstant +tile-map-map-height+ 12)
(defconstant +tile-map-map-layers+ 3)

(defconstant +tile-map-sky-tile+ 2)

(defparameter *tile-map-editor-mode* nil)

(defparameter *tile-map-map-cells* (make-array (list +tile-map-map-width+
						     +tile-map-map-height+)
					       :element-type 'simple-vector))
(defparameter *tile-map-tile-sheet* nil)

(defun tile-map-tiles-per-row ()
  (->int (/ (cadr *tile-map-tile-sheet*) +tile-map-tile-width+)))

(defun tile-map-initialize (tile-texture)
  (setf *tile-map-tile-sheet* tile-texture)

  (do ((x 0 (incf x)))
      ((>= x +tile-map-map-width+))
    (do ((y 0 (incf y)))
	((>= y +tile-map-map-height+))
      (setf (aref *tile-map-map-cells* x y) (make-map-square +tile-map-sky-tile+
							     0 0 "" t)))))

(defun tile-map-tile-source-rectangle (tile-index)
  (make-rectangle :x (->int (* (mod tile-index (tile-map-tiles-per-row)) +tile-map-tile-width+))
		  :y (->int (* (->int (/ tile-index (tile-map-tiles-per-row))) +tile-map-tile-height+))
		  :width +tile-map-tile-width+
		  :height +tile-map-tile-height+))

(defun tile-map-get-cell-by-pixel-x (pixel-x)
  (->int (/ pixel-x +tile-map-tile-width+)))

(defun tile-map-get-cell-by-pixel-y (pixel-y)
  (->int (/ pixel-y +tile-map-tile-height+)))

(defun tile-map-get-cell-by-pixel (pixel-location)
  (make-vector-2 :x (->float (tile-map-get-cell-by-pixel-x (->int (vector-2-x pixel-location))))
		 :y (->float (tile-map-get-cell-by-pixel-y (->int (vector-2-y pixel-location))))))

(defgeneric tile-map-get-cell-center (obj1 &optional obj2))

(defmethod tile-map-get-cell-center ((cell-x integer) &optional cell-y)
  (make-vector-2 :x (->float (+ (* cell-x +tile-map-tile-width+) (/ +tile-map-tile-width+ 2)))
		 :y (->float (+ (* cell-y +tile-map-tile-height+) (/ +tile-map-tile-height+ 2)))))

(defmethod tile-map-get-cell-center ((cell vector-2) &optional obj2)
  (declare (ignore obj2))
  (tile-map-get-cell-center (->int (vector-2-x cell))
			    (->int (vector-2-y cell))))

(defgeneric tile-map-cell-world-rectangle (obj1 &optional obj2))

(defmethod tile-map-cell-world-rectangle ((cell-x integer) &optional cell-y)
  (make-rectangle :x (* cell-x +tile-map-tile-width+)
		  :y (* cell-y +tile-map-tile-height+)
		  :width +tile-map-tile-width+
		  :height +tile-map-tile-height+))

(defmethod tile-map-cell-world-rectangle ((cell vector-2) &optional obj2)
  (declare (ignore obj2))
  (tile-map-cell-world-rectangle (->int (vector-2-x cell))
				 (->int (vector-2-y cell))))

(defgeneric tile-map-cell-screen-rectangle (obj1 &optional obj2))

(defmethod tile-map-cell-screen-rectangle ((cell-x integer) &optional cell-y)
  (camera-world-to-screen (tile-map-cell-world-rectangle cell-x cell-y)))

(defmethod tile-map-cell-screen-rectangle ((cell vector-2) &optional obj2)
  (declare (ignore obj2))
  (tile-map-cell-screen-rectangle (->int (vector-2-x cell))
				  (->int (vector-2-y cell))))

(defgeneric tile-map-cell-is-passable? (obj1 &optional obj2))

(defmethod tile-map-cell-is-passable? ((cell-x integer) &optional cell-y)
  (let ((square (tile-map-get-map-square-at-cell cell-x cell-y)))
    (if square
	(passable square)
	nil)))

(defmethod tile-map-cell-is-passable? ((cell vector-2) &optional obj2)
  (declare (ignore obj2))
  (tile-map-cell-is-passable? (->int (vector-2-x cell))
			      (->int (vector-2-y cell))))

(defun tile-map-cell-is-passable-by-pixel? (pixel-location)
  (tile-map-cell-is-passable? (tile-map-get-cell-by-pixel-x (->int (vector-2-x pixel-location)))
			      (tile-map-get-cell-by-pixel-y (->int (vector-2-y pixel-location)))))

(defgeneric tile-map-cell-code-value (obj1 &optional obj2))

(defmethod tile-map-cell-code-value ((cell-x integer) &optional cell-y)
  (let ((square (tile-map-get-map-square-at-cell cell-x cell-y)))
    (if square
	(code-value square)
	"")))

(defmethod tile-map-cell-code-value ((cell vector-2) &optional obj2)
  (declare (ignore obj2))
  (tile-map-cell-code-value (->int (vector-2-x cell))
			    (->int (vector-2-y cell))))

(defun tile-map-get-map-square-at-cell (tile-x tile-y)
  (if (and (>= tile-x 0) (< tile-x +tile-map-map-width+)
	   (>= tile-y 0) (< tile-y +tile-map-map-height+))
      (aref *tile-map-map-cells* tile-x tile-y)
      nil))

(defun tile-map-set-map-square-at-cell (tile-x tile-y tile)
  (when (and (>= tile-x 0) (< tile-x +tile-map-map-width+)
	     (>= tile-y 0) (< tile-y +tile-map-map-height+))
    (setf (aref *tile-map-map-cells* tile-x tile-y) tile)))

(defun tile-map-set-tile-at-cell (tile-x tile-y layer tile-index)
  (when (and (>= tile-x 0) (< tile-x +tile-map-map-width+)
	     (>= tile-y 0) (< tile-y +tile-map-map-height+))
    (setf (aref (layer-tiles (aref *tile-map-map-cells* tile-x tile-y)) layer) tile-index)))

(defgeneric tile-map-get-map-square-at-pixel (obj1 &optional obj2))

(defmethod tile-map-get-map-square-at-pixel ((pixel-x integer) &optional pixel-y)
  (tile-map-get-map-square-at-cell (tile-map-get-cell-by-pixel-x pixel-x)
				   (tile-map-get-cell-by-pixel-y pixel-y)))

(defmethod tile-map-get-map-square-at-pixel ((pixel-location vector-2) &optional obj2)
  (declare (ignore obj2))
  (tile-map-get-map-square-at-pixel (->int (vector-2-x pixel-location))
				    (->int (vector-2-y pixel-location))))

(defun tile-map-draw ()
  (let ((start-x (tile-map-get-cell-by-pixel-x (->int (vector-2-x (camera-position)))))
	(end-x (tile-map-get-cell-by-pixel-x (->int (+ (vector-2-x (camera-position))
						       (camera-view-port-width)))))
	(start-y (tile-map-get-cell-by-pixel-y (->int (vector-2-y (camera-position)))))
	(end-y (tile-map-get-cell-by-pixel-y (->int (+ (vector-2-y (camera-position))
						       (camera-view-port-height))))))
    (do ((x start-x (incf x)))
	((> x end-x))
      (do ((y start-y (incf y)))
	  ((> y end-y))
	(do ((z 0 (incf z)))
	    ((>= z +tile-map-map-layers+))
	  (when (and (>= x 0) (< x +tile-map-map-width+)
		     (>= y 0) (< y +tile-map-map-height+))
	    (draw-tex-rect *tile-map-tile-sheet*
			   (make-vector-2
			    :x (->float (rectangle-x (tile-map-cell-screen-rectangle x y)))
			    :y (->float (rectangle-y (tile-map-cell-screen-rectangle x y))))
			   (tile-map-tile-source-rectangle (aref (layer-tiles (aref *tile-map-map-cells* x y))
								 z))
			   (make-white-color)
			   0.0
			   (make-vector-2)
			   0.0
			   'none
			   (- 1.0 (* z 0.1)))))

	(when *tile-map-editor-mode*
	  (draw-edit-mode-items x y))))))

(defun draw-edit-mode-items (x y)
  (when (or (< x 0) (>= x +tile-map-map-width+)
	    (< y 0) (>= y +tile-map-map-height+))
    (return-from draw-edit-mode-items))

  (unless (tile-map-cell-is-passable? x y)
    (draw-tex-rect *tile-map-tile-sheet*
		   (make-vector-2
		    :x (->float (rectangle-x (tile-map-cell-screen-rectangle x y)))
		    :y (->float (rectangle-y (tile-map-cell-screen-rectangle x y))))
		   (tile-map-tile-source-rectangle 1)
		   (make-color :r 255 :a 80)
		   0.0
		   (make-vector-2)
		   nil
		   :none
		   0.0))

  (unless (string= (code-value (aref *tile-map-map-cells* x y)) "")
    (let ((screen-rect (tile-map-cell-screen-rectangle x y)))
      (draw-string (code-value (aref *tile-map-map-cells* x y))
		   (make-vector-2 :x (->float (rectangle-x screen-rect))
				  :y (->float (rectangle-y screen-rect)))
		   (make-white-color)
		   :centered nil))))

(defun tile-map-save-map (file-stream)
  (with-standard-io-syntax
    (print *tile-map-map-cells* file-stream)))

(defun tile-map-load-map (file-stream)
  (handler-case
      (with-standard-io-syntax
	(setf *tile-map-map-cells* (read file-stream)))
    (error () (tile-map-clear-map))))

(defun tile-map-clear-map ()
  (do ((x 0 (incf x)))
      ((>= x +tile-map-map-width+))
    (do ((y 0 (incf y)))
	((>= y +tile-map-map-height+))
      (do ((z 0 (incf z)))
	  ((>= z +tile-map-map-layers+))
	(setf (aref *tile-map-map-cells* x y) (make-map-square 2 0 0 "" t))))))
