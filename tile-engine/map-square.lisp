(in-package #:tile-engine)

(defmacro make-map-square (background interactive foreground code passable)
  `(make-array 3 :initial-contents (list (make-array 3 :element-type 'integer
						     :initial-contents (list ,background
									     ,interactive
									     ,foreground))
					 ,code
					 ,passable)))

(eval-when (:compile-toplevel :load-toplevel)
  (defun map-square-value (map-square value)
    `(aref ,map-square ,value)))

(defmacro layer-tiles (map-square)
  (map-square-value map-square 0))

(defmacro code-value (map-square)
  (map-square-value map-square 1))

(defmacro passable (map-square)
  (map-square-value map-square 2))

(defun toggle-passable (map-square)
  (setf (passable map-square) (not (passable map-square))))
