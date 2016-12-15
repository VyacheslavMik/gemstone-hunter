(in-package #:level-editor)

;; common
(defparameter *images-directories* '("projects/gemstone-hunter/level-editor/images/"))
(defparameter *default-mouse-state* (list -1.0 -1.0 :button-release :button-release))

;; map-editor
(defparameter *mouse-state* nil)
(defparameter *event-box* nil)
(defparameter *h-adjustment* nil)
(defparameter *v-adjustment* nil)
(defparameter *maps-directories* '("projects/gemstone-hunter/level-editor/maps/"))

;; game
(defparameter *editing-code* nil)
(defparameter *current-code-value* nil)
(defparameter *draw-layer* 0)
(defparameter *last-mouse-state* nil)
(defparameter *draw-tile* 0)
(defparameter *hover-code-value* "")
(defparameter *changed* nil)
