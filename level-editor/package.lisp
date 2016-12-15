(in-package #:cl-user)

(defpackage #:level-editor
  (:use #:cl
	#:game-engine
	#:tile-engine
	#:cffi
	#:gdk
	#:gobject
	#:glop)
  (:import-from #:alexandria #:switch)
  (:export #:main))
