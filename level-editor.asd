(in-package #:asdf)

(defsystem #:level-editor
  :version "0.1"
  :description "Level editor for Gemstone Hunter"
  :author "Hedin"
  :depends-on (#:game-engine
	       #:tile-engine
	       #:cl-cffi-gtk
	       #:cl-opengl
	       #:cffi
	       #:glop
	       #:alexandria)
  :components
  ((:module "level-editor"
	    :components
	    ((:file "gtk.header-bar")
	     (:file "gtk.list-box")
	     (:file "package")
	     (:file "common-params" :depends-on ("package"))
	     (:file "level-editor-game" :depends-on ("package"
						     "common-params"))
	     (:file "map-editor" :depends-on ("package"
					      "gtk.header-bar"
					      "gtk.list-box"
					      "common-params"
					      "level-editor-game"))))))
