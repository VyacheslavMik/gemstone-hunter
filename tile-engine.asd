(in-package #:asdf)

(defsystem #:tile-engine
  :version "0.1"
  :description "Tile engine for Gemstone Hunter"
  :author "Hedin"
  :depends-on (#:game-engine)
  :components
  ((:module "tile-engine"
	    :components
	    ((:file "package")
	     (:file "camera" :depends-on ("package"))
	     (:file "map-square" :depends-on ("package"))
	     (:file "tile-map" :depends-on ("package"
					    "map-square"
					    "camera"))))))
