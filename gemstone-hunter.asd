(in-package #:asdf)

(defsystem #:gemstone-hunter
  :version "0.1"
  :description "Game Gemstone Hunter"
  :author "Hedin"
  :depends-on (#:game-engine
	       #:tile-engine)
  :components
  ((:module "game"
	    :components
	    ((:file "package")
	     (:file "utils" :depends-on ("package"))
	     (:file "generic-functions" :depends-on ("package"))
	     (:file "animation-strip" :depends-on ("package"
						   "generic-functions"))
	     (:file "game-object" :depends-on ("package"
					       "generic-functions"
					       "animation-strip"))
	     (:file "gemstone" :depends-on ("package"
					    "game-object"))
	     (:file "enemy" :depends-on ("package"
					 "game-object"))
	     (:file "player" :depends-on ("package"
					  "game-object"))
	     (:file "level-manager" :depends-on ("package"
						 "gemstone"
						 "enemy"
						 "player"))
	     (:file "game" :depends-on ("package"
					"utils"
					"level-manager"))))))
