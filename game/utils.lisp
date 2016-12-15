(in-package #:gemstone-hunter)

(defparameter *images-directories* '("projects/gemstone-hunter/game/images/"
				     "images/"))

(defun get-suitable-directory (directories)
  (or (find-if #'probe-file directories)
      (error "Suitable directory not found")))

(defun get-file-path (directories filename)
  (concatenate 'string (get-suitable-directory directories) filename))

(defun get-texture-path (filename)
  (get-file-path *images-directories* filename))

(defun split-string (string char)
  (loop
     for i = 0 then (1+ j)
     as j = (position char string :start i)
     collect (subseq string i j)
     while j))
