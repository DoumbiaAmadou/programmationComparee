;Auteur: Nguyen Nga

; retrieve data in file and save in a list
(defun getWords (filename)
  "return the list of world in file words"
  (defparameter *listWords* (list nil))
   (let ((in (open filename)))
     (when in
       (loop for line = (read-line in nil)
	     while line do ( nconc *listWords* (list line) )
	)
      )
     (close in)
     )
   *listWords*)
(getWords "wordsTest")
