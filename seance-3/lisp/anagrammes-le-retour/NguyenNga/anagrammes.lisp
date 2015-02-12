(defun getWords (filename)
  "return the list of world in file words"
   (let ((in (open filename)))
     (when in
       (loop for line = (read-line in nil)
	     while line do (format t "~a~%" line)
	)
      )
     (close in)
     )
)
