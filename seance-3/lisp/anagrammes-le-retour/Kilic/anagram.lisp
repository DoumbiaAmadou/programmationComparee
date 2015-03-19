;; #!/usr/bin/clisp

(defun sorted (w)
  "returns the sorted word [] in a lexicographical order"
  (sort (copy-seq w) 'char<))

(defun is_anagram (w1 w2)
  "check if the words [w1] and [w2] are anagram."
  (if (string-not-equal w1 w2)
      (string-equal (sorted w1) (sorted w2))))

(defun list_from_file (filename)
  "make a string list with the lines in the file [filename]"
  (defparameter *word_list* (list nil))
  (let ((in (open filename)))
    (when in
      (loop for line = (read-line in nil)
	    while line do ( nconc *word_list* (list line) )
	    )
      )
    (close in)
    )
   *word_list*)

(defun anagramsNaive (l words)
  "print all anagrams in the list [words] for every words w in [l]"
  (loop for w1 in l
	do (
	    format t "~% ~A : " w1)
	(loop for w2 in words
	      do (if (is_anagram w1 w2)
		     (format t "~A " w2))
		   )
	)
  )


(anagrams *args* (list_from_file "../words"))




