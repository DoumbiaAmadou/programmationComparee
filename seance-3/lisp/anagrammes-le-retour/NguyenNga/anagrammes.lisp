;Auteur: Nguyen Nga

; retrieve data in file and save in a list
(defun getWords (filename)
  "return the list of world in file words"
  (defparameter *listWords* (list nil))
   (let ((in (open filename)))
     (when in
       (loop for line = (read-line in nil)
	     while line do ( nconc *listWords* (list line))))
     (close in))
   *listWords*)
;test getWords
(getWords "wordsTest")

;sort string
(defun sortWord (word)
   "return the string sorted"
   (sort (copy-seq word) #'char>))
;test sortWord
(sortWord "ohno")


;compaire two words
(defun compaireWord (w1 w2)
  "return true if two words are equivalent"
  (string-equal (sortWord w1) (sortWord w2)))
;test compaireWords
(compaireWords "ohno" "nooh")

;make hash-table
(defun makeHashTable (words)
  "return a hash-table"
  (setq hashWords (make-hash-table))
  (loop for word on words do 
    ((= (gethash (sortWord word) hashWords) ))
    (setf )))

;main function
(defun anagrammes ())
