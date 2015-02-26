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

;sort string
(defun sortWord (word)
   "return the string sorted"
   (sort (copy-seq word) #'char<))

;compaire two words
(defun compaireWords (w1 w2)
  "return true if two words are equivalent"
  (string-equal (sortWord w1) (sortWord w2)))

;make hash-table
(defun makeHashTable (words)
  "return a hash-table"
  (setq hashWords (make-hash-table))
  (loop for word on words do 
    (let ((key (sortWord word)))
      ((= (gethash key hashWords) nil)

      (setf (gethash key hashWords) 
        ((defparameter *anagrammes* ())
          (adjoin word *anagrammes*) *anagrammes*)) 
      (setf (gethash key hashWords) 
        (adjoin word *anagrammes*) *anagrammes*))))
  hashWords)

;main function
;this function doesn't use hash table, so it's very naive :(
(defun anagrammesNaive (listArg words)
  "search all anagrammes for each word in listArg"
  (loop for word in listArg do 
    (format t "~% ~A: " word)
    (loop for word2 in words do 
      (if (compaireWords word word2) (format t "~A " word2)))))

;(anagrammes *args* (getWords "../words"))
