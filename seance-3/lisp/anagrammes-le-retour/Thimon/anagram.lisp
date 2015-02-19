(defun anagram(word)
 (let ((in (open "words")))
  (when in
    (loop for line = (read-line in nil)
         while line do (if (is-anagram word line) (print line))
    )(close in)))
)

(defun is-anagram(word1 word2)
	(string-equal (sort (copy-seq word1) #'char-lessp) (sort (copy-seq word2) #'char-lessp))
)

(defun main(words)
	(loop for word in words do (print word)(anagram word))
)
