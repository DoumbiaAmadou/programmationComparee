#! /usr/local/bin/clisp

;Main
(setq SourceFile (open "words.txt"))   ;open file to read
(setq FileLine (read-line SourceFile)) ;read file content
(close SourceFile) 
(loop for line in FileLine
	do (if (comparingString line (car *args*)) (print line)))

;Comparing two strings after sorted
(defun comparingString (w1 w2) (string-equal (sortWord w1) (sortWord w2)))

;Sorting a word
(defun sortWord(s) (sort (copy-seq s) #'char))