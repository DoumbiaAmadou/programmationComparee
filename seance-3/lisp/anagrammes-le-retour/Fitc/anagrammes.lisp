#! /usr/local/bin/clisp

;Read programm arg
(format t "~&~s~&" *args*)

; (let ((the-string (copy-seq "this is the string")))
;   (sort the-string #'char-lessp))

; (with-open-file (stream "../words")
;     (loop for line = (read-line stream nil 'foo)
;           until (eq line 'foo)
;           do (print line)))
