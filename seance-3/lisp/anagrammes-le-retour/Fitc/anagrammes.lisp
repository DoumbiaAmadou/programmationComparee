#! /usr/local/bin/clisp

;Read programm arg
; (format t "~&~s~&" *args*)

; (let ((the-string (copy-seq "this is the string")))
;   (sort the-string #'char-lessp))


(setq wds (make-hash-table))
(defun string-sorter(s) (sort (copy-seq s) #'char-lessp))
; (defun put-string-to-dict(s) (setf (gethash (STRINGSORTER s)))) 

(defun check-anagram(s1 s2) (string-equal (string-sorter s1) (string-sorter s2))) 
(with-open-file (stream "../words")
    (loop for line = (read-line stream nil 'foo)
          until (eq line 'foo)
          do (if (check-anagram line "aborder") (print line) (continue))))
