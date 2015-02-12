#! /usr/local/bin/clisp
;(setq wds (make-hash-table))
; (defun put-string-to-dict(s) (setf (gethash (STRINGSORTER s)))) 

;Sort string in lexicographical order
(defun string-sorter(s) (sort (copy-seq s) #'char-lessp))

;Return true if s1 and s2 are anagrams
(defun check-anagram(s1 s2) (string-equal (string-sorter s1) (string-sorter s2))) 

;Inefficace mais marche bien quand même
(with-open-file (stream "../words")
    (loop for line = (read-line stream nil 'foo)
          until (eq line 'foo)
          do (if (check-anagram line (car *args*)) (print line) (continue))))
