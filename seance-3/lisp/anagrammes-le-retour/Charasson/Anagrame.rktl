#lang racket
(define in (open-input-file "../words"))
(for ([arg (current-command-line-arguments)]) (displayln arg))

;;exemple pour lire un fichier ligne a ligne
(define (read-next-line-iter file)
	   (let ((line (read-line file)))
	     (unless (eof-object? line)
	       (display line)
	       (newline)
	       (read-next-line-iter file))))
;;(call-with-input-file "../words" read-next-line-iter)

;;faire une hashtable
(define table (hash 'a 1 'b 2))

(define get
