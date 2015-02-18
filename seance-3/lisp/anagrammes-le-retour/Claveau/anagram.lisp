#lang racket
(require racket/list)
(define fichier (open-input-file "../words"))

(define (listToString l) 
    (if (null? l)
	""
        (string-append (string-append (list-ref l 0) " ") (listToString (list-tail l 1)))
    )
)

(define (hashKey s) (list->string (sort (string->list s) char<?)))

(define (add file) 
  (let ((line (read-line file)))
    (if (eof-object? line)
      (hash)
      (hash-update (add file) (hashKey line) (lambda (prev) (append (list line) prev)) (list))
    )
  )
)
(define dico (add fichier))

(close-input-port fichier)

(for ([arg (current-command-line-arguments)]) (
    displayln (string-append arg (string-append ": " (listToString (hash-ref dico (list->string (sort (string->list arg) char<?)) (list " ")))))
))

(newline)

