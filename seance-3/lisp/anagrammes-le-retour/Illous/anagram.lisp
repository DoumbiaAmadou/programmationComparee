#lang racket
(require racket/list)

(define file (open-input-file "../words"))


(define (string_of_list list)
  (cond 
   [(empty? list) ""]
   [else (string-append (string-append (first list) " ")
			(string_of_list (rest list)))]
			)
  )


