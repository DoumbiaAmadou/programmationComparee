;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(defun reorder (x) "Sort chars in a string."
  (concat (sort (append x ()) '<) () ) )

;; test (reorder "zbcad") "abcdz"

(defun isanagram (x y) "Test if two word are anagrams"
  (string= (reorder (downcase x)) (reorder (downcase y) ) ) )

;; test (isanagram "couille" "luciole") t

;; Build a dictionnary from a file
(defun anagram (f) "Load file and return word list"
  (split-string
   (with-temp-buffer
     (insert-file-contents f)
     (buffer-string) )
   "\n" t)
)

;; load dictionnary
(setq dico (anagram "/home/ju/workspace/up7/pcomp15/seance-1/anagrams/words") )

;; Naive function to find anagram. Test with each word in dictionnary
(defun findanagram (dico w) ""
  (let ((result ()))
    (while dico
      (when (isanagram (car dico) w) (push (car dico) result) )
      (setq dico (cdr dico) ) )
    result ) )

;; Does not follow spec's printing format.
( ( lambda (&rest ws) "" (loop for w in ws do
                               (princ w)
                               (princ (findanagram w) ) ) argv ) )
