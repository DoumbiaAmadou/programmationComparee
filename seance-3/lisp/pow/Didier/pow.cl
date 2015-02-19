(defun powaux (n x) (
     	if (= n 0) 
       	   '1
  	   `(* ,x (,(powaux (- n 1)) x)) 
	)
)

(defun pow (n) 
       (let ((x (gensym))) `(lambda (,x) ,(powaux n x))))