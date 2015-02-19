(defun pow (n) 
		( lambda (x)
			(if (= n 1) 
				'x 
				`(* x ,(pow (- n 1)))
				;;`(pow (- n 1) ,(* x x))
			)
		)
)