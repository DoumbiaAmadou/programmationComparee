(defun pow (x n)
  (cond	; if n = O, return 1
	((= n 0) 1)
	; if n < 0, we calculate in this formulair
	((< n 0) (* (/ 1 x) (pow x (+ n 1))))
	; case normal
	(t (* x (pow x (- n 1))))))

(format t "Test function (pow x n) ~%")
(format t "Test 1: 2^0 = ~D.~%" (pow 2 0))
(format t "Test 2: 2^5 =  ~D.~%" (pow 2 5))
(format t "Test 3: 2^-5 =  ~D.~%" (pow 2 -5))
