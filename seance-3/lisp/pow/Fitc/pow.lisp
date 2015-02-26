#! /usr/local/bin/clisp

(defun pow(n) (if (= n 1) (function (lambda (x) x))  (function (lambda (x) (* x (funcall (pow (- n 1)) x))))))