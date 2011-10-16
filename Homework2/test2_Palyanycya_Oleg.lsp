(defun make-adder (x)
	(lambda (y) (+ y x)))

(defun my-complement (pred)
	(lambda (a) (not (funcall pred a))))

(defun compose (&rest funs)
	(if (eq (cdr funs) nil)
		(lambda (&rest args) (funcall (car funs) args))
		(lambda (&rest args) (funcall (car funs) (apply (apply 'compose (cdr funs)) args)))))
