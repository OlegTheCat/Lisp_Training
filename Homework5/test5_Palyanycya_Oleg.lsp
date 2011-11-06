(defun build-our-let* (lst body)
	(if (cdr lst)
		`(our-let ,(list (car lst)) ,(build-our-let* (cdr lst) body))
		`(our-let ,(list (car lst)) ,@body)))

(defmacro our-let (arg-lst &body body)
	`((lambda ,(mapcar #'car arg-lst) ,@body) ,@(mapcar #'cadr arg-lst)))		

(defmacro our-let* (lst &body body)
	(build-our-let* lst body))

(defun build-gensyms (lst)
	(if (cdr lst)
		(cons `(,(car lst) ,(gensym)) (build-gensyms (cdr lst)))
		(list (list (car lst) (gensym)))))
		
(defmacro with-gensyms (symbs-lst &body body)
	`(let ,(build-gensyms symbs-lst) ,@body))

(defmacro defanaph (name fn)
	`(defmacro ,name (&rest args) (build-body ,fn args)))

(defun build-body (fun lst &optional symbs)
	(if lst
		(let ((symb (gensym)))
			`(let* ((,symb ,(car lst))(it ,symb)) ,(build-body fun (cdr lst) (cons symb symbs))))
		`(,fun ,@(reverse symbs))))