(defmacro aif (expr body1 &optional body2) 
	`(let ((it ,expr))
		(if it
			,body1
			,body2)))

(defmacro awhen (expr &body body)
	`(let ((it ,expr))
		(when it
			,@body)))

(defun traverse (fun tree fget-node fget-children fmake-node)
	(unless (null tree)
		(funcall fmake-node (funcall fun (funcall fget-node tree)) 
			(mapcar #'(lambda (child) (when child
										(traverse fun child fget-node fget-children fmake-node))) (funcall fget-children tree)))))			

(defmacro cut (&rest rest)
	(let ((params nil))
		(let ((all (traverse
						#'(lambda (x) (if (eq x '_) (let ((symb (gensym))) (setq params (cons symb params)) symb) x))
						rest
						#'(lambda (node) (if (atom node) node (car node)))
						#'(lambda (node) (if (atom node) nil (cdr node)))
						#'(lambda (val &optional list-of-nodes) (if val (if list-of-nodes (cons val list-of-nodes) val))))))
			`(lambda ,(reverse params) ,all))))