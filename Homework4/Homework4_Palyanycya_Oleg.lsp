;************************Calc-beginning************************
(defstruct rational-number
	numerator
	denominator)

(defun create-hash-table (lst &optional (table (make-hash-table)))
	(mapcar #'(lambda (x) (setf (gethash (car x) table) (cadr x))) lst)
	table)

(defun divide (&rest args)
	(if args
		(if (cdr args)
			(labels ((div (args-lst &optional (numerator (if (rational-number-p (car args)) (rational-number-numerator (car args)) (car args)))
											 (denominator (if (rational-number-p (car args)) (rational-number-denominator (car args)) 1)))
					(if args-lst
						(if (rational-number-p (car args-lst))
							(let ((numerator-arg (rational-number-numerator (car args-lst))) 
								  (denominator-arg (rational-number-denominator (car args-lst))))
								(div (cdr args-lst) (* numerator denominator-arg) (* denominator numerator-arg)))
							(div (cdr args-lst) numerator (* (car args-lst) denominator)))
						(if (= denominator 1)
							numerator
							(if (zerop denominator)
								(error "division by zero")
								(make-rational-number :numerator numerator :denominator denominator)))))) (div (cdr args)))
			(if (rational-number-p (car args))
				(make-rational-number :numerator (rational-number-denominator (car args)) :denominator (rational-number-numerator (car args)))
				(make-rational-number :numerator 1 :denominator (car args))))
		(error "invalid number of arguments")))

(defun add (&rest args)
	(if args
		(labels ((pl (args-lst &optional (numerator (if (rational-number-p (car args)) (rational-number-numerator (car args)) (car args)))
										 (denominator (if (rational-number-p (car args)) (rational-number-denominator (car args)) 1)))
				(if args-lst
					(if (rational-number-p (car args-lst))
						(let ((numerator-arg (rational-number-numerator (car args-lst))) 
							  (denominator-arg (rational-number-denominator (car args-lst))))
							(pl (cdr args-lst) (+ (* numerator denominator-arg) (* denominator numerator-arg)) (* denominator denominator-arg)))
						(pl (cdr args-lst) (+ numerator (* denominator (car args-lst))) denominator))
					(if (= denominator 1)
						numerator
						(make-rational-number :numerator numerator :denominator denominator))))) (pl (cdr args))) 
	0))

(defun subs (&rest args)
	(if args
		(if (cdr args)
			(labels ((minus (args-lst &optional (numerator (if (rational-number-p (car args)) (rational-number-numerator (car args)) (car args)))
												 (denominator (if (rational-number-p (car args)) (rational-number-denominator (car args)) 1)))
						(if args-lst
							(if (rational-number-p (car args-lst))
								(let ((numerator-arg (rational-number-numerator (car args-lst))) 
									  (denominator-arg (rational-number-denominator (car args-lst))))
									(minus (cdr args-lst) (- (* numerator denominator-arg) (* denominator numerator-arg)) (* denominator denominator-arg))
								)
								(minus (cdr args-lst) (- numerator (* denominator (car args-lst))) denominator)
							)
							(if (= denominator 1)
								numerator
								(make-rational-number :numerator numerator :denominator denominator)
							)
						))) (minus (cdr args)))
			(if (rational-number-p (car args))
				(make-rational-number :numerator (-(rational-number-numerator (car args))) :denominator (rational-number-denominator (car args)))
				(-(car args))))
		(error "Ivalid number of arguments.")))

(defun mult (&rest args)
	(if args
		(labels ((mul (args-lst &optional (numerator (if (rational-number-p (car args)) (rational-number-numerator (car args)) (car args)))
										 (denominator (if (rational-number-p (car args)) (rational-number-denominator (car args)) 1)))
				(if args-lst
					(if (rational-number-p (car args-lst))
						(let ((numerator-arg (rational-number-numerator (car args-lst))) 
							  (denominator-arg (rational-number-denominator (car args-lst))))
							(mul (cdr args-lst) (* numerator numerator-arg) (* denominator denominator-arg)))
						(mul (cdr args-lst) (* numerator (car args-lst)) denominator))
					(if (= denominator 1)
						numerator
						(make-rational-number :numerator numerator :denominator denominator))))) (mul (cdr args)))
		1))

(defun search-value (symb const-table-lst)
	(when const-table-lst
		(let ((val (gethash symb (car const-table-lst))))
			(if val
				val
				(search-value symb (cdr const-table-lst))))))

(defun calc (lst &optional (const-table-lst nil))
	(if lst
		(cond 
			((eq (car lst) '+) (apply 'add (calc-operands (cdr lst) nil const-table-lst)))
			((eq (car lst) '-) (apply 'subs (calc-operands (cdr lst) nil const-table-lst)))
			((eq (car lst) '*) (apply 'mult (calc-operands (cdr lst) nil const-table-lst)))
			((eq (car lst) '/) (apply 'divide (calc-operands (cdr lst) nil const-table-lst)))
			((eq (car lst) 'let) (calc (car(cdr(cdr lst))) (cons (create-hash-table (car (cdr lst))) const-table-lst))))))


(defun calc-operands (lst acc const-table-lst)
	(cond
		((null lst) acc)
		((or (numberp lst) (rational-number-p lst)) (cons lst acc))
		((symbolp lst) (cons (search-value lst const-table-lst) acc))
		(t (calc-operands (if (listp (car lst)) 
							(calc (car lst) const-table-lst)
							(car lst)) (calc-operands (cdr lst) acc const-table-lst) const-table-lst))))
;************Calc-end****************

(defun create-hash-table-for-expansion (lst &optional const-table-lst params-lst (table (make-hash-table)))
	(if lst 
		(progn
			(multiple-value-bind (params all) (trav (cadar lst) const-table-lst)
				(if (listp (cadar lst))
					(setf (gethash (caar lst) table) all)
					(setf (gethash (caar lst) table) (cadar lst))
				)
				(create-hash-table-for-expansion (cdr lst) const-table-lst (append params-lst params) table)))
		(values table (remove-duplicates params-lst))))

;Returns list of constants and list that need to be evaluated.
(defun trav (lst &optional const-table-lst params-lst all-lst)
	(cond
		((null lst) (values params-lst all-lst))
		((or (numberp lst) (rational-number-p lst) (eq lst '+) (eq lst '-) (eq lst '*) (eq lst '/)) 
			(values params-lst (cons lst all-lst)))
		((symbolp lst) 
			(let ((val (search-value lst const-table-lst)))
				;(format t "Value: ~A~%" val)
				(if val
					(values params-lst (cons val all-lst))
					(values (cons lst params-lst) (cons lst all-lst)))))
		(t 
			(if (eq (car lst) 'let)
				(multiple-value-bind (table tab-params) (create-hash-table-for-expansion (cadr lst) const-table-lst)
					(multiple-value-bind (lst-params lst-all) (trav (caddr lst) (cons table const-table-lst))
						(values (append tab-params lst-params) lst-all)))
				(multiple-value-bind (params all) (trav (cdr lst) const-table-lst params-lst all-lst) 
					(if (listp (car lst))
						(multiple-value-bind (lst-params lst-all) (trav (car lst) const-table-lst)
							(values (append lst-params params) (cons lst-all all)))				
						(trav (car lst) const-table-lst params all)))))))

(defun calc-primitive-forms (lst)
	(if lst
		(cond 
			((eq (car lst) '+) 
				(multiple-value-bind (numbers consts) (calc-number-operands (cdr lst) nil nil)
					(if consts
						(cons '+ (cons (apply 'add numbers) consts))
						(apply 'add numbers))))
			((eq (car lst) '-)
				(multiple-value-bind (numbers consts) (calc-number-operands (cddr lst) nil nil)
					(let ((first-arg (if (listp (cadr lst)) (calc-primitive-forms (cadr lst)) (cadr lst))))
						(if consts
							(if (numberp first-arg)
								(cons '- (cons (apply 'subs (cons first-arg numbers)) consts))
								(cons '- (cons first-arg (cons (apply 'add numbers) consts))))
							(if (numberp first-arg)
								(apply 'subs (cons first-arg numbers))
								(cons '- (cons first-arg (apply 'add numbers))))))))
			((eq (car lst) '*)
				(multiple-value-bind (numbers consts) (calc-number-operands (cdr lst) nil nil)
					(if consts
						(cons '* (cons (apply 'mult numbers) consts))
						(apply 'mult numbers))))
			((eq (car lst) '/) 
				(multiple-value-bind (numbers consts) (calc-number-operands (cddr lst) nil nil)
					(let ((first-arg (if (listp (cadr lst)) (calc-primitive-forms (cadr lst)) (cadr lst))))
						(if consts
							(if (numberp first-arg)
								(cons '/ (cons (apply 'divide (cons first-arg numbers)) consts))
								(cons '/ (cons first-arg (cons (apply 'mult numbers) consts))))
							(if (numberp first-arg)
								(apply 'divide (cons first-arg numbers))
								(cons '/ (cons first-arg (apply 'mult numbers)))))))))))

(defun calc-number-operands (lst acc const-lst)
	(cond
		((null lst) (values acc const-lst))
		((or (numberp lst) (rational-number-p lst)) (values (cons lst acc) const-lst))
		((symbolp lst) (values acc (cons lst const-lst)))
		(t
			(multiple-value-bind (numbers consts) (calc-number-operands (cdr lst) acc const-lst)
				(if (listp (car lst))
					(let ((expr (calc-primitive-forms (car lst))))
						(if (listp expr)
							(values numbers (cons expr consts))
							(values (cons expr numbers) consts)))
					(calc-number-operands (car lst) numbers consts))))))

(defun simplify-operation (lst symb)
	(when lst
		(append (if (listp (car lst))
			(cond 
				((eq (caar lst) symb) (simplify-operation (cdar lst) symb))
				(t (list (simplify (car lst)))))
			(list (car lst))) (simplify-operation (cdr lst) symb))))

(defun simplify-operands (lst)
	(when lst
		(cons
			(if (listp (car lst))
				(simplify (car lst))
				(car lst))
			(simplify-operands (cdr lst)))))

(defun simplify (lst)
	(cond
		((eq (car lst) '+) 
			(cons '+ (simplify-operation (cdr lst) '+)))
		((eq (car lst) '*)
			(cons '* (simplify-operation (cdr lst) '*)))
		((eq (car lst) '-)
			(cons '- (cons (cadr lst) (simplify-operation (cddr lst) '+))))
		((eq (car lst) '/)
			(cons '/ (cons (cadr lst) (simplify-operation (cddr lst) '*))))
		(t (simplify-operands lst))))


;This macro makes lambda as was shown in example, but it (lambda) doesn't work.
(defmacro make-calc (expr)
	(multiple-value-bind (params all) (trav expr)
		`(lambda ,params (calc ',(calc-primitive-forms (simplify all))))))