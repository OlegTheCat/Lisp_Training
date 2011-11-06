(defstruct rational-number
	numerator
	denominator
)

(defun divide (&rest args)
	(if args
		(if (cdr args)
			(labels ((div (args-lst &optional (numerator (if (rational-number-p (car args)) (rational-number-numerator (car args)) (car args)))
											 (denominator (if (rational-number-p (car args)) (rational-number-denominator (car args)) 1)))
					(if args-lst
						(if (rational-number-p (car args-lst))
							(let ((numerator-arg (rational-number-numerator (car args-lst))) 
								  (denominator-arg (rational-number-denominator (car args-lst))))
								(div (cdr args-lst) (* numerator denominator-arg) (* denominator numerator-arg))
							)
							(div (cdr args-lst) numerator (* (car args-lst) denominator))
						)
						(if (= denominator 1)
							numerator
							(if (zerop denominator)
								(error "division by zero")
								(make-rational-number :numerator numerator :denominator denominator)
							)
						)
					))) (div (cdr args)))
			(if (rational-number-p (car args))
				(make-rational-number :numerator (rational-number-denominator (car args)) :denominator (rational-number-numerator (car args)))
				(make-rational-number :numerator 1 :denominator (car args))
			)
		)
		(error "invalid number of arguments")
	)
)

(defun add (&rest args)
	(if args
		(labels ((pl (args-lst &optional (numerator (if (rational-number-p (car args)) (rational-number-numerator (car args)) (car args)))
										 (denominator (if (rational-number-p (car args)) (rational-number-denominator (car args)) 1)))
				(if args-lst
					(if (rational-number-p (car args-lst))
						(let ((numerator-arg (rational-number-numerator (car args-lst))) 
							  (denominator-arg (rational-number-denominator (car args-lst))))
							(pl (cdr args-lst) (+ (* numerator denominator-arg) (* denominator numerator-arg)) (* denominator denominator-arg))
						)
						(pl (cdr args-lst) (+ numerator (* denominator (car args-lst))) denominator)
					)
					(if (= denominator 1)
						numerator
						(make-rational-number :numerator numerator :denominator denominator)
					)
				))) (pl (cdr args)))
		0)
)

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
				(-(car args))
			)
		)
		(error "Ivalid number of arguments.")
	)
)

(defun mult (&rest args)
	(if args
		(labels ((mul (args-lst &optional (numerator (if (rational-number-p (car args)) (rational-number-numerator (car args)) (car args)))
										 (denominator (if (rational-number-p (car args)) (rational-number-denominator (car args)) 1)))
				(if args-lst
					(if (rational-number-p (car args-lst))
						(let ((numerator-arg (rational-number-numerator (car args-lst))) 
							  (denominator-arg (rational-number-denominator (car args-lst))))
							(mul (cdr args-lst) (* numerator numerator-arg) (* denominator denominator-arg))
						)
						(mul (cdr args-lst) (* numerator (car args-lst)) denominator)
					)
					(if (= denominator 1)
						numerator
						(make-rational-number :numerator numerator :denominator denominator)
					)
				))) (mul (cdr args)))
		1
	)
)

(defun calc (lst &optional (const-table-lst nil))
	(if lst
		(cond 
			((eq (car lst) '+) (apply 'add (calc-operands (cdr lst) nil const-table-lst)))
			((eq (car lst) '-) (apply 'subs (calc-operands (cdr lst) nil const-table-lst)))
			((eq (car lst) '*) (apply 'mult (calc-operands (cdr lst) nil const-table-lst)))
			((eq (car lst) '/) (apply 'divide (calc-operands (cdr lst) nil const-table-lst)))
			((eq (car lst) 'let) (calc (car(cdr(cdr lst))) (cons (create-hash-table (car (cdr lst))) const-table-lst)))
		)
	)
)

(defun calc-operands (lst acc const-table-lst)
	(cond
		((null lst) acc)
		((or (numberp lst) (rational-number-p lst)) (cons lst acc))
		((symbolp lst) (cons (search-value lst const-table-lst) acc))
		(t (calc-operands (if (listp (car lst)) 
							(calc (car lst) const-table-lst)
							(car lst)) (calc-operands (cdr lst) acc const-table-lst) const-table-lst))
	)
)

(defun search-value (symb const-table-lst)
	(when const-table-lst
		(or (gethash symb (car const-table-lst)) (search-value symb (cdr const-table-lst)))
	)
)

(defun create-hash-table (lst &optional (table (make-hash-table)))
	(mapcar #'(lambda (x) (setf (gethash (car x) table) (cadr x))) lst)
	table
)


;(print (calc '(+ 1 2 (+ 3 2 (* 3 3)))))
;(print (calc '(let ((a 10) (b 20)) (+ a b (* 3 a)))))
;(print (calc '(let ((a 1)(b 2)(c 3)) (+ a 1 (/ (let ((r 2) (b 3)) (- r b)) c)))))
