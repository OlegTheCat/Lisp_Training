(defun create-hash-table (lst &optional (table (make-hash-table)))
	(mapcar #'(lambda (x) (setf (gethash (car x) table) (cadr x))) lst)
	table)

(defun lexer (path)
	(with-open-file (str path)
		(read-chars str (create-hash-table (create-operations-list)))))

(defun create-operations-list ()
	(list
		'(#\* mul)
		'(#\+ add)
		'(#\- sub)
		'(#\/ div)
		'(#\( lpar)
		'(#\) rpar)))

(defun calculate-number (lst &optional (power 1) (number 0))
	(if lst
		(calculate-number (cdr lst) (* 10 power) (+ number (* power (car lst))))
		number))

(defun read-numbers (stream lst char)
	;Reading char without moving a pointer: if it's an eof or not number, 
	;then the sequence of number reached its end and.
	(let ((pchar (peek-char nil stream nil 'eof)))
		(if (and (not(eq pchar 'eof)) (>= (char-int pchar) 48) (<= (char-int pchar) 57))
			(read-numbers stream (cons (- (char-int char) 48) lst) (read-char stream nil 'eof))
			(list 'num (calculate-number (cons (- (char-int char) 48) lst))))))

(defun read-chars (stream table &optional lst)
	(let ((char (read-char stream nil 'eof)))
		(if (not (eq char 'eof))
			(cond
				((and (>= (char-int char) 48) (<= (char-int char) 57)) (read-chars stream table (cons (read-numbers stream nil char) lst)))
				((or (eq char #\Space) (eq char #\NewLine) (eq char #\Tab)) (read-chars stream table lst))
				(t (read-chars stream table (cons (gethash char table) lst))))
			(reverse lst))))