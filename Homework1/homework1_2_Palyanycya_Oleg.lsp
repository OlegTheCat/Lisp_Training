(defun get-number-of-els (arr &optional (number 0))
	(if (eq arr nil)
		(progn number)
		(get-number-of-els (cdr arr) (+ number 1))))

(defun my-reverse (arr)
	(labels ((rev (arr tmplist)
				(if (eq arr nil)
					tmplist
					(rev (cdr arr) (cons (car arr) tmplist))
				)
			))
	(rev arr nil)))

(defun read-els (arr number &optional (tmplist nil))
	(if (> number (get-number-of-els arr))
		arr
		(if (< number 0)
			(progn nil)
			(if (not (eq number 0))
				(read-els (cdr arr) (- number 1) (cons (car arr) tmplist))
				(progn tmplist)))))

(defun detect-pal (arr)
	(if (equal arr (my-reverse arr))
		t
		nil)
		)

(defun get-pal (arr number)
	(if (or (< (get-number-of-els arr) number) (< number 2))
		nil
		(let ((pal (read-els arr number)))
			(if (eq (detect-pal pal) T)
				(progn pal)
				(get-pal (cdr arr) number)))))

(defun get-largest-pal (arr &optional (number nil))
	(if (eq number nil)
		(get-largest-pal arr (get-number-of-els arr))
		(if (not (< number 2))
			(let ((largest-pal (get-pal arr number)))
				(if (not(eq largest-pal nil))
					(progn largest-pal)
					(get-largest-pal arr (- number 1))))
			nil)))