#|
	In this task a leave of the tree is equal to the node with empty list of children.
	Examples:
		(5 (3 2 1)) - wrong!
		(5 ((3) (2) (1))) - wrong!
		(5 ((3 NIL) (2 NIL) (1 NIL))) - correct!
|#

(defun make-node (val &optional list-of-nodes)
	;We can't create a node with value that = NIL.
	(if val
		(list val list-of-nodes)
		nil))

(defun get-children (node)
	(car (cdr node)))

(defun get-node (node)
	(car node))


(defun traverse (fun tree fget-node fget-children fmake-node)
	(if (not(eq tree nil))
		(funcall fmake-node (funcall fun (funcall fget-node tree)) 
			;Applicating the function 'traverse' to every child of the tree.
			(labels ((traverse-children (children)
						(if children
							(cons (traverse fun (car children) fget-node fget-children fmake-node) (traverse-children (cdr children)))
							nil))) (traverse-children (funcall fget-children tree))))
		nil))

#|
Example:
(setq left-branch (make-node 1 (list (make-node 2 (list (make-node 5) (make-node 6))) (make-node 3) (make-node 4))))
(setq right-branch (make-node 7 (list (make-node 3))))
(setq root (make-node 8 (list left-branch right-branch)))
(print root)
(setq changed-root (traverse #'1+ root #'get-node #'get-children #'make-node))
(print changed-root)
|#