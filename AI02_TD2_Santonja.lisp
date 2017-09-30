;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  TD 2  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun derive-terme (expr var)
	(if (eq var expr) 1 0)
)



(defun derive-add (expr var)
	(if (listp expr) 
		(list (car expr) (derive-add (cadr epr) var) (derive-add (caddr expr) var)) 
		(derive-terme expr var)
	)
)



(defun derive-mult (expr var)
	(if (listp expr)
		(list '+ (list '* (derive-mult (cadr expr) var) (caddr expr)) (list '* (cadr expr) (derive-mult (caddr expr) var)))
		(derive-terme expr var)
	)
)



(defun derive-all (expr var)
	(if (listp expr)
		(cond ((eq (car expr) '+) (list '+ 
									(derive-all (cadr expr) var) 
									(derive-all (caddr expr) var)
								))
			  ((eq (car expr) '*) (list '+ 
			  							(list '* 
			  								(derive-all (cadr expr) var) 
			  								(caddr expr)
			  							)
			  							(list '* 
			  								(cadr expr) 
			  								(derive-all (caddr expr) var)
			  							)
			  					))
		)
		(derive-terme expr var)
	)
)



(defun simp (expr)
	(if (listp expr)
		(let (
				(op (car expr))
		 	 	(u (simp (cadr expr)))
		 	 	(v (simp (caddr expr)))
		 	 )
			(cond 
				((and (numberp u) (numberp v)) (eval expr))
				((and (eq '+ op) (eql 0 u)) v)
				((and (eq '+ op) (eql 0 v)) u)
				((and (eq '* op) (eql 1 u)) v)
				((and (eq '* op) (eql 1 v)) u)
				((and (eq '* op) (or (eql 0 u) (eql 0 v))) 0)
				(T (list op u v))
			)
		)
		expr	
	)
)
