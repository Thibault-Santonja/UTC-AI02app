;AI02_TD3_SANTONJA
;2017-09-25
;Exercice 1

(defun affiche (expr)
    (cons
        (car expr)
        (if (not (null (cdr expr))) (affiche (cdr expr)))
    )
)

(defun affiche1 (expr)
    (if x 
        (progn (print (car x))
        (print1 (cdr x)))
    )
)

;(defun affiche2 (expr)
;   (mapcar #'(lambda (y) )
;)

(defun affiche3 (expr)
    (cond
        ((null expr) nil)
        (T (print (car expr)) (print3 (cdr expr)))
    )
)

(defun print4 (x)
    (dolist (y x "fin") (print y))
)

(defun print5 (expr)
    (format "~{~&~S ~}" expr)
)



;Exercice 2

(defun make-html (l)
    (format t (write (concatenate 'string "<" (string (car l)) ">") :stream file))
    (dolist (elem (cdr l)) 
        (if (listp elem) 
            (make-html elem) 
            (format t (write elem :stream file))
        )
    )
    (format t (write (concatenate 'string "</" (string (car l)) ">") :stream file))
)

(setq file (open "D:/index.html" 
			:if-does-not-exist :create 
			:direction : output
			:if-exists : overwrite
			)
)



(load "D:/index.html")



(write (make-html ll) :stream file)



(close file)