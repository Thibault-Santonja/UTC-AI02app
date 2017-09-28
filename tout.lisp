; 2017-09-14
; TP - 1 LISP
; Santonja Thibault






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  TD 1  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CG-USER:(defun transf(expr) (if 	(listp expr)
							(list 	(cadr expr)
									(transf (car expr))
									(transf (caddr expr))
							)
							expr
					)
)
TRANSF

CG-USER:(setq Z '((x + 5) / ((x + 2) + (x * 2))))
((X + 5) / ((X + 2) + (X * 2)))

CG-USER:(transf Z)
(/ (+ X 5) (+ (+ X 2) (* X 2)))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  TP 1  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;  EXERCICE 1  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TP à rendre pour le 30 sept 2017

(CADR (CDR (CDR (CDR '(DO RE MI FA SOL LA SI))))) 

(CONS (CADR '((A B)(C D))) (CDDR '(A (B (C)))))

(CONS (CONS 'HELLO NIL) '(HOW ARE YOU))

(CONS 'JE (CONS 'JE (CONS 'JE (CONS 'BALBUTIE NIL))))

(CADR (CONS 'TIENS (CONS '(C EST SIMPLE) ())))

; question 4
(defun nombre3	(expr) 
				(if (AND (numberP (car expr)) (AND (numberP (cadr expr)) (numberP (caddr expr) ) ) )
					'BRAVO
					'PERDU
				)
)


(defun group	(L1 L2)
				(cond
					((not (null L1))
					(cons	(list (car L1) (car L2))
							(group (cdr L1) (cdr L2))
					))
				)
)

(defun revers (l)
			(cond
				((null l) '())
				(T (append 	(revers (cdr l)) ; ATTENTION APPEL A LA FONCTION PRÉCÉDENTE !
							(list (car l))
					)
				)
            )
) 

(defun palindrome (l)
	(equal l (revers l))
)

(defun mon-equal (x y)
	(cond 
		((numberp x) (numberp y))
        ((symbolp x) (eq x y))
        ((and (consp x) (consp y))
         (and 	(my-equal (car x) (car y)) 
         		(my-equal (cdr x) (cdr y))
         )
        )
        (t nil)
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;  EXERCICE 4  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq BaseTest '(
	("Guerre de Burgondie" 523 533 ("Royaume Franc") ("Burgondes")("Royaume de Bourgogne"))
	("Conquête de la Thuringe" 531 531 ("Royaume Franc") ("Thuringes")("Thuringes"))
	("Guerre des Goths" 535 553 (("Royaume ostrogoth")("Royaume Franc")("Alamans")("Royaume wisigoth")("Burgondes"))("Empire byzantin")("Péninsule italienne"))
	("Conquête de l'Alémanie" 536 536 ("Royaume Franc") ("Alamans")("Alémanie"))
	("Conquête de la Bavière" 555 555 ("Royaume Franc") ("Bavarii")("Bavière"))
	("Guerre franco-frisonne" 600 793 ("Royaume Franc") ("Royaume de Frise")(("Pays-Bas")("Allemagne")))
	("Guerre civile des Francs" 715 719 (("Royaume Franc")("Neustrie")) ("Austrasie")("Royaume franc"))
	("Guerre des Lombards" 755 758 ("Royaume Franc") ("Lombards")("Lombardie"))
	("Guerre d'Aquitaine" 761 768 ("Royaume Franc") ("Aquitains")(("Vasconie")("Aquitaine")))
	("Guerre des Saxons" 772 804 ("Royaume Franc") ("Saxons")("Germanie"))
	("Guerre des Lombards" 773 774 ("Royaume Franc") ("Lombards")("Lombardie"))
	("Invasion omeyyade en France" 719 759 ("Royaume Franc") ("Califat omeyyade")(("Royaume d'Aquitaine")("Septimanie")))
	("Guerre des Avars" 791 805 ("Royaume Franc") ("Avars")("Pannonie"))
	("Invasions sarrasines en Provence" 798 990 (("Royaume Franc")("Comté de Provence")) ("Sarrasins")("Provence"))
	("Invasions vikings en France" 799 1014 ("Royaume Franc") ("vikings")(("Normandie")("Bretagne")))
	("Première croisade" 1096 1099 (("Comté de Blois")("Comté de Toulouse")	("Comté de Boulogne")("Marquisat de Provence")
									("Comté de Flandre")("Duché de Normandie") ("Diocèse du Puy-en-Velay")	("Comté de Vermandois")
									("République de Gênes")	("Duché de Basse-Lotharingie")	("Principauté de Tarente")
									("Empire byzantin")	("Royaume de Petite-Arménie")	("Cross-Pattee-alternate")("Croisés") ("Royaume Franc"))
								(("Sultanat de Roum")("Danichmendides")("Califat fatimide"))("Terre sainte"))
	)
)

(car (cdr '("Guerre de Burgondie" 523 533 ("Royaume Franc") ("Burgondes")("Royaume de Bourgogne")) ))
(defun dateDebut (expr)
		(car (cdr expr))
)


(defun nomConflit (expr)
		(car expr)
)

(defun allies (expr)
		(car (cdr (cdr (cdr expr))))
)

(defun ennemis (expr)
		(car (cdr (cdr (cdr (cdr expr)))))
)

(defun lieu (expr)
		(car (cdr (cdr (cdr (cdr (cdr expr))))))
)

;(defun FB1 (expr)	;nom de la dernière liste de la liste
;		(if (not (null (cdr expr))) 
;			(FB1 (cdr expr))
;			(NOMCONFLIT (car expr))
;		)
;)


;;;;;;;;;;;;;;;OK;;;;;;;;;;;;;;;
(defun FB1 (expr)
	(cons
		(NOMCONFLIT (car expr))
		(if (not (null (cdr expr)))
			(FB1 (cdr expr))
		)
	)
)

(fb1 basetest)







;;;;;;;;;;;;;;;PB "NIL";;;;;;;;;;;;;;;;;;;;;;;;;
;(equal (allies (car basetest)) '("Royaume Franc"))
(defun FB2 (expr)
	(cons
		(if	(equal (allies (car expr)) '("Royaume Franc")) (nomConflit (car expr)) '())
		(if (not (null (cdr expr))) (FB2 (cdr expr)))
	)
)

(fb2 basetest)









;(defun FB3 (expr participant)
;	(cond
;		(OR (equal (allies (car expr)) participant)
;			(equal (ennemis (car expr)) participant)
;		)(nomConflit (car expr))
;		(not (null (cdr expr))) (FB3 (cdr expr) participant)
;	)
;)


;;;;;;;;;;;;;;;PB "NIL";;;;;;;;;;;;;;;;;;;;;;;;;

(defun FB3 (expr participant)
	(cons
		(if (OR (equal (allies (car expr)) participant)
				(equal (ennemis (car expr)) participant)
			)
			(nomConflit (car expr))
			'())
		(if (not (null (cdr expr))) (FB3 (cdr expr) participant) '())
	)
)

(fb3 basetest '("Thuringes"))












;;;;;;;;;;;;OK;;;;;;;;;;;;;;;;;;;
(defun FB4 (expr)
	(if
		(= (dateDebut (car expr)) 523) (nomConflit (car expr))
		(if (not (null (cdr expr))) (FB4 (cdr expr)))
	)
)










;;;;;;;;;;;;;;;PB "NIL";;;;;;;;;;;;;;;;;;;;;;;;;


(defun FB5 (expr)
	(cons
		(if (AND (> (dateDebut (car expr)) 523)
				 (< (dateDebut (car expr)) 715)
			) 
			(nomConflit (car expr))
			'())
		(if (not (null (cdr expr))) (FB5 (cdr expr)))
	)
)











;;;;;;;;;;;;OK;;;;;;;;;;;;;;;;;;;


;(or
;(equal (allies (car basetest)) '("Lombards"))
;(equal (ennemis (car basetest)) '("Lombards")))


(defun FB6_test (expr)
	(cond
		((equal (allies (car expr)) '("Lombards")) 1)
		((equal (ennemis (car expr)) '("Lombards")) 1)
		(T 0)
	)
)

(defun FB6 (expr)
  (if (null  expr)
      (FB6_test expr)
      (+ (FB6_test expr)
         (FB6 (cdr expr)))
   )
)
















;TD2


(defun simp (expr)
	(if (listp expr)
		(let ( 	(op (car expr))
				(u (simp (cadr expr)))
				(v (simp (caddr expr)))
			)
			(cond
				((and (numberP u) (numberp v)) (eval expr))
				((and (eq '+ op)(eql 0 u)) v)
				((and (eq '+ op)(eql 0 v)) u)
				((and (eq '* op)(eql 1 u)) v)
				((and (eq '* op)(eql 1 v)) u)
				((and (eq '* op)(or (eql 0 u) (eql 0 v))) 0)
			)
		)
		expr
	)
)
(defun deriv_all (expr var)
		(
			(if (listp expr) 
				(cond ((eq (car ewpr) '+)
						(list '+ (deriv_all (cadr expr) var)
								(deriv_all (caddr expr) var)
						)
					)
					(
						(eq (car expr)
							(list '+
								(list '* (deriv_all (cadr exp) var)(caddr expr))
								(list '* (cadr expr) (deriv_all (caddr expr) var))
							)
						)
					)
				)
				(if (eq var expr) 1 0)
			)
		)
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  TD 2  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun derivf (expr var)
	(if (listp expr)
		(list '+ 	(list '* (derivpf (cadr expr) var) (caddr expr))
					(list '* (cadr expr) (derivpf (caddr expr) var))
		)
		(derivterme expr var)
	)
)


(defun derivp (expr var)
	(if (listp expr)
		(list (car expr) (derivpf (cadr expr) var) (derivpf (caddr expr) var))
		(derivterme expr var)
	)
)

(defun derivpf (expr var)
	(if (eq (car expr) +)
		(derivp expr var)
	)
)

(defun simp (expr)
	(if (listp expr)
		(let ( 	(op (car expr))
				(u (simp (cadr expr)))
				(v (simp (caddr expr)))
			)
			(cond
				((and (numberP u) (numberp v)) (eval expr))
				((and (eq '+ op)(eql 0 u)) v)
				((and (eq '+ op)(eql 0 v)) u)
				((and (eq '* op)(eql 1 u)) v)
				((and (eq '* op)(eql 1 v)) u)
				((and (eq '* op)(or (eql 0 u) (eql 0 v))) 0)
			)
		)
		expr
	)
)





;AI02_TP1_SANTONJA

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
;	(mapcar #'(lambda (y) )
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



; => tester avec un Loop, formet


(setq ll
	'(html 	(header (title "ma page")) 
			(body 	(h1 "un titre")
					(p "soror et aemula Romae")
			)		
	)
)




(defun baliseDeb (expr)
	(concatenate 'string "<" (string expr) ">")
)
(defun baliseFin (expr)
	(concatenate 'string "</" (string expr) ">")
)

(defun baliseDeb (expr)
(with-open-file (str "D:/index.html"
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
  (format str (concatenate 'string "<" (string expr) ">")))
)
(defun baliseFin (expr)
(with-open-file (str "D:/index.html"
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
  (format str (concatenate 'string "</" (string expr) ">")))
)

(defun make_html (expr)
	(if (listp (car expr)) (make_html (car expr)) (print (baliseDeb (car expr))))
	(if (not (null (cdr expr))) 
		(if (stringp (cdr expr)) 
			(print (cdr expr)) 
			(make_html (cdr expr)) 
		)
	)
	(if (not (listp (car expr))) 
		(print (baliseFin (car expr)))
	)
)





(with-open-file (str "D:/index.html"
                     :direction :output
                     :if-exists :overwrite
                     :if-does-not-exist :create)
  (format str (make-html ll))
)

;(with-open-file (str "D:/filename.txt"
;                     :direction :output
;                     :if-exists :supersede
;                     :if-does-not-exist :create)
;  (format str "write anything ~%")
;)

; si le premier element n'est pas une liste l'afficher (car c'est la balise)
; sinon faire appel à make_html

; si il y a un cdr -> si le cdr est un texte afficher le cdr 
;						sinon faire appel à make_html

; si le premier élément n'est pas une list (c'est donc une balise) -> fermer la balise




(defun make-html (l)
    (format t (concatenate 'string "<" (string (car l)) ">"))
    (dolist (elem (cdr l)) 
        (if (listp elem) 
            (make-html elem) 
            (format t elem)
        )
    )
    (format t (concatenate 'string "</" (string (car l)) ">"))
)


;(defun make-html (l)
;    (format t (deb l))
;    (dolist (elem (cdr l)) 
;        (if (listp elem) 
;            (make-html elem) 
;            (format t (ecrire l))
;        )
;    )
;    (format t (fin l))
;)

;(defun ecrire (l)
;	(with-open-file (str "D:/index.html"
;	                     :direction :output
;	                     :if-does-not-exist :create)
;	  (format str l)
;	)
;)

;(defun deb (l)
;	(with-open-file (str "D:/index.html"
;                     :direction :output
;                     :if-does-not-exist :create)
;  (format str (concatenate 'string "<" (string (car l)) ">")))
;)

;(defun fin (l)
;		(with-open-file (str "D:/index.html"
;                     :direction :output
;                     :if-does-not-exist :create)
;  (format str (concatenate 'string "</" (string (car l)) ">")))
;)




(with-open-file (str "D:/index.html"
                     :direction :output
                     :if-exists :overwrite
                     :if-does-not-exist :create)
  (format str "write anything ~%")
)


(with-open-file (str "D:/index.html"
                     :direction :output
                     :if-exists :overwrite
                     :if-does-not-exist :create)
  (format str (make-html ll))
)








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


