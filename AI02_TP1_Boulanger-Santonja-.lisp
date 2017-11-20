; 2017-09-14
; TP - 1 LISP
; Santonja Thibault - Boulanger Corentin




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  TP 1  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;  EXERCICE 1  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
;;;;;;;;;;;;;;;;;;;;;;;  EXERCICE 2  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;  EXERCICE 3  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


(defun FB1 (expr)
	(write (NOMCONFLIT (car expr)))
	(if (not (null (cdr expr)))
		(FB1 (cdr expr))
	)
)


(defun FB2 (expr)
	(if	(equal (allies (car expr)) '("Royaume Franc")) (write (nomConflit (car expr))))
	(if (not (null (cdr expr))) (FB2 (cdr expr)))
)


(defun FB3 (expr participant)
	(if (not (null (cdr expr)))
		(if (OR (equal (allies (car expr)) participant)
				(equal (ennemis (car expr)) participant)
			)
			(cons (nomConflit (car expr)) (FB3 (cdr expr) participant))
			(FB3 (cdr expr) participant)
		)
		NIL
	)
)

(fb3 basetest '("Thuringes"))


(defun FB4 (expr)
	(if
		(= (dateDebut (car expr)) 523) (nomConflit (car expr))
		(if (not (null (cdr expr))) (FB4 (cdr expr)))
	)
)
 


(defun FB5 (expr)
	(if (not (null (cdr expr)))
		(if (AND (> (dateDebut (car expr)) 523)
				 (< (dateDebut (car expr)) 715)
			) 
			(cons (nomConflit (car expr)) (FB5 (cdr expr)))
			(FB5 (cdr expr))
		)
	)
)
 


(defun FB6 (expr)
  (if (null  expr)
      (FB6_test expr)
      (+ (FB6_test expr)
         (FB6 (cdr expr)))
   )
)







