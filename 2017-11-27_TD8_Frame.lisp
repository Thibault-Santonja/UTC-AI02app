;2017-11-27_TD8_Frame


(l'éléphant
	(couleur
		(value gris)
	)
	(age
		(value 18)
	)
	(poids
		(value 45)
	)
)


(Clyde
	(type (value INDIVIDU))
	(IS-A (value ELEPHANT))
	(COLOR (value "grey"))
	(AGE (value 5))
	(POIDS (value (resultat de la fonction associé)))
)


#|| MON ALGO
création_individue (nom (PROP1 VAL1 ...))
	créer un indentifiant
	push l'indentifiant dans la liste des individus
	pour chaque couples (PROP VALUE)
		ajouter (PROP (value VALUE)) à la frame que l'on vient de créer
	fin pour
	retourner l'identifiant
FIN
||#

#| CORRECTION
création_individue (nom (PROP1 VAL1 ...))
	Vérifier si le frame existe (Élephant)
		sinon ERROR
	créer un indentifiant pour le frame
	créer le début de la représentation
		(nom
			(type (value INDIVIDU))
			(is-a (value ELEPHANT))
		)
	push l'indentifiant dans la liste des individus
	pour chaque slot (couples (PROP VALUE))
		vérifier si le slot appartient au concept
			vérifier s'il y a un démon
				appliquer le démon
				si demon OK
					l'appiquer
				sinon
					warning
			sinon 
				ajouter (PROP (value VALUE)) à la frame que l'on vient de créer
		sinon 
			WARNING
	fin pour
	retourner l'identifiant
FIN
|#


;ma fonction
(defun make-individu (NAME list-prop)
	"Documentation for make-individu."
	(if (not (member NAME *frame*))
		(error "ERROR: concept ~s inexistant" NAME)
	)
	(let 
		(
			(id (gentemp NAME))
			(name (cadr (member 'NAME list-prop)))
		)
		(if (not name)
			(error "ERROR : nom pas passé en paramètre")
		)
	)
)

0

;correction
(defun make-individu-corr (name concept prop-val-list)
	"Documentation for make-individu-corr."
	(unless (member concept *frame)	;unless =  (if (not ...)) (progn ...)
		(error "ERROR: concept ~s inexistant" NAME)
	)

	(let 
		(
			(id (gentemp 'F))
			(allowed-slot (mapcar #'car (cdr (symbol-value concept))))
			slot value fn
		)
		(set id '(,name (type (value individual))
						(is-a (value ,concept))))
		(loop
			(unless prop-val-list (return nil))
			(setq 	slot (pop prop-val-list)
					value (pop prop-val-list))
			(when (member set allowed-slot)
				(setq fn (cadr (assoc 'if-added (cdr (assoc slot (cdr (symbol-value concept)))))))
				(setq value (if fn (funcall fn slot value)))
				(when value
					(set id (append (symbol-value id) (list (list slot (list 'value value)))))
				)
			)
		)
		(push id *frame*)
		id
	)
)

#||
get-slot-value (INDIVIDU AGE)
	accéder à INDIVIDUE s'il existe 
	sinon
		ERROR
	Fin Si
	accéder au slot s'il existe
	sinon
		ERROR
	Fin Si
	accéder à la valeur du slot si elle existe
		l'afficher
	sinon 
		si un démon existe
			applique le démon 
			si résultat démon OK
				l'afficher
			sinon
				ERREUR
			Fin Si
		sinon 
			ERROR
		Fin Si
	Fin Si
FIN



fonction get-slot-value_cor (nom, slot)
	si le nom existe dans la liste des frames
		si il existe une valeur pour le slot
			la retourner
		sinon
			si il existe un démon IF-NEEDED
				renvoyer son résultat
			sinon
				si un slot is-a existe
					renvoyer get-slot-val-bis (nom_is-a, nom, slot)
				sinon
					retourner NIL
	retourner NIL
FIN
||#



(defun get-slot-value (nom, slot)
	"Documentation for get-slot-value."
	(if (member nom *frame*)
		(let (individu (symbol-value (member nom *frame*)))
			(if (member 'value (cdr individu))
				(return-from get-slot-value (cadr (assoc 'value (cdr (assoc slot (cdr individu)))))) ; on retourne la valeur
				(if 

				)
			)
		)
		(error "Le frame ~S n'existe pas" nom)
	)

)


(defun get-slot-value (nom slot)
	(let (individu)
		(dolist (f *frames*)
			(if (eq (car (symbol-value f)) nom)
				(setq individu (symbol-value f))
			)
		)
		(if individu
			(if (assoc 'value (cdr (assoc slot (cdr individu)))) ; si le slot existe dans l'individu
				(return-from get-slot-value (cadr (assoc 'value (cdr (assoc slot (cdr individu)))))) ; on retourne la valeur
				(if (assoc slot (cdr (symbol-value (cadr (assoc 'value (cdr (assoc 'is_a (cdr individu)))))))) ; sinon si le slot existe dans le parent
					(let ((slot_parent (assoc slot (cdr (symbol-value (cadr (assoc 'value (cdr (assoc 'is_a (cdr individu))))))))))
						(cond
							((assoc 'DEFAULT (cdr slot_parent)) ; si il y a une valeur par défaut
								(return-from get-slot-value (cdr (assoc 'DEFAULT (cdr slot_parent))))
							)
							((assoc 'IF-NEEDED (cdr slot_parent)) ; si il y a une fonction IF-NEEDED
								(return-from get-slot-value (funcall (cadr (assoc 'IFNEEDED (cdr slot_parent))) individu))
							)
							(T (error "Pas de valeur trouvée pour ce slot ni de fonction de calcul"))
						)
					)
					(error "Le slot ~S n'exite pas" slot) ; erreur : le slot n'existe pas
				)
			)
			(error "Le frame ~S n'existe pas" nom)
		)
	)
)


(defun waves (node mark type)
	"Documentation for waves."
	(mapcar 
		#'(lambda (arc)
			(if (eq (cadr (assoc 'from-node arc)) node)
				(mark-node (cadr (assoc 'to-node arc)) mark)
			)
		)
		(cdr (assac type *arcs*))
	)
)







(defun get-slot-value_corr (parameters)
	"Documentation for get-slot-value_corr."
	(unless (member frame *frames*) (return-from get-slot-value_corr nil))
	(let ((slot-list (cdr (assoc slot (cdr (symbol-value frame )))))
			value parent
			)
		(setq value 
			(cond
				((null slot-list) nil)
				((cadr (assoc 'value slot-list)))
			)
		)
		(when value (return-from get-slot-value_corr value))
		(setq parent (cadr (cadr (assoc 'is-a (cdr (symbol-value frame))))))
		(when parent (inherit-slot-value parent slot frame))
	)
)
(defun inherit-slot-value (frame slot original-frame)
	(unless (and frame (member frame *frame*))
		(return-from inherit-slot-value nil)
	)
	(let ((slot-list (cdr (assoc slot (cdr (symbol-value frame)))))
		parent demon value)
		(setq value (cond ((null slot-list) nil)
						((cadr (assoc 'default slot-list)))
						((setq demon (cadr (assoc 'IF-NEEDED slot-list)))
							(funcall demon original-frame slot))))
		(when value
			(return-from inherit-slot-value value))
		(setq parent (cadr (assoc 'is-a (cdr (symbol-value frame)))))
		(when parent
			(inherit-slot-value parent slot original-frame))
	)
)