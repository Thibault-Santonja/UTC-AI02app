;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  TD 4  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



('entree (1 
			(2 
				(7 
					(8 
						(9 
							(10 
								(15 
									(16 
										(17 
											(18 
												(19 
													(20 
														(13 'sortie)
													)
												)
											)
										)
									) 
								11 
									(14 
									12 
										(5 
											(4)
										)
									)
								)
							)
						) 
					6 
						(3)
					)
				)
			)
		)
)

(setq lab '(
	(E 1) (1 E 2) (2 1 7) 
	(3 6) (4 5) (5 4 12) 
	(6 3 7) (7 2 6 8) (8 7 9)
	(9 8 10) (10 9 11 15)
	(11 10 12 14) (12 5 11)
	(13 20) (14 11) (15 10 16)
	(16 15 17) (17 16 18)
	(18 17 19) (19 18 20) 
	(20 13 19 S)
	)
)



(defun successeur (etat labyrinthe)
	(cdr (assoc etat labyrinthe))
)



(setq l_res ())



(setq l_explore ())



(defun explore (expr)
	(if (eq expr 'S)
		(push expr l_res)
		(progn
			(push expr l_explore)
			(dolist (succ (cdr (assoc expr lab)))
				(if (not (member succ l_explore))
					(explore succ)
				)
				(if (member succ l_res)
						(push expr l_res)
				)
			)
		)
	)
	(if (eq expr 'E)
		(print l_res))
)
