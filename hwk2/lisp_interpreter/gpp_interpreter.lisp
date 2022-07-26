( load "gpp_lexer.lisp" )
(setq symbol_table nil)

;NOTE: There were too many states and I didn't write all the cases in the comment section of DFA here, though all the DFA states are implemented
;Following are the rules of the Grammar

;CFG rules - all implemented
;RULE SECTION
; 1  START : EXPI
;  2        | EXPLISTI
;  3        | EXPB
;  4        | START EXPI
;  5        | START EXPLISTI
;  6        | START EXPB
;  7        | COMMENT
;  8        | START COMMENT
;  9        | OP_OP KW_EXIT OP_CP
; 10        | START OP_OP KW_EXIT OP_CP

; 11  EXPI : OP_OP OP_PLUS EXPI EXPI OP_CP
; 12       | OP_OP OP_MINUS EXPI EXPI OP_CP
; 13       | OP_OP OP_MULT EXPI EXPI OP_CP
; 14       | OP_OP OP_DBLMULT EXPI EXPI OP_CP
; 15       | OP_OP OP_DIV EXPI EXPI OP_CP
; 16       | IDENTIFIER
; 17       | VALUE
; 18       | OP_OP KW_SET IDENTIFIER EXPI OP_CP
; 19       | OP_OP KW_IF EXPB EXPI OP_CP
; 20       | OP_OP KW_FOR EXPB EXPI OP_CP
; 21       | OP_OP KW_IF EXPB EXPI EXPI OP_CP
; 22       | OP_OP KW_DISP EXPI OP_CP

; 23  EXPB : OP_OP KW_AND EXPB EXPB OP_CP
; 24       | OP_OP KW_OR EXPB EXPB OP_CP
; 25       | OP_OP KW_NOT EXPB OP_CP
; 26       | OP_OP KW_EQUAL EXPB EXPB OP_CP
; 27       | OP_OP KW_EQUAL EXPI EXPI OP_CP
; 28       | OP_OP KW_LESS EXPI EXPI OP_CP
; 29       | KW_TRUE
; 30       | KW_FALSE
; 31       | OP_CP KW_DISP EXPB OP_CP

; 32  EXPLISTI : OP_OP KW_CONCAT EXPLISTI EXPLISTI OP_CP
; 33           | OP_OP KW_APPEND EXPI EXPLISTI OP_CP
; 34           | LISTVALUE
; 35           | OP_OP KW_LIST VALUES OP_CP
; 36           | OP_OP KW_DISP LISTVALUE OP_CP

; 37  LISTVALUE : OP_OP VALUES OP_CC
; 38            | OP_OP OP_CC
; 39            | KW_NIL

; 40  VALUES : VALUES VALUE
; 41         | VALUE
; START	: EXPI {printf("Syntax OK.\nResult %d\n",$1);}
; 		| EXPLISTI {printf("Syntax OK.\nResult "); print_list($1);}
; 		| EXPB {printf("Syntax OK.\nResult %d\n",$1);}
; 		| START EXPI {printf("Syntax OK.\nResult %d\n",$2); }
; 		| START EXPLISTI {printf("Syntax OK.\nResult "); print_list($2);}
; 		| START EXPB {printf("Syntax OK.\nResult %d\n",$2);}
; 		| COMMENT {printf("COMMENT\n");}
; 		| START COMMENT {printf("COMMENT\n");}
; 		| OP_OP KW_EXIT OP_CP {freeList(); exit(0);}
; 		| START OP_OP KW_EXIT OP_CP { freeList(); exit(0); }
; 		;

;rule represents current rule and it starts from 0 
(defun gpp_interpreter(rule)

	(setq res_interpreter nil)

	(case rule 
		(1 (setq res_interpreter (rule-1-expi token_list)))
		(2 (setq res_interpreter (rule-3-expb token_list)))
		(3 (setq res_interpreter (rule-2-explisti token_list)))
		)	


	(if ( not (eq res_interpreter nil) )
		(return-from gpp_interpreter res_interpreter)
		(progn
			;if not the valid result
			;continue to the next rule
			;or if rule is larger than 9 report the syntax error since it does not map to any rule
			(if (eq rule 3)
				(progn
					(print "Syntax error.")
					(exit)
					)
				(progn
					;otherwise continue trying to map the to some of the rules
					; (setq rule (+ rule 1))
					(gpp_interpreter (+ rule 1))
					)
			)
		)
	)
)

(defun gpp_interpreter_expi_expb(rule tokens)

	; (print "HERE TOKENS")
	; (print tokens)

	(setq res_interpreter_expi_exbi nil)

	(case rule 
		(1 (setq res_interpreter_expi_exbi (rule-1-expi tokens)))
		(2 (setq res_interpreter_expi_exbi (rule-3-expb tokens)))
	)	


	(if ( not (eq res_interpreter_expi_exbi nil) )
		(return-from gpp_interpreter_expi_expb res_interpreter_expi_exbi)
		(progn
			;if not the valid result
			;continue to the next rule
			;or if rule is larger than 9 report the syntax error since it does not map to any rule
			(if (eq rule 2)
				(progn
					(print "Syntax error.")
					(exit)
					)
				(progn
					;otherwise continue trying to map the to some of the rules
					; (setq rule (+ rule 1))
					(gpp_interpreter_expi_expb (+ rule 1) tokens)
					)
			)
		)
	)
)

(defun syntax-error (&optional s)
	(print "Syntax error")
	(if (not (eq s nil))
		(print s)
		(terpri)
	)
	(exit)
)
(defun rule-1-expi( tokens )
	;expi is here
	; EXPI 	: OP_OP OP_PLUS EXPI EXPI OP_CP {$$ = $3 + $4; }
	; 	| OP_OP OP_MINUS EXPI EXPI OP_CP {$$ = $3 - $4;}
	; 	| OP_OP OP_MULT EXPI EXPI OP_CP {$$ = $3*$4; }
	; 	| OP_OP OP_DBLMULT EXPI EXPI OP_CP {$$ = pow_gpp($3 , $4);}
	; 	| OP_OP OP_DIV EXPI EXPI OP_CP {$$ = $3 / $4; }
	
	;first check if either VAL or ID

	(if (eq tokens nil)
		(return-from rule-1-expi nil)
	)
	;handling first terminals
	;rule 17
	(if
		(eq (car tokens) 'VALUE)
		(return-from rule-1-expi (get-value))
	)
	;handling first terminals
	;rule 16
	(if
		(eq (car tokens) 'IDENTIFIER)
		(return-from rule-1-expi (identifier-to-value))
	)

	; now we know it is a some other operator and some non terminal in here
	; check OP_OP  OP_CC
	(if (eq (is-enclosed tokens) T)
		(progn
			;check for rules 11-15
			(setq res (rule-11-15_27_28-expi-helper tokens))
			;if didn't pass
			;check for set
			(if (eq res nil)
				(setq res (rule-18-expi-set tokens)))
			; (print "RULE 19")
			; (print res)
			;if res is nil
			;check for rule-18-23 and continue like that
		)
		(return-from rule-1-expi nil);if not enclosed meaning that this can not be EXPI so go and try to map it to some other rule!
	)

	(return-from rule-1-expi res)

)


(defun rule-3-expb( tokens )
	
	; 23  EXPB : OP_OP KW_AND EXPB EXPB OP_CP
	; 24       | OP_OP KW_OR EXPB EXPB OP_CP
	; 25       | OP_OP KW_NOT EXPB OP_CP
	; 26       | OP_OP KW_EQUAL EXPB EXPB OP_CP
	; 27       | OP_OP KW_EQUAL EXPI EXPI OP_CP
	; 28       | OP_OP KW_LESS EXPI EXPI OP_CP
	; 29       | KW_TRUE
	; 30       | KW_FALSE
	; 31       | OP_CP KW_DISP EXPB OP_CP

	;handling first terminals
	;rule 29
	(if
		(eq (car tokens) 'KW_TRUE)
		(return-from rule-3-expb (get-value))
	)
	;handling first terminals
	;rule 30
	(if
		(eq (car tokens) 'KW_FALSE)
		(return-from rule-3-expb (get-value))
	)

	;rule hybrid
	(if
		(eq (car tokens) 'VALUE)
		(return-from rule-3-expb (get-value))
	)
	;rule hybrid
	(if
		(eq (car tokens) 'IDENTIFIER)
		(return-from rule-3-expb (identifier-to-value))
	)

	; now we know it is a some other operator and some non terminal in here
	; check OP_OP  OP_CC
	(if (eq (is-enclosed tokens) T)
		(progn
			;check for rules 11-15
			(setq res (rule-23-26-expb-helper tokens))
			;if didn't pass
			;check for set
			; (if (eq res nil)
			; 	(setq res (rule-18-expi-set tokens)))
			; (print "RULE 19")
			; (print res)
			;if res is nil
			;check for rule-18-23 and continue like that
		)
		(return-from rule-3-expi nil);if not enclosed meaning that this can not be EXPI so go and try to map it to some other rule!
	)

	(return-from rule-3-expb res)

)
(defun rule-23-26-expb-helper(tokens)
	;vals -
	; operator
	; tokens

	; 23  EXPB : OP_OP KW_AND EXPB EXPB OP_CP
	; 24       | OP_OP KW_OR EXPB EXPB OP_CP
	; 26       | OP_OP KW_EQUAL EXPB EXPB OP_CP	;expi argumented

	; 25       | OP_OP KW_NOT EXPB OP_CP
	

	;reduction of the parsed list
	(setq tokens (sandwich-list tokens) )
	( let 
		( ( operator (car tokens) ) ) 
		;if it not a desired operator go to rule-2
		(if (eq (is-rule-23-26-expb-operator operator) nil)
			(return-from rule-23-26-expb-helper nil)
		)

		; IT IS rule 11 - 15 everything from now on can produce either a good output or an error

		;reduce the token list now!
		(setq tokens ( cdr tokens ) )
		; 4 cases
		; ID/VAL ID/VAL
		; EXPI ID/VAL
		; ID/VAL EXPI
		; EXPI EXPI


		;check for KW_NOT immediately first!
		(if (eq operator 'KW_NOT)
			(progn
				; KW_NOT EXPBI
				; (print tokens)
				(let ((lhs (rule-3-expb tokens)))
					(return-from rule-23-26-expb-helper (rule-23-26-expb-exec lhs nil operator))
				)
			)
		)

		; (setq lhs nil) ; left hand side
		; (setq rhs nil) ; right hand side
		;get next token
		(if (eq (is-t-f (car tokens)) T )
			(progn
				( let 
					( ( lhs (rule-3-expb (list (car tokens) ) ) ) ) 
					;(setq lhs  )
					(setq tokens (cdr tokens))

					;T/F + sth
					;rhs in tokens - check ID/VAL or (   -> neither -> syntax error
					(if (eq (is-t-f (car tokens)) T )
						(progn; T/F   T/F

							; use local variables because of tail recursion
							( let 
								( ( rhs (rule-3-expb (list (car tokens) ) ) ) )
								; rhs = right hand side
								(setq tokens (cdr tokens))
								;after this sytax is ok if there is nothing left in the tokens list!
								(if (eq tokens nil);this means that the sytax is alright
									(progn 
										(return-from  rule-23-26-expb-helper (rule-23-26-expb-exec lhs rhs operator))
									)
									(progn;this means that there is problema and that there is a syntax error here because this can't be the case for our langauge and rule 11-15 is not satified as it has more that 2 arguments!
										(syntax-error "Too many arguments for for EXPBI --- rule 22-26")							
									)
								)
							)
						)
						(progn;T/F   EXPB/CP  -> if not EXPI than sytax error
							

							;we need rhs
							;it can be EXPB only if enclosed
							;expansion of EXPB here
							(if 
								(eq (is-enclosed tokens) T)
								(progn
									; (print "LHS HERE IS ")
									; (print lhs)
									; (print "RHS HERE IS ")
									; (print rhs)
									; (print "LHS HERE IS ")
									; (print lhs)
									;(setq rhs (rule-3-expb tokens));find value of the EXPI on the right handside
									(setq rhs (gpp_interpreter_expi_expb 1 tokens));find value of the EXPI on the right handside
									
									(return-from  rule-23-26-expb-helper (rule-23-26-expb-exec lhs rhs operator))
								)
								(syntax-error "EXPB - OP_OP true/false  ---NON true/false non Enlosed--- OP_CP");otherwise it is a syntax error
							)
						)
					)
				)
			)
			(progn ;lhs != F/T
				;lhs ?= EXPBI/EXPI   -> tokens enclosed ! ()
				(if (eq (is-enclosed-lhs tokens) T)
					(progn ; lhs = EXPBI
						; (print "TOKENS")
						; (print tokens)

						; lhs = EXPBI
						; get the enclosed list 	->   form of: ( ... )   id/val/expi
						(let (  (count_op (count-op tokens) ) )
							( let ( ( tokens_new (get-sublist tokens count_op) ) ) 
								( let ((tokens_lhs (car tokens_new)) (tokens_rhs (cadr tokens_new)) ) 
									; (print "LHS")
									; (print tokens_lhs)
									; (print "RHS")
									; (print tokens_rhs)	

									( let ((lhs (gpp_interpreter_expi_expb 1 tokens_lhs)) (rhs (gpp_interpreter_expi_expb 1 tokens_rhs) )) 
										(if (not 
												(or 
													(eq lhs nil)(eq rhs nil) )
											)
											(return-from  rule-23-26-expb-helper (rule-23-26-expb-exec lhs rhs operator))
											(syntax-error "RHS or LHS of EXPBI not valid")
										)
									)
								)
							)
						)
					)
					(progn
						;lhs != EXPI/EXPIB
						;lhs != T/F
						;sytanx error!
						(syntax-error "lhs != EXPBI --- lhs != T/F --- in rule 22-26 of expbi")
					)
				)
			)
		)
	);end of let for operator
)

(defun is-enclosed(tokens)
	(if (and 
			(eq (car tokens ) 'OP_OP)
			(eq (car (last tokens) ) 'OP_CP)
		)
		(return-from is-enclosed T)
		(return-from is-enclosed nil)
	)
)
(defun is-enclosed-lhs( tokens ) 
	(if (not (eq (car tokens) 'OP_OP) )
		(return-from is-enclosed-lhs nil))
	;otherwise see that the number of open parantesis is the same as the number of closed paranteses
	;if list is ( ( ) ) for example
	; we put ount (  ) ) and decrease count

	(let ((temp_list (cdr tokens)))

		(loop 
		   ; (setq a (+ a 1))
		   ; (write a)
		   ; (terpri)
		   ( setq temp_list (cdr temp_list) )
		   ; (print "HERE")
		   ; (print (car temp_list))
		   (when (eq temp_list nil) (return-from is-enclosed-lhs nil))
		   (when (eq (car temp_list) 'OP_CP) (return-from is-enclosed-lhs T))
		)
	)
)

(defun is-t-f( symbl )
	(if (or 
			(eq symbl 'KW_TRUE )
			(eq symbl 'KW_FALSE )
			(eq symbl 'VALUE)
			(eq symbl 'IDENTIFIER)
		)
		(return-from is-t-f T)
		(return-from is-t-f nil)
	)
)
(defun is-ID-VAL( symbl )
	(if (or 
			(eq symbl 'IDENTIFIER )
			(eq symbl 'VALUE ) )
		(return-from is-ID-VAL T)
		(return-from is-ID-VAL nil)
	)
)
(defun expi-helper (&optional rule reduced_list)	
	;(print "IN EXPI")
	; 11  EXPI : OP_OP OP_PLUS EXPI EXPI OP_CP
	; 12       | OP_OP OP_MINUS EXPI EXPI OP_CP
	; 13       | OP_OP OP_MULT EXPI EXPI OP_CP
	; 14       | OP_OP OP_DBLMULT EXPI EXPI OP_CP
	; 15       | OP_OP OP_DIV EXPI EXPI OP_CP
	; 16       | IDENTIFIER
	; 17       | VALUE
	; 18       | OP_OP KW_SET IDENTIFIER EXPI OP_CP
	; 19       | OP_OP KW_IF EXPB EXPI OP_CP
	; 20       | OP_OP KW_FOR EXPB EXPI OP_CP
	; 21       | OP_OP KW_IF EXPB EXPI EXPI OP_CP
	; 22       | OP_OP KW_DISP EXPI OP_CP
	(if (eq rule nil)
		(setq rule 11);meaning check all
		)
	(setq res nil)
	(case rule 
			(11 (setq res rule-11-expi-plus))
			(12 (rule-12))
			(13 (rule-13))
			(14 (rule-14))
			(15 (rule-15))
			(16 (setq res (rule-16 reduced_list)))
			(17 (rule-17))
			(18 (rule-18))
			(19 (rule-19))
			(20 (rule-20))
			(21 (rule-21))
			(22 (rule-22))
			(23 (rule-23))
	)	
)

;rule 18 
; 18       | OP_OP KW_SET IDENTIFIER EXPI OP_CP
(defun rule-18-expi-set(tokens)

	(setq tokens (sandwich-list tokens) )
	( let 
		( ( operator (car tokens) ) ) 
		;if it not a desired operator go to rule-2
		(if (not (eq operator 'KW_SET))
			(return-from rule-18-expi-set nil))
		;reduce
		(setq tokens ( cdr tokens ) )
		;IDENTIFIER VALUE left
		;if not than syntax mistake
		; (print tokens)
		(if (not (and 
					(eq (car tokens) 'IDENTIFIER)
					(eq (car (cdr tokens)) 'VALUE)
			)	)
			(syntax-error "Set operation takes IDENTIFIER and VALUE as a parameter")
		)
		;when here we know it is set function
		( let ((rhs (rule-1-expi (cdr tokens))) (lhs (car identifier_list)))
			;set it here
			;shift
			(setq identifier_list (cdr identifier_list))
			(push (list lhs rhs) symbol_table )

			; (print "CAAR")
			; (print  (car symbol_table   ))
			(return-from rule-18-expi-set (cadar symbol_table))
		)
		
	)
)
; 11  EXPI : OP_OP OP_PLUS EXPI EXPI OP_CP
(defun rule-11-15_27_28-expi-helper(tokens)
	;vals -
	; operator
	; tokens

	; 11  EXPI : OP_OP OP_PLUS EXPI EXPI OP_CP
	; 12       | OP_OP OP_MINUS EXPI EXPI OP_CP
	; 13       | OP_OP OP_MULT EXPI EXPI OP_CP
	; 14       | OP_OP OP_DBLMULT EXPI EXPI OP_CP
	; 15       | OP_OP OP_DIV EXPI EXPI OP_CP
	; 27       | OP_OP KW_EQUAL EXPI EXPI OP_CP
	; 28       | OP_OP KW_LESS EXPI EXPI OP_CP
	;expi argumented

	;reduction of the parsed list
	(setq tokens (sandwich-list tokens) )
	( let 
		( ( operator (car tokens) ) ) 
		;if it not a desired operator go to rule-2
		(if (eq (is-rule-11-15_27_28-expi-operator operator) nil)
			(return-from rule-11-15_27_28-expi-helper nil)
		)

		; IT IS rule 11 - 15 everything from now on can produce either a good output or an error

		;reduce the token list now!
		(setq tokens ( cdr tokens ) )
		; 4 cases
		; ID/VAL ID/VAL
		; EXPI ID/VAL
		; ID/VAL EXPI
		; EXPI EXPI

		; (setq lhs nil) ; left hand side
		; (setq rhs nil) ; right hand side
		;get next token
		(if (eq (is-ID-VAL (car tokens)) T )
			(progn
				( let 
					( ( lhs (rule-1-expi (list (car tokens) ) ) ) ) 
					;(setq lhs  )
					(setq tokens (cdr tokens))
					;ID/VAL + sth
					;rhs in tokens - check ID/VAL or (   -> neither -> syntax error
					(if (eq (is-ID-VAL (car tokens)) T )
						(progn; ID/VAL   ID/VAL
							; use local variables because of tail recursion
							( let 
								( ( rhs (rule-1-expi (list (car tokens) ) ) ) )
								; rhs = right hand side
								(setq tokens (cdr tokens))
								;after this sytax is ok if there is nothing left in the tokens list!
								(if (eq tokens nil);this means that the sytax is alright
									(progn 
										(return-from  rule-11-15_27_28-expi-helper (rule-11-15_27_28-expi-exec lhs rhs operator))
									)
									(progn;this means that there is problema and that there is a syntax error here because this can't be the case for our langauge and rule 11-15 is not satified as it has more that 2 arguments!
										(syntax-error "Too many arguments for for EXPI --- rule 11-15")							
									)
								)
							)
						)
						(progn;ID/VAL   EXPI   -> if not EXPI than sytax error
							;we need rhs
							;it can be EXPI only if enclosed
							;expansion of EXPI here
							(if 
								(eq (is-enclosed tokens) T)
								(progn
									; (print "LHS HERE IS ")
									; (print lhs)
									; (print "RHS HERE IS ")
									; (print rhs)
									; (print "LHS HERE IS ")
									; (print lhs)
									(setq rhs (rule-1-expi tokens));find value of the EXPI on the right handside
									(return-from  rule-11-15_27_28-expi-helper (rule-11-15_27_28-expi-exec lhs rhs operator))
								)
								(syntax-error "EXPI - OP_OP ID/VAL  ---NON ID/VAL non Enlosed--- OP_CP");otherwise it is a syntax error
							)
						)
					)
				)
			)
			(progn ;lhs != ID/VAL 
				;lhs ?= EXPI/EXBI   -> tokens enclosed ! ()

				;check the operator to figure out if its either EXPI / EXPBI
				(if (not (or ( eq operator 'KW_IF)(eq operator 'KW_FOR) ) )
					(progn
						(if (eq (is-enclosed-lhs tokens) T)
							(progn ; lhs = EXPI
								; (print "TOKENS")
								; (print tokens)
								; lhs = EXPI
								; get the enclosed list 	->   form of: ( ... )   id/val/expi
								(let (  (count_op (count-op tokens) ) )
									( let ( ( tokens_new (get-sublist tokens count_op) ) ) 
										( let ((tokens_lhs (car tokens_new)) (tokens_rhs (cadr tokens_new)) ) 
											; (print "LHS")
											; (print tokens_lhs)
											; (print "RHS")
											; (print tokens_rhs)	
											
											( let ((lhs (rule-1-expi tokens_lhs)) (rhs (rule-1-expi tokens_rhs) )) 
												(if (not 
														(or 
															(eq lhs nil)(eq rhs nil) )
													)
													(return-from  rule-11-15_27_28-expi-helper (rule-11-15_27_28-expi-exec lhs rhs operator))
													(syntax-error "RHS or LHS of EXPI not valid")
												)
											)
										)
										
									)
								)
							)
							(progn
								;lhs != EXPI
								;lhs != ID/VAL
								;sytanx error!
								(syntax-error "lhs != EXPI --- lhs != ID/VAL --- in rule 11-15 of expi")
							)
						)
					)
					(progn ;IF / FOR
						  ;	   | OP_OP KW_IF EXPB EXPI OP_CP	
					; 20       | OP_OP KW_FOR EXPB EXPI OP_CP						;for has the same rule as if and works the same
					; 21       | OP_OP KW_IF EXPB EXPI EXPI OP_CP
					;reduce lhs from the  here and shift

					;whatever it is, separate the list now
						(let (  (count_op (count-op-plus tokens) ) )
							( let ( ( tokens_new (get-sublist tokens count_op) ) ) 
								( let ((tokens_lhs (car tokens_new)) (tokens_rhs (cadr tokens_new)) ) 
									; (print "IF STATEMENT AMAZING")
									; (print "LHS")
									; (print tokens_lhs)
									; (print "RHS")
									; (print tokens_rhs)	
									;check number of elements in the rhs
									; RHS = EXPI | EXPI EXPI     ->   rhs will contain exactly one element if it is in the rightmost list 
									; 

										;figure out RHS1
										;if VAL find
										;if NOT separate the list
										;check if VAL
									(if (eq (car tokens_rhs) 'VALUE)
										(progn; EXPI_1 = VAL    - >   remove and get the other one using expi
											(let ( (lhs (rule-3-expb tokens_lhs)) (rhs1 (rule-1-expi (list (car tokens_rhs)))) (rhs2 (rule-1-expi (cdr tokens_rhs))))
												;RHS1    = VALUE -> take valeue of rhs2 from expi
												(return-from  rule-11-15_27_28-expi-helper (rule-11-15_27_28-expi-exec lhs (list rhs1 rhs2) operator))
											)
										)
										(progn ; EXPI_1 = ?   -    find sublists and than use them to find EXPIs
											(let (  (count_op_rhs (count-op-plus tokens_rhs) ) ); EXPI  - GET THE LISTS
												( let ( ( tokens_new_rhs (get-sublist tokens_rhs count_op_rhs) ) )
													; (print "--------------ERROR PART----------------") 
													; (print tokens_new_rhs )
													; (print (cdr tokens_new_rhs))
													( let ((lhs (rule-3-expb tokens_lhs))  (rhs_1 ( rule-1-expi (car tokens_new_rhs))) (rhs_2 ( rule-1-expi (cadr tokens_new_rhs))) ) 
														(return-from  rule-11-15_27_28-expi-helper (rule-11-15_27_28-expi-exec lhs (list rhs_1 rhs_2) operator))
													)
												)
											)
										)
									)
								)
							)
						)
					)
				)
			)
		)
	);end of let for operator
)

(defun rule-2-explisti (tokens)
	(if (eq tokens nil)
		(return-from rule-2-explisti nil)
	)

	;its terminal alike thing
	(return-from rule-2-explisti (rule-32-36-explisti-helper tokens))
)
(defun rule-32-36-explisti-helper(tokens)
	;vals -
	; operator
	; tokens


	; 32  EXPLISTI : OP_OP KW_CONCAT EXPLISTI EXPLISTI OP_CP     - has to be enclosed both of them
	; 33           | OP_OP KW_APPEND EXPI EXPLISTI OP_CP         - can be enclosed
	; 34           | LISTVALUE  {OP VALUE VALUE VALUE CP} 								- reduces using written function for it
	; 35           | OP_OP KW_LIST VALUES OP_CP					- same as list value, only tokens are different
	; 36           | OP_OP KW_DISP LISTVALUE OP_CP	;expi argumented

	; 37  LISTVALUE : OP_OP VALUES OP_CC
	; 38            | OP_OP OP_CC
	; 39            | KW_NIL

	; 39            | KW_NIL
	(if (eq (car tokens) 'KW_NIL) 
		(return-from rule-32-36-explisti-helper nil)
	)
	;reduction of the parsed list
	(setq tokens (sandwich-list tokens) )
	; 38            | OP_OP OP_CC
	(if (eq tokens nil)	
		(return-from rule-32-36-explisti-helper nil)
	)

	( let 
		( ( operator (car tokens) ) ) 
		;if it not a desired operator go to rule-2
		(if (eq (is-rule-32-36-explisti-operator operator) nil)
			(return-from rule-32-36-explisti-helper nil)
		)
		; IT IS rule 11 - 15 everything from now on can produce either a good output or an error

		;reduce the token list now!
		(setq tokens ( cdr tokens ) )
		
		; (print operator)
		(if (eq operator 'KW_LIST);means return getvalues ofc
			(return-from rule-32-36-explisti-helper (get-list-values tokens))
		)
		;EXPI EXPLIsti
		;VALUE ( list ... )		is its form
		(if (eq operator 'KW_APPEND);means return getvalues ofc
			(progn;out operator is append
				;next parsed token has to be value
				(if ( eq ( car tokens ) 'VALUE )
					(progn
						(let ((lhs (get-value)));fetch out value
							;lhs = VALUE
							;rhs =? EXPILIST

							;shift the value
							(setq tokens (cdr tokens))
							; (print "RHS ?= EXPLISTI")
							; (print tokens)

							(let ((rhs (rule-2-explisti tokens)))
								; (print "WELCOME")
								(return-from rule-32-36-explisti-helper (rule-32-36-explisti-exec lhs rhs operator))
							)
						)
					)
					(progn ;EXPI EXPLISTI
						; (print "NOW FINDING EXPI")
						; (print tokens )
						(let (  (count_op (count-op tokens) ) )
							( let ( ( tokens_new (get-sublist tokens count_op) ) ) 
								( let ((tokens_lhs (car tokens_new)) (tokens_rhs (cadr tokens_new)) ) 
									; (print "LHS")
									; (print tokens_lhs)
									; (print "RHS")
									; (print tokens_rhs)	
									
									( let ((lhs (rule-1-expi tokens_lhs)) (rhs (rule-2-explisti tokens_rhs) )) 
										(if (not 
												(or 
													(eq lhs nil)(eq rhs nil) )
											)
											(return-from  rule-32-36-explisti-helper (rule-32-36-explisti-exec lhs rhs operator))
											(syntax-error "RHS or LHS of EXPI not valid")
										)
									)
								)
								
							)
						)
					)
				)
				return-from rule-32-36-explisti-helper (get-list-values tokens)
			)
		);KW_APPEND DONE

		;KW_CONCAT
		(if (eq operator 'KW_CONCAT);means return getvalues ofc
			(progn;out operator is append
				; (print "NOW FINDING EXPI")
				; (print tokens )
				(let (  (count_op (count-op tokens) ) )
					( let ( ( tokens_new (get-sublist tokens count_op) ) ) 
						( let ((tokens_lhs (car tokens_new)) (tokens_rhs (cadr tokens_new)) ) 
							; (print "LHS")
							; (print tokens_lhs)
							; (print "RHS")
							; (print tokens_rhs)	
							
							( let ((lhs (rule-2-explisti tokens_lhs)) (rhs (rule-2-explisti tokens_rhs) )) 
								(if (not 
										(or 
											(eq lhs nil)(eq rhs nil) )
									)
									(return-from  rule-32-36-explisti-helper (rule-32-36-explisti-exec lhs rhs operator))
									(syntax-error "RHS or LHS of EXPLISTI not valid")
								)
							)
						)
						
					)
				)
			)
		);KW_CONCAT DONE




		;EXPLISTI EXPLISTI

		; OP_OP KW_CONCAT EXPLISTI EXPLISTI OP_CP
		;OP_OP KW_APPEND EXPI EXPLISTI OP_CP
		

		; 4 cases
		; ID/VAL ID/VAL
		; EXPI ID/VAL
		; ID/VAL EXPI
		; EXPI EXPI

		; (setq lhs nil) ; left hand side
		; (setq rhs nil) ; right hand side
		;get next token
		(if (eq (is-ID-VAL (car tokens)) T )
			(progn
				( let 
					( ( lhs (rule-1-expi (list (car tokens) ) ) ) ) 
					;(setq lhs  )
					(setq tokens (cdr tokens))
					;ID/VAL + sth
					;rhs in tokens - check ID/VAL or (   -> neither -> syntax error
					(if (eq (is-ID-VAL (car tokens)) T )
						(progn; ID/VAL   ID/VAL
							; use local variables because of tail recursion
							( let 
								( ( rhs (rule-1-expi (list (car tokens) ) ) ) )
								; rhs = right hand side
								(setq tokens (cdr tokens))
								;after this sytax is ok if there is nothing left in the tokens list!
								(if (eq tokens nil);this means that the sytax is alright
									(progn 
										(return-from  rule-11-15_27_28-expi-helper (rule-11-15_27_28-expi-exec lhs rhs operator))
									)
									(progn;this means that there is problema and that there is a syntax error here because this can't be the case for our langauge and rule 11-15 is not satified as it has more that 2 arguments!
										(syntax-error "Too many arguments for for EXPI --- rule 11-15")							
									)
								)
							)
						)
						(progn;ID/VAL   EXPI   -> if not EXPI than sytax error
							;we need rhs
							;it can be EXPI only if enclosed
							;expansion of EXPI here
							(if 
								(eq (is-enclosed tokens) T)
								(progn
									; (print "LHS HERE IS ")
									; (print lhs)
									; (print "RHS HERE IS ")
									; (print rhs)
									; (print "LHS HERE IS ")
									; (print lhs)
									(setq rhs (rule-1-expi tokens));find value of the EXPI on the right handside
									(return-from  rule-11-15_27_28-expi-helper (rule-11-15_27_28-expi-exec lhs rhs operator))
								)
								(syntax-error "EXPI - OP_OP ID/VAL  ---NON ID/VAL non Enlosed--- OP_CP");otherwise it is a syntax error
							)
						)
					)
				)
			)
			(progn ;lhs != ID/VAL 
				;lhs ?= EXPI   -> tokens enclosed ! ()
				(if (eq (is-enclosed-lhs tokens) T)
					(progn ; lhs = EXPI
						; (print "TOKENS")
						; (print tokens)
						; lhs = EXPI
						; get the enclosed list 	->   form of: ( ... )   id/val/expi
						(let (  (count_op (count-op tokens) ) )
							( let ( ( tokens_new (get-sublist tokens count_op) ) ) 
								( let ((tokens_lhs (car tokens_new)) (tokens_rhs (cadr tokens_new)) ) 
									; (print "LHS")
									; (print tokens_lhs)
									; (print "RHS")
									; (print tokens_rhs)	
									
									( let ((lhs (rule-1-expi tokens_lhs)) (rhs (rule-1-expi tokens_rhs) )) 
										(if (not 
												(or 
													(eq lhs nil)(eq rhs nil) )
											)
											(return-from  rule-11-15_27_28-expi-helper (rule-11-15_27_28-expi-exec lhs rhs operator))
											(syntax-error "RHS or LHS of EXPI not valid")
										)
									)
								)
							)
						)
					)
					(progn
						;lhs != EXPI
						;lhs != ID/VAL
						;sytanx error!
						(syntax-error "lhs != EXPI --- lhs != ID/VAL --- in rule 11-15 of expi")
					)
				)
			)
		)
	);end of let for operator
)

(defun count-op (local_list)
	; (print "LOCAL LIST")
	; (print local_list)
	(return-from count-op (count-op_help local_list 0) )
)
(defun count-op_help (local_list count)
	(if (eq local_list nil)( return-from count-op_help count ) )
	(if (eq (car local_list) 'OP_CP) (return-from count-op_help count) )
	(if (eq (car local_list) 'OP_OP)
		(return-from count-op_help ( count-op_help (cdr local_list) (+ count 1) ) ) 
		(return-from count-op_help ( count-op_help (cdr local_list) count ) )
	)
)

(defun count-op-plus (local_list)
	(return-from count-op-plus (count-op-plus_help (cdr local_list) 1 1) )
)
(defun count-op-plus_help (local_list count result)
	(if (eq local_list nil)( syntax-error "COUNT-OP-PLUS_HELP Unbalanced parenthesis" ) )
	(if (eq (car local_list) 'OP_CP) 
		(progn
			(if (eq (+ count -1) 0)
				(return-from count-op-plus_help result))
			;otherwise ddecrese the count and result remains the same
			(return-from count-op-plus_help ( count-op-plus_help (cdr local_list) (+ count -1) result) )
		) 
	)
	(if (eq (car local_list) 'OP_OP)
		(return-from count-op-plus_help ( count-op-plus_help (cdr local_list) (+ count 1) (+ result 1) ) ) 
		(return-from count-op-plus_help ( count-op-plus_help (cdr local_list) count result) )
	)
)

(defun get-sublist (given_list count_op)
	(let ((new_list nil)) 
		; (print "IN GET SUBLIST - TOKENS PASSED -")
		; (print given_list)
		(setq new_list  (get-sublist-helper given_list new_list count_op))
		(return-from get-sublist new_list)
	)
)
;performs separation here
(defun get-sublist-helper (given_list new_list count_op)
	(if (eq given_list nil);if this happens it is a syntax error! unbalanced parenthesis
		(return-from get-sublist-helper (list new_list nil))
		;(syntax-error "Unbalanced parenthesis EXPI lhs")
	)
	(if (eq count_op 0)
		(return-from get-sublist-helper (list new_list given_list)))

	;otherwise add a new element
	(setq new_list (append new_list (list (car given_list))))
	(if (eq (car given_list) 'OP_CP)
		(setq count_op (- count_op 1)))
	
	(return-from get-sublist-helper (get-sublist-helper (cdr given_list) new_list count_op))
)


(defun is-t-f-val (operand)
	(if (or (eq operand 'true)(eq operand 'false))
		(return-from is-t-f-val T)
		(return-from is-t-f-val nil)
		)
)
(defun rule-23-26-expb-exec(lhs rhs operator)

	; (print "HERE HEY")

	; (print lhs)
	; 	(print rhs)

	; (print operator)
	; (print "OPERATOR")
	; (print operator)
	; (print lhs)
	; (print rhs)
	(if (not (or (eq operator 'KW_LESS) (eq operator 'KW_EQUAL)))
		(progn
			(if( not ( and (is-t-f-val lhs) (is-t-f-val rhs) ) )
				(progn;speical case for KW_NOT
					(if (not (and (eq rhs nil) (eq operator 'KW_NOT)(or (eq lhs 'true)(eq lhs 'false) ) ) )
						(syntax-error "EXPBI invalid value type")
					)
				)
			)
		)
	)
	;EXPI EXPI
	;EXBI EXBI
	(if ( numberp lhs )
		(progn
			(if (not (numberp rhs)) 
				(syntax-error "EXPI EXPBI parameter pair not allowed in EXPBI")
			)
		)
		(progn; else lhs is a value
			(if (not (symbolp rhs))
				(syntax-error "EXPI EXPBI parameter pair not allowed in EXPBI")
			)
		)
	)
	; (print "HEERERE")
	(case operator
		('KW_AND 
			(
			if( and ( eq lhs 'true) (eq rhs 'true) )
				(return-from rule-23-26-expb-exec 'true)
				(return-from rule-23-26-expb-exec 'false)
			)
		)
		('KW_OR 
			(
			if( or ( eq lhs 'true) ( eq rhs 'true) )
				(return-from rule-23-26-expb-exec 'true)
				(return-from rule-23-26-expb-exec 'false)
			)
		)
		('KW_EQUAL 
			(
				if( equal lhs rhs )
					(return-from rule-23-26-expb-exec 'true)
					(return-from rule-23-26-expb-exec 'false)
			)
		)
		('KW_NOT 
			(
				if( equal lhs 'true )
					(return-from rule-23-26-expb-exec 'false)
					(return-from rule-23-26-expb-exec 'true)
			)
		)
		('KW_EQUAL 
			(
				if( equal lhs rhs )
					(return-from rule-23-26-expb-exec 'true)
					(return-from rule-23-26-expb-exec 'false)
			)
		)
		('KW_LESS 
			(
				if( < lhs rhs )
					(return-from rule-23-26-expb-exec 'true)
					(return-from rule-23-26-expb-exec 'false)
			)
		)
	)
)

(defun rule-11-15_27_28-expi-exec(lhs rhs operator)
	; (print "EXECUTION")
	; (print lhs)
	; (print rhs)
	; (print operator)
	(case operator 
		('OP_PLUS (return-from rule-11-15_27_28-expi-exec (+ lhs rhs)))
		('OP_MINUS (return-from rule-11-15_27_28-expi-exec (- lhs rhs)))
		('OP_DIV (return-from rule-11-15_27_28-expi-exec (/ lhs rhs)))
		('OP_MULT (return-from rule-11-15_27_28-expi-exec (* lhs rhs)))
		('OP_DBLMULT (return-from rule-11-15_27_28-expi-exec (expt lhs rhs)))
		('KW_IF (return-from rule-11-15_27_28-expi-exec (if-exec lhs rhs)))
		('KW_FOR (return-from rule-11-15_27_28-expi-exec (for-exec lhs rhs)))
	)
)
(defun if-exec(lhs rhs)
	;lhs gotta be either 'true or 'false
	; (print "IF-EXEC")
	; (print rhs )
	(if (or( eq lhs 'true )(eq lhs 'false))
		(progn
			(if (eq lhs 'true)
				(return-from if-exec (car rhs))
				(return-from if-exec (cadr rhs))
			)
		)
		(syntax-error "IF --- first parameter has to be a boolean value")
	)
)
(defun for-exec(lhs rhs)
	;lhs gotta be either 'true or 'false
	; (print "for execution")
	(if ( > (length rhs) 1) 
		( syntax-error "FOR --- parameter is only EXPI")
	)
	(if (or( eq lhs 'true )(eq lhs 'false))
		(progn
			(if (eq lhs 'true)
				(return-from for-exec (car rhs))
				(return-from for-exec 'NIL_)
			)
		)
		(syntax-error "FOR --- first parameter has to be a boolean value")
	)
)
(defun is-rule-23-26-expb-operator (symb)
	(if( or
			(eq symb 'KW_OR)
			(eq symb 'KW_NOT)
			(eq symb 'KW_AND)
			(eq symb 'KW_EQUAL)
			(eq symb 'KW_EQUAL)
			(eq symb 'KW_LESS)
		)
		(return-from is-rule-23-26-expb-operator T)
		(return-from is-rule-23-26-expb-operator nil)
	)
)

(defun rule-32-36-explisti-exec(lhs rhs operator)
	(case operator 
		('KW_CONCAT (return-from rule-32-36-explisti-exec (append lhs rhs)))
		('KW_APPEND (return-from rule-32-36-explisti-exec (append rhs (list lhs))))
		; ('KW_DISP (return-from rule-11-15_27_28-expi-exec (/ lhs rhs)))
		; ('KW_LIST (return-from rule-11-15_27_28-expi-exec (* lhs rhs)))
	)
)
(defun is-rule-32-36-explisti-operator (symb)
	(if( or
			(eq symb 'KW_CONCAT)
			(eq symb 'KW_APPEND)
			(eq symb 'KW_DISP)
			(eq symb 'KW_LIST)
		)
		(return-from is-rule-32-36-explisti-operator T)
		(return-from is-rule-32-36-explisti-operator nil)
	)
)
(defun is-rule-11-15_27_28-expi-operator(symb)
	(if( or
			(eq symb 'OP_PLUS)
			(eq symb 'OP_MINUS)
			(eq symb 'OP_MULT)
			(eq symb 'OP_DBLMULT)
			(eq symb 'OP_DIV)
			(eq symb 'KW_IF)
			(eq symb 'KW_FOR)
		)
		(return-from is-rule-11-15_27_28-expi-operator T)
		(return-from is-rule-11-15_27_28-expi-operator nil)
	)
)
(defun rule-16(reduced_list)
	(if (eq (car reduced_list) 'IDENTIFIER) 
		(progn;if this is the case, reduce 
			;find the identifier
			;goes from left to right
			;reduce identifier list
			(setq variable_name (car identifier_list))
			;shift identifiers
			(setq identifier_list (cdr identifier_list))
			(return-from rule-16 (identifier-to-value variable_name))
			)
		)
	)


; 37  LISTVALUE : OP_OP VALUES OP_CC
; 38            | OP_OP OP_CC
; 39            | KW_NIL
(defun get-list-values (tokens) 
	(if (eq (car tokens) 'KW_NIL)
		(return-from get-list-values nil)
	)

	(if (> (length tokens) 1) 
		(progn
			(if (and (eq (car tokens) 'OP_OP) ( eq (cadr tokens) 'OP_CP) )
				(return-from get-list-values nil)
			)
		)
	)
	;otherwise get the values here
	; (setq tokens (sandwich-list tokens))
	(return-from get-list-values (get-values tokens))
	;gives values only in return
)

; reduction
; 40  VALUES : VALUES VALUE
; 41         | VALUE
(defun get-values(values)
	;list of values passed
	;check if each element is a list
	;if not there is an error here
	(check-all-values values);perform check and report the error if it exists here
	(setq new_reslist nil)
	;get all the values now

	; (loop for x in '(1 2 3)
 ;  do (print x))
 	; (print "VALUSE")
 	; (print values)
	(loop for i in values
    		do(progn
    			; (print "LIOOP")
    			(setq new_reslist (append new_reslist (list (get-value))) )

    		)
    )
    ; (print "VALUES FORMED LIST IS ")
    ; (print new_reslist)
    (return-from get-values new_reslist)
)

;report an error if not here
(defun check-all-values (cur_values)
	(if (eq cur_values nil)
		(return-from check-all-values T))

	(if (eq (car cur_values) 'VALUE)
		(return-from check-all-values (check-all-values (cdr cur_values)))
		(syntax-error "Values rule --- each element should be value")
	)
)

(defun get-value()
	(setq res (car value_list))
	(setq value_list (cdr value_list))
	(return-from get-value res)
	)

;RULE NUMBER 17 of EXPI IDENTIFIER
(defun identifier-to-value() 
	;(return-from identifier-to-value (identifier-to-value_helper variable_name symbol_table))
	(setq var_name (car identifier_list))
	(setq identifier_list (cdr identifier_list))

	; (let ((s_tbl symbol_table))
	; 	(print s_tbl)
	; 	(return-from identifier-to-value (identifier-to-value_helper var_name symbol_table))
	; )
	(dolist (id symbol_table) 
		(progn
			(if (equal var_name (car id))
				(return-from identifier-to-value (cadr id))
			)
		)
	)
	; if still here means an error
	(syntax-error "Undefined identifier")
)

(defun gpplexer(&optional filename)
		(if (not (eq filename nil))
			(setq in (open filename :if-does-not-exist nil))
		)
		(main)
	)
(defun main()
	(setq str (getLine))
	(if (eq str nil)
		(return-from main 0)
		)
	(setq ret_value (processLine str))
	(if ( or (string= ret_value "SYNTAX_ERROR")(string= ret_value "KW_EXIT") )
		(return-from main 1)
		(newLineAndMain))
	)

(defun sandwich-list(my_list)
	(setq list (cdr my_list))
	(setq list (reverse list))
	(setq list (cdr list))
	(setq list (reverse list))
	(return-from sandwich-list list)
	)
(defun newLineAndMain()
	
	; (terpri)
	;(print token_list)
	; (print value_list)

	;(print identifier_list)
	;interpret the list of tokens !!!!!
	; (print "HERE LPS")
	; (exit)

	;skip comment
	(if (eq (car token_list) 'COMMENT)
		(progn
			(print "Syntax ok")
			(terpri)
		)		
		
	)
	(if (eq (car (last token_list )) 'COMMENT )
		(progn
			(setq token_list (reverse token_list))
			(setq token_list (cdr token_list))
			(setq token_list (reverse token_list))
		)
;		(print (eq (last token_list) 'COMMENT  ))
	)

	(if (not ( eq token_list nil) ) 
				(progn
					( setq perse_res (gpp_interpreter 1))

					(if ( not (eq perse_res nil) )
						(progn
							; (print "Syntax ok")
							(terpri)
							(princ "Result ")
				   			(write res_interpreter)
				   			(terpri)
							))
				)
		)
	
		
	(setq identifier_list nil)
	(setq value_list nil)
	(setq token_list nil)
	; (terpri)
	(main)
	)
;starting the interpreter
(if (equal *args* nil)
  (gpplexer)
  (gpplexer (first *args*) ) )