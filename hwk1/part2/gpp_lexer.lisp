; lexer for g++ langauge
( setq token -1 )
( setq quotes_flag 0 )
( setq comment_flag 0 )
( setq double_mult_flag 0 )
(setq in nil)

(defun gppinterpreter(&optional filename)
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

(defun getLine()
		(if (eq in nil)
		(return-from getLine (read-line))
		(return-from getLine (read-line in nil)))
 )

(defun newLineAndMain()
	(if(eq in nil)
		(terpri))
	(main)
	)
(defun processLine(str)
	(setq str (concatenate 'string str (list #\newline)))
	;(setq str_list (string-to-list str))
	;(loop for item in str_list
	;	do(print (char (list-to-string item) 1))
	;	)
	(setq n (length str))
	(setq curr_token "")
	(loop for i from 1 to n 
		do(
					setq curr_token 
			( preProcessToken curr_token str i ))
		do(
			if(or (string= curr_token "SYNTAX_ERROR")(string= curr_token "KW_EXIT") )( return-from processLine curr_token )
			)
	))

(defun preProcessToken( token str i )

	(setq curr_char (char str (- i 1)))

	;if dubl mult we should skip it!!!
	(if (= double_mult_flag 1) (return-from preProcessToken (retAndSetMultFlag)))

	(if 
		(not
			(or 
				(char= curr_char #\newline )
				(char= curr_char #\space)
				(char= curr_char #\tab)
				;(= double_mult_flag 1)
				 )
				 )

				(setq token (concatenate 'string token (list curr_char)))
				)
	;(print token)
	;now we have a new token in here that we should check
	;for newline we both set the flag and return the empty token
	(if(char= curr_char #\newline)(setq comment_flag 0))

	;token might be empty, if so return it
	(if (string= token "")(return-from preProcessToken token))
	;if the comment flag is on we just pass the empty token, otherwise we analyze the token
	(if (= comment_flag 0) (setq token ( preProcessLogic token str i ))(setq token ""))
	
	;(print res)
	(return-from preProcessToken token)
 )

(defun retAndSetMultFlag()
	(setq double_mult_flag 0)
	(return-from retAndSetMultFlag "")
	)

(defun preProcessLogic(token str i)
	;check everything
	;(print token)

	;token - current state
	;str - input
	;i - next
	(setq check_all_res (DFA token str i) )

	;if nothing is found and there was no SYNTAX_ERROR and nothing was found -> continue building the token
	(if (string= check_all_res "") (return-from preProcessLogic token))
	;if there is an SYNTAX_ERROR -> send the SYNTAX_ERROR!
	(if (string= check_all_res "SYNTAX_ERROR")(return-from preProcessLogic "SYNTAX_ERROR"))
	(if (string= check_all_res "KW_EXIT")(return-from preProcessLogic "KW_EXIT"))
	;if there is something that was printed continue by sending the empty string!!!
	(return-from preProcessLogic "")
)

;str - input   of DFA 
;token - curr_state    of DFA
;i - next    of DFA
(defun DFA( token str i )
	;before checking what the next operator is we first check if we are currently holding a operator here
	(setq check (IS_OP token str i))
	;(print check)
	(if 
		(not 
			(string= check ""));means it is an operator -> reutrn and make token "" !
		(return-from DFA (printRetToken check))
		)
	;(print check)

	;special case for double mult
	;(if (string= check "*") (return-from DFA "") )
	;(if(not (string= check ""))(print check))
	;(if(not (string= check ""))(setq token ""))

	;check if next char is operator, space, newline or tab than
	(setq next_operator (checkNextOperatorNewlineSpaceTab str i))

	;if they are not neighr operator, space newline nor tab than return token itself to be constructed even more
	;(print next_operator)
	(if (= next_operator 0)( return-from DFA ""))

	;check for keyword first
	(setq check (IS_KW token))

	(if (not (string= check "")) (return-from DFA (printRetToken check)))
	;take the token and check if it is vlaue identifier or an SYNTAX_ERROR
	;it will after this return SYNTAX_ERROR IDENTIFIER OR VALUE
	
	(setq identifier_value_error_check (DFA_VAL_ID token))
	(if (string= identifier_value_error_check "SYNTAX_ERROR")
		(progn
			(terpri)
			(format t identifier_value_error_check)
			(format t " ")
			(format t token)
			(format t " ")
			(format t "cannot be tokenized")
			)
		(printRetToken identifier_value_error_check)
		)

	;return the value
	(return-from DFA identifier_value_error_check)

	;(return-from DFA (printRetToken (DFA_VAL_ID token)))

;		(print token)
	;(if (string= check "SYNTAX_ERROR")(return-from DFA (printRetToken check)))
	;if it is operator take the token and see if it is an identifier, value or SYNTAX_ERROR
	;if token is an empty string check for operator! ?
	;(if (string= check "")(setq check (IS_OP token str i)))
	;(if(not (string= check ""))(print check))
	;(if(not (string= check ""))(setq token ""))

	;(return-from DFA token)
 )
(defun DFA_VAL_ID(token)
	(setq value 0)
	(setq indentifier 0)
	;go char by char

	;set current DFA input and check it in this state
	(setq first_char (char token 0))
	;if first char is number -> all of them have to be number
	(if(= (isNumber first_char) 1)( setq value (checkIfNumber token) ))
	;if first char is letter -> all other either letter or digit
	(if(= (isLetter first_char) 1)( setq indentifier (checkIfLetterOrDigit token) ))
	;if SYNTAX_ERROR both value and identifiter are 0
	(if (= value 1)( return-from DFA_VAL_ID "VALUE"))
	(if (= indentifier 1)( return-from DFA_VAL_ID "IDENTIFIER"))
	(return-from DFA_VAL_ID "SYNTAX_ERROR")
	;otherwise it is an SYNTAX_ERROR
	)
(defun isLetter(first_char)
	(if (or (and (char>= first_char #\a )(char<= first_char #\z))(and (char>= first_char #\A )(char<= first_char #\Z)) ) 
		(return-from isLetter 1)
		(return-from isLetter 0) )
	)
(defun checkIfLetterOrDigit(token)
	(loop for i from 0 to (- (length token) 1)
		do(if
			( not 
				(or 
					(= (isNumber (char token i) ) 1)
					(= (isLetter (char token i) ) 1) ) )
			(return-from checkIfLetterOrDigit 0)
			)
		)
	(return-from checkIfLetterOrDigit 1)
	)
(defun isNumber(first_char)
	(if (and (char>= first_char #\0 )(char<= first_char #\9)) 
		(return-from isNumber 1)
		(return-from isNumber 0) )
	)
(defun checkIfNumber(token)
	(loop for i from 0 to (- (length token) 1)
		do(if( not (= (isNumber (char token i) ) 1) ) (return-from checkIfNumber 0) )
		)
	(return-from checkIfNumber 1)
	)
(defun printRetToken(token)
	(print (read-from-string token))
	(return-from printRetToken token)
	)
(defun checkNextOperatorNewlineSpaceTab(str i)
	(if (and (< i (length str)) (= (checkNextOperatorHelper str i) 1))( return-from checkNextOperatorNewlineSpaceTab 1) )
	(return-from checkNextOperatorNewlineSpaceTab 0)
)

(defun checkNextOperatorHelper(str i)
	(setq curr_char (char str i))
	(cond
    ( (char= curr_char #\+)  (return-from checkNextOperatorHelper 1) )
    ( (char= curr_char #\-)  (return-from checkNextOperatorHelper 1) )
    ( (char= curr_char #\/) (return-from checkNextOperatorHelper  1) )
    ( (char= curr_char #\*) (return-from checkNextOperatorHelper 1 ) )
    ( (char= curr_char #\( )  (return-from checkNextOperatorHelper 1) )
    ( (char= curr_char #\) )  (return-from checkNextOperatorHelper 1) )
    ( (char= curr_char #\" ) (return-from checkNextOperatorHelper 1 ) )
    ( (char= curr_char #\,)  (return-from checkNextOperatorHelper 1) )
    ( (char= curr_char #\;) (return-from checkNextOperatorHelper 1 ) )
    ( (char= curr_char #\newline) (return-from checkNextOperatorHelper 1 ) )
    ( (char= curr_char #\tab) (return-from checkNextOperatorHelper 1 ) )
    ( (char= curr_char #\space) (return-from checkNextOperatorHelper 1 ) )
    (t (return-from checkNextOperatorHelper 0))
    )
)
(defun IS_KW(token)
  (cond
    ( (string= token "and") (return-from IS_KW "KW_AND") )
    ( (string= token "or") (return-from IS_KW "KW_OR") )
    ( (string= token "not") (return-from IS_KW "KW_NOT") )
    ( (string= token "equal") (return-from IS_KW "KW_EQUAL") )
    ( (string= token "less") (return-from IS_KW "KW_LESS") )
    ( (string= token "nil") (return-from IS_KW "KW_NIL") )
    ( (string= token "list") (return-from IS_KW "KW_LIST") )
    ( (string= token "append") (return-from IS_KW "KW_APPEND") )
    ( (string= token "concat") (return-from IS_KW "KW_CONCAT") )
    ( (string= token "set") (return-from IS_KW "KW_SET") )
    ( (string= token "deffun") (return-from IS_KW "KW_DEFFUN") )
    ( (string= token "for") (return-from IS_KW "KW_FOR") )
    ( (string= token "if") (return-from IS_KW "KW_IF") )
    ( (string= token "exit") (return-from IS_KW "KW_EXIT") )
    ( (string= token "load") (return-from IS_KW "KW_LOAD") )
    ( (string= token "disp") (return-from IS_KW "KW_DISP") )
    ( (string= token "true") (return-from IS_KW "KW_TRUE") )
    (t (return-from IS_KW ""))
  )
)
(defun IS_OP(token str i)
  (cond
    ( (string= token "+")  (return-from IS_OP "OP_PLUS") )
    ( (string= token "-")  (return-from IS_OP "OP_MINUS") )
    ( (string= token "/") (return-from IS_OP  "OP_DIV") )
    ( (string= token "*") (return-from IS_OP (multProcess token str i)) )
    ;( (string= token "**") (return-from IS_OP (multProcess token str i)) )
    ( (string= token "(")  (return-from IS_OP "OP_OP") )
    ( (string= token ")")  (return-from IS_OP "OP_CP") )
    ( (string= token "\"") (return-from IS_OP (quoteOperator)) )
    ( (string= token ",")  (return-from IS_OP "OP_COMMA") )
    ( (string= token ";") (return-from IS_OP (commentOperator str i)) )
    (t (return-from IS_OP ""))
    )
  )
(defun multProcess(token str i )
	;(return-from multProcess (multProcessHelper token str i) )
	( if (not (and ( < i (length str )) (char= #\* (char str i)) ) )
		( return-from multProcess "OP_MULT")
		( return-from multProcess (dblmulthelper))	
	)
)

(defun dblmulthelper()
	(setq double_mult_flag 1)
	(return-from dblmulthelper "OP_DBLMULT")
	)
(defun quoteOperator() 
	(if (= quotes_flag 0)( 
			return-from quoteOperator (opOC)
		)( 
			return-from quoteOperator (opCC)	
		)
	)
)
(defun opOC()
	(setq quotes_flag 1)
	(return-from opOC "OP_OC")
)
(defun opCC()
	(setq quotes_flag 0)
	(return-from opCC "OP_CC")
)

(defun commentOperator(str i)

	(if (and (< i (length str)) (char= #\; (char str i)))
		(return-from commentOperator (commentOP))
	)
	(return-from commentOperator "")
)
(defun commentOP()
	(setq comment_flag 1)
	(return-from commentOP "COMMENT")
)

;starting the interpreter
(if (equal *args* nil)
  (gppinterpreter)
  (gppinterpreter (first *args*) ) )