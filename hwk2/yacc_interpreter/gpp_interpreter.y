%{
	#include <stdio.h>
	#include <string.h>
    #include "gpp_interpreter.h"
	int yylex();
	void yyerror (char *s);
	extern FILE *yyin;
%}

/*keywords*/
%token KW_AND
%token KW_OR
%token KW_NOT
%token KW_EQUAL
%token KW_LESS
%token KW_NIL
%token KW_LIST
%token KW_APPEND
%token KW_CONCAT
%token KW_SET
%token KW_FOR
%token KW_IF
%token KW_EXIT
%token KW_LOAD
%token KW_DISP
%token KW_TRUE
%token KW_DEFFUN
%token KW_FALSE
%token ERROR

/*operators*/
%token OP_PLUS
%token OP_MINUS
%token OP_DIV
%token OP_MULT
%token OP_OP
%token OP_CP
%token OP_OC
%token OP_CC
%token OP_DBLMULT
%token COMMENT


/**/
%union{
	int val;
	int* val_list;
	char identifier[12];
}

%token <val> VALUE
%token <identifier> IDENTIFIER

%type <val> EXPI
%type <val> EXPB
/*%type <val> INPUT*/

%type <val_list> VALUES
%type <val_list> EXPLISTI
%type <val_list> LISTVALUE

%start START
%%

START	: EXPI {printf("Syntax OK.\nResult %d\n",$1);}
		| EXPLISTI {printf("Syntax OK.\nResult "); print_list($1);}
		| EXPB {printf("Syntax OK.\nResult %d\n",$1);}
		| START EXPI {printf("Syntax OK.\nResult %d\n",$2); }
		| START EXPLISTI {printf("Syntax OK.\nResult "); print_list($2);}
		| START EXPB {printf("Syntax OK.\nResult %d\n",$2);}
		| COMMENT {printf("COMMENT\n");}
		| START COMMENT {printf("COMMENT\n");}
		| OP_OP KW_EXIT OP_CP {freeList(); exit(0);}
		| START OP_OP KW_EXIT OP_CP { freeList(); exit(0); }
		;


EXPI 	: OP_OP OP_PLUS EXPI EXPI OP_CP {$$ = $3 + $4; }
		| OP_OP OP_MINUS EXPI EXPI OP_CP {$$ = $3 - $4;}
		| OP_OP OP_MULT EXPI EXPI OP_CP {$$ = $3*$4; }
		| OP_OP OP_DBLMULT EXPI EXPI OP_CP {$$ = pow_gpp($3 , $4);}
		| OP_OP OP_DIV EXPI EXPI OP_CP {$$ = $3 / $4; }

		| IDENTIFIER {/*get value from our list!*/; $$ = getIdentifier($1);}
		| VALUE {$$=$1;}
		
		/*| IDENTIFIER EXPLISTI {$$ = EXPLISTI;} this can't be a rule, i didn't understand this maybe OP SET IDENTIFIER EXPILISTI CP but such rule isn't written anywhere*/
		/*later on after EXPB defined following were also added within the .pdf*/
		| OP_OP KW_SET IDENTIFIER EXPI OP_CP {$$ = $4; ;set_element($3, $4);/*save in table(O(1))/list(O(n)) in our case*/; }
		

		| OP_OP KW_IF EXPB EXPI OP_CP {$$ = (0 == $3) ? 0: $4;}
		| OP_OP KW_FOR EXPB EXPI OP_CP { $$ = (0 == $3) ? 0 : $4; }
		| OP_OP KW_IF EXPB EXPI EXPI OP_CP {$$ = (1 == $3) ? $4: $5;}/*branching the if statement here! if - else where first EXPI is the first block and the second EXPI is the second block*/
    	| OP_OP KW_DISP EXPI OP_CP { $$ = $3; } 

		/*| OP_OP KW_DEFFUN IDLIST EXPLISTI OP_CP  --- I am skipping this rule since IDLIST was not provided in lexer in the previous homework. It wasn't mentioned anywhere*/
		/*| OP_OP IDENTIFIER EXPLISTI OP_CP  --- I am skipping this one also because if I can't define a function of course I can't also call it*/
		;


EXPB	: OP_OP KW_AND EXPB EXPB OP_CP {$$ = $3 && $4;}
		| OP_OP KW_OR EXPB EXPB OP_CP {$$ = $3 || $4;}
		| OP_OP KW_NOT EXPB OP_CP {$$ = !($3);}
		| OP_OP KW_EQUAL EXPB EXPB OP_CP {$$ = ($3 == $4);}
		| OP_OP KW_EQUAL EXPI EXPI OP_CP {$$ = ($3 == $4);}
		| OP_OP KW_LESS EXPI EXPI OP_CP { $$ = ($3 < $4);} 
   		| KW_TRUE  { $$ = 1; }   /* true */
    	| KW_FALSE   { $$ = 0;}/* false */
    	| OP_CP KW_DISP EXPB OP_CP { {$3 ? "T":"NIL"; } $$ = $3; $$ = $3; }
		;


EXPLISTI 	: OP_OP KW_CONCAT EXPLISTI EXPLISTI OP_CP {$$ = concatenate_lists($3, $4); }
			| OP_OP KW_APPEND EXPI EXPLISTI OP_CP { $$ = append_list($4, $3); }
			| LISTVALUE {$$ = $1;}
			| OP_OP KW_LIST VALUES OP_CP {$$ = $3;}
			| OP_OP KW_DISP LISTVALUE OP_CP { $$=$3;}
			;

LISTVALUE	: OP_OP VALUES OP_CC {$$ = $2; }
			| OP_OP OP_CC {$$ = NULL;}
			| KW_NIL { $$ = NULL;}
			;

/*VALUE -> values IntegerValue(VALUE) | IntegerValue(VALUE) (int .l i recognize the integer toekn as VALUE */

VALUES 	: VALUES VALUE { $$ = append_list($1, $2); }
		| VALUE	{$$ = NULL; $$ = append_list($$, $1); }
		;


%%

void yyerror(char *s) {
    printf( "AN ERROR OCCURED %s\n", s );
    //return 1;
}
int main(int argc, char** argv){
	
	FILE* fp = NULL;
	printf("Welcome to G++ parser:\n");
	if( argc > 1 ){
		fp = fopen(argv[1], "r");
		if( fp == NULL ){
			printf("Could not open %s file with this filename\n", argv[1]);
			return 1;
		}
		yyin = fp;
	}
	return yyparse();
		
}