%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
FILE *fptr;
void printToken( int token );
void manipulateToken( int token );
/*keywords*/
#define KW_AND 10
#define KW_OR 11
#define KW_NOT 12
#define KW_EQUAL 13
#define KW_LESS 14
#define KW_NIL 15
#define KW_LIST 16
#define KW_APPEND 17
#define KW_CONCAT 18
#define KW_SET 19
#define KW_DEFFUN 20
#define KW_FOR 21
#define KW_IF 22
#define KW_EXIT 23
#define KW_LOAD 24
#define KW_DISP 25
#define KW_TRUE 26
#define KW_FALSE 27
/*operators*/
#define OP_PLUS 28
#define OP_MINUS 29
#define OP_DIV 30
#define OP_MULT 31
#define OP_OP 32
#define OP_CP 33
#define OP_DBLMULT 34
#define OP_OC 35
#define OP_CC 36
#define OP_COMMA 37
/*comment*/
#define COMMENT 39
#define IDENTIFIER 40
#define VALUE 41

/*program logic necessities*/
#define ERROR 101

int token = -1;
int quotes_flag = 0;
int comment_flag = 0;
%}

%%
[\n] {comment_flag=0;}
[ \t] { /*just skip spaces and tabs*/}
and { manipulateToken(KW_AND);}
or { manipulateToken(KW_OR);}
not { manipulateToken(KW_NOT);}
equal { manipulateToken(KW_EQUAL);}
less { manipulateToken(KW_LESS);}
nil { manipulateToken(KW_NIL);}
list { manipulateToken(KW_LIST);}
append { manipulateToken(KW_APPEND);}
concat { manipulateToken(KW_CONCAT);}
set { manipulateToken(KW_SET);}
deffun { manipulateToken(KW_DEFFUN);}
for { manipulateToken(KW_FOR);}
if { manipulateToken(KW_IF);}
load { manipulateToken(KW_LOAD);}
disp { manipulateToken(KW_DISP);}
true { manipulateToken(KW_TRUE);}
false { manipulateToken(KW_FALSE);}
exit { manipulateToken(KW_EXIT); return 0;}
"+" { manipulateToken(OP_PLUS);}
"-" { manipulateToken(OP_MINUS);}
"/" { manipulateToken(OP_DIV);}
"**" { manipulateToken(OP_DBLMULT);}
"*" { manipulateToken(OP_MULT);}
"(" { manipulateToken(OP_OP);}
")" { manipulateToken(OP_CP);}
;{2} { manipulateToken(COMMENT); comment_flag = 1;}
[0-9]+ { manipulateToken(VALUE);}
[a-zA-Z]{1}[0-9a-zA-Z]* { manipulateToken(IDENTIFIER); }
"\"" {
	if(quotes_flag){
		manipulateToken(OP_CC);
		quotes_flag = 0;
	}
	else{
		manipulateToken(OP_OC);
		quotes_flag = 1;
	}

}
[0-9][a-zA-Z]+ {manipulateToken(ERROR);}
. {manipulateToken(ERROR);}
%%

void manipulateToken( int token ){
	if( !comment_flag )
		printToken(token);
	
}
void printToken( int token ){

	switch( token ){
		case KW_AND:
			fprintf(fptr, "%s", "KW_AND\n");
			break;
		case KW_OR:
			fprintf(fptr, "%s", "KW_OR\n");
			break;
		case KW_NOT:
			fprintf(fptr,"%s", "KW_NOT\n");
			break;
		case KW_EQUAL:
			fprintf(fptr,"%s", "KW_EQUAL\n");
			break;
		case KW_LESS:
			fprintf(fptr,"%s", "KW_LESS\n");
			break;
		case KW_NIL:
			fprintf(fptr,"%s", "KW_NIL\n");
			break;
		case KW_LIST:
			fprintf(fptr,"%s", "KW_LIST\n");
			break;
		case KW_APPEND:
			fprintf(fptr,"%s", "KW_APPEND\n");
			break;
		case KW_CONCAT:
			fprintf(fptr,"%s", "KW_CONCAT\n");
			break;
		case KW_SET:
			fprintf(fptr,"%s", "KW_SET\n");
			break;
		case KW_DEFFUN:
			fprintf(fptr,"%s", "KW_DEFFUN\n");
			break;
		case KW_FOR:
			fprintf(fptr,"%s", "KW_FOR\n");
			break;
		case KW_IF:
			fprintf(fptr,"%s", "KW_IF\n");
			break;
		case KW_EXIT:
			fprintf(fptr,"%s", "KW_EXIT\n");
			break;
		case KW_LOAD:
			fprintf(fptr,"%s", "KW_LOAD\n");
			break;
		case KW_DISP:
			fprintf(fptr,"%s", "KW_DISP\n");
			break;
		case KW_TRUE:
			fprintf(fptr,"%s", "KW_TRUE\n");
			break;
		case KW_FALSE:
			fprintf(fptr,"%s", "KW_FALSE\n");
			break;
		case OP_PLUS:
			fprintf(fptr,"%s", "OP_PLUS\n");
			break;
		case OP_MINUS:
			fprintf(fptr,"%s", "OP_MINUS\n");
			break;
		case OP_DIV:
			fprintf(fptr,"%s", "OP_DIV\n");
			break;
		case OP_MULT:
			fprintf(fptr,"%s", "OP_MULT\n");
			break;
		case OP_OP:
			fprintf(fptr,"%s", "OP_OP\n");
			break;
		case OP_CP:
			fprintf(fptr,"%s", "OP_CP\n");
			break;
		case OP_DBLMULT:
			fprintf(fptr,"%s", "OP_DBLMULT\n");
			break;
		case OP_OC:
			fprintf(fptr,"%s", "OP_OC\n");
			break;
		case OP_CC:
			fprintf(fptr,"%s", "OP_CC\n");
			break;
		case OP_COMMA:
			fprintf(fptr,"%s", "OP_COMMA\n");
			break;
		case COMMENT:
			fprintf(fptr,"%s", "COMMENT\n");
			break;
		case IDENTIFIER:
			fprintf(fptr,"%s", "IDENTIFIER\n");
			break;
		case VALUE:
			fprintf(fptr,"%s", "VALUE\n");
			break;
		case ERROR:
			fprintf(fptr, "%s", "SYTANX_ERROR");
			fprintf(fptr," %s cannot be tokenized\n", yytext);
			exit(1);
	}
}

int yywrap(){}

int main(int argc, char* argv[]){
	fptr = stdout;
	if(argc == 2)
		yyin = fopen(argv[1], "r");
	else
		yyin = stdin;
	yylex();

	fclose(yyin);
	return 0;
}