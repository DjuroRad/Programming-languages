%{
#include "y.tab.h"
extern YYSTYPE yylval;

/**
void printToken( int token );
int manipulateToken();
**/
int token = -1;
int quotes_flag = 0;
int comment_flag = 0;
%}

%%
[\n\f\r\t ] {} /*skip*/
;;(.*) {return COMMENT;}	/*comment*/
and { return KW_AND;}
or { return KW_OR;}
not { return KW_NOT;}
equal { return KW_EQUAL;}
less { return KW_LESS;}
nil { return KW_NIL;}
list { return KW_LIST;}
append { return KW_APPEND;}
concat { return KW_CONCAT;}
set { return KW_SET;}
deffun { return KW_DEFFUN;}
for { return KW_FOR;}
if { return KW_IF;}
load { return KW_LOAD;}
disp { return KW_DISP;}
true { return KW_TRUE;}
false { return KW_FALSE;}
exit { return KW_EXIT; return 0;}
\+ { return OP_PLUS;}
\- { return OP_MINUS;}
\/ { return OP_DIV;}
\*\* { return OP_DBLMULT;}
\* { return OP_MULT;}
\( { return OP_OP;}
\) { /*ECHO; printf(" - echoing \n");*/return OP_CP;}
[0-9]+ { /*ECHO; printf(" - echoing \n");*/ yylval.val = atoi(yytext); return VALUE;}
[a-zA-Z]{1}[0-9a-zA-Z]* { strcpy(yylval.identifier, yytext); return IDENTIFIER; }
\" {
	if(quotes_flag){
		return OP_CC;
		quotes_flag = 0;
	}
	else{
		return OP_OC;
		quotes_flag = 1;
	}

}
[0-9][a-zA-Z]+ {/*printf("YYTEXT: %s\n", yytext);*/return ERROR;}
. {/*printf("YYTEXT: %s\n", yytext);*/ return ERROR;}
%%

int yywrap(){}
/**
int manipulateToken(){
	if( !comment_flag )
		return token;
}
**/