%{
	//header functions
	#include<stdio.h>
	#include<string.h>
	#include<stdlib.h>
	#include <math.h>
	#include "1907037.tab.h"
	extern char* strdup(const char*);
	void yyerror(char *s);
%}

Variable	[A-Z]+[a-z]*[0-9]*[a-z]*[A-Z]+	
Include		"Sornaly"[ ][a-z]+.[h]
Digit		[-]?[0-9][0-9]*([.][0-9]*)?
Comment		[ ]*!!.*!![ ]*	
String		["].*["]


%%	

JATRA 		return(JATRA);
number		return(Int);
floating	return(Float);
line		return(Char);
SURU		return(SURU);
SES		return(SES);
=> 		return(ASSIGN);
ADD	 	return(ADD);
SUB	 	return(SUB);
MUL	 	return(MUL);
DIV	 	return(DIV);
POW	 	return(POW);
~	 	return(EQL);
SHOW		return(SHOW);
SIN		return(SIN);
COS		return(COS);
TAN		return(TAN);
LOG10		return(LOG10);
LOG		return(LOG);
IF		return(IF);
ELSEIF		return(ELSEIF);
ELSE		return(ELSE);
LESSER		return(LESSER);
GREATER		return(GREATER);
GorE 		return(GorE);
LorE 		return(LorE);
MIN 		return(MINN);
MAX		return(MAX);
GCD		return(GCD);
FOR 		return(FOR);
WHILE 		return(WHILE);
Include		return(Include);
Void		return(Void);

{Include}	printf("This is an associative library.\n");
{Variable}	{yylval.stringValue = strdup(yytext); return(ID);}
{String}	{
			yytext++;
			int l=strlen(yytext);
            		yylval.stringValue=yytext;
            		yylval.stringValue[l-1]='\0';
	 		return(CHARACTER);
}

{Digit}		{	
			int length = strlen(yytext);
			int i = 0;
			while(i<length){
			if(yytext[i]=='.')
			{
				yylval.floatValue = atof(yytext); 
				return(NUM);
			}
				i++;
			}
			yylval.floatValue = atoi(yytext); 
			return(NUM);
		}


{Comment} 	{printf("\nThis is a  comment.\n");}
[,;{}()-><@]	{yylval.stringValue = strdup(yytext); return *yytext;}

[\n]		{}


%%

int yywrap(void){
	return 1;
}
