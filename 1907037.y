%{ 	
	//header functions	
	#include<stdio.h>
	#include<math.h>
	#include<string.h>
	int yylex(void);
	void yyerror(char *s);


	// number of variables added
	int no_var = 0; 
	int no_stats=0;


	// structure to store variables
	struct DataType {
    		int Type;
    		char  var_name[50];
    		int intValue;
    		float floatValue;
    		char *stringValue;
	} array_var[100];

	
	//search for used name
	int searchVar(char name[50]){ 
		int i;
		for(i=0; i<no_var ; i++){
			if(strcmp(array_var[i].var_name, name) == 0){
				return 1; // Found
			}
		}
		return 0; // Not found
	}


	//setting data_type
	void setDataType(int type){
		int i;
		for(i=0; i<no_var ; i++){
			if(array_var[i].Type == -1){
				array_var[i].Type = type;
			}
		}
	}
	

	// Search the index of any variable 
	int get_var_ind(char name[50]){
		int i;
		for(i=0; i<no_var ; i++){
			if(strcmp(array_var[i]. var_name, name) == 0){
				return i;
			}
		}
		return -1;
	}

%}

%union{
	int ival;
	double floatValue;
	char* stringValue;
}

%token JATRA Int Float Char Bool SURU SES ID NUM ASSIGN CHARACTER ADD SUB MUL DIV POW EQL SHOW SIN COS TAN LOG10 LOG LorE GorE ELSEIF GREATER LESSER ELSE IF MINN MAX GCD FOR WHILE Include
%left ADD SUB
%left DIV MUL

%type<floatValue>JATRA  Int Float Char Bool SURU SES TYPE NUM  t assignment expression ASSIGN ADD SUB MUL DIV POW EQL SHOW SIN COS TAN LOG10 LOG mathematics sinFunct cosFunct tanFunct logFunct log10Funct ifCondition LorE GorE GREATER LESSER then ELSE IF ELSEIF MINN MAX GCD minFunct maxFunct GcdFunct FOR for_code WHILE while_code Include 

%type<stringValue> ID1 ID CHARACTER

%%

                 8
program:JATRA SURU    statement SES	{printf("\nTotal variables: %d\n",no_var);printf("\nTotal statements: %d\n",no_stats);printf("\ncomplete code!\n");}
		;

statement: 
        |declaration statement
        |assignment statement
        |expression statement
        |print_code statement
		|mathematics statement
		|ifCondition statement
		|for_code statement
		|while_code statement
		|include statement
		|Void statement
        ;

//DECLARATION section
declaration: TYPE ID1 DEAD{
    setDataType($1);

}   
    ;

include: Include {printf("\nthis is not right.");}

TYPE: Int   {$$ = 1; printf("\nDataType:- INT\n");}
        |Float   {$$ = 2; printf("\nDataType:- FLOAT\n");}
        |Char   {$$ = 3; printf("\nDataType:- CHAR\n");}
        ;

ID1: ID1 ',' ID {
	if(searchVar($3)==0){
		printf("\nValid declaration");
		strcpy(array_var[no_var]. var_name, $3);
		printf("\nVariable name: %s\n", $3);
		array_var[no_var].Type =  -1;
		no_var = no_var + 1;
	}
	else{
		printf("\nVariable is already used");
	}
} 
	| ID {
	if(searchVar($1)==0){
		printf("\nValid declaration");
		strcpy(array_var[no_var]. var_name, $1);
		printf("\nVariable name: %s\n", $1);
		array_var[no_var].Type =  -1;
		no_var = no_var + 1;
	}
	else{
		printf("\nVariable is already used\n");
	}
	strcpy($$, $1);
	}
	;

DEAD:	 '@'	{no_stats++;printf("\nThis is a statement.");}
	;

assignment: ID ASSIGN expression DEAD {
	$$ = $3;
	if(searchVar($1)==1){
		int i = get_var_ind($1);
		if(array_var[i].Type==1){
			array_var[i].intValue = $3;
			printf("\nVariable value: %d (INT)", array_var[i].intValue);
		}
		else if(array_var[i].Type==2){
			array_var[i].floatValue = (float)$3;
			printf("\nVariable value: %2f (FLOAT)", array_var[i].floatValue);
		}
		else
		{printf("\nIt maybe string .");}
	}
}
	|ID ASSIGN CHARACTER DEAD  {
		//$$ = $3;
	    if(searchVar($1)==1){
		int i = get_var_ind($1);
		if(array_var[i].Type==3){
			array_var[i].stringValue = $3;
			printf("\nVariable value: %s (CHAR)", array_var[i].stringValue);
			}
		}
	}
	;

expression: NUM					{ $$ = $1; 	}
        
	|  ADD '(' expression ',' expression ')'    {$$ = $3 + $5;}

	|  SUB '('expression ',' expression')'	{ $$ = $3 - $5; }

	|  MUL '('expression ',' expression')'	{ $$ = $3 * $5; }

	|  DIV '('expression ',' expression')'	{ if($5){
				     					$$ = $3 / $5;
				  					}
				  					else{
										$$ = 0;
										printf("\ndivision by zero error\t");
				  					} 	
				    			}
	| expression POW expression	{ $$ = pow($1 , $3);}
	| '(' expression ')'		{ $$ = $2;}
    	|expression EQL expression	{ $$ = $1 == $3; } 
	| expression LESSER expression	{ $$ = $1 < $3; }
	
	| expression GREATER expression	{ $$ = $1 > $3; }
	| expression GorE expression	{ $$ = $1 >= $3; }
	
	| expression LorE expression	{ $$ = $1 <= $3; }
	| mathematics ; 
	| t {$$=$1};

t: '(' expression ')' {$$ = $2;}
	| ID { 
	int index = get_var_ind($1);
	if(index == -1)
	{
		yyerror("VARIABLE DOESN'T EXIST\n");
	}
	else
	{
		
		if(array_var[index].Type == 1)
		{
			$$ = array_var[index].intValue;
		}
		else if(array_var[index].Type == 2)
		{
			$$ = array_var[index].floatValue;
		}
	}
    }
	;


print_code: SHOW '(' ID ')' DEAD {
	int i = get_var_ind($3);
	if(array_var[i].Type == 1){
		printf("\nVariable name:---> %s, Value:---> %d\n\n", array_var[i]. var_name, array_var[i].intValue);
	}
	else if(array_var[i].Type == 2){
		printf("\nVariable name:---> %s, Value:---> %f\n\n", array_var[i]. var_name, array_var[i].floatValue);
	}
	else{
		printf("\nVariable name:---> %s, Value:---> %s\n\n", array_var[i]. var_name, array_var[i].stringValue); 	}
	}

	
	;
	
mathematics:
       sinFunct 
	| cosFunct 
	| tanFunct 
	| logFunct 
	| log10Funct 
	| minFunct
	| maxFunct
	| GcdFunct
	;
minFunct:MINN '(' expression ',' expression ')'{
        if($3>$5)
        {
            $$ = $5;
        }
        else
        {
            $$ = $3;
        }
        printf("%d\n",$$);
    }
maxFunct:MAX '(' expression ',' expression ')'{
        if($3>$5)
        {
            $$ = $3;
        }
        else
        {
            $$ = $5;
        }
       
    }
GcdFunct:GCD '(' expression ',' expression ')'{
       int c;
       int a = $3;
       int b = $5;
       if(a>b)
       {
           int temp = a;
           a=b;
           b=a;
       }
       while(a!=0)
       {
           int temp = b%a;
           b=a;
           a=temp;
       }
       $$ = b;
    }
	;
sinFunct: SIN '(' expression ')'  {
	printf("\nValue of Sin(%lf) is %lf\n\n",$3,sin($3*3.1416/180)); 
	$$=sin($3*3.1416/180);
}
	;
	

cosFunct: COS '(' expression ')'  {
	printf("\nValue of Cos(%lf) is %lf\n\n",$3,cos($3*3.1416/180)); 
	$$=cos($3*3.1416/180);
}
	;
tanFunct: TAN '(' expression ')'  {
	printf("\nValue of Tan(%lf) is %lf\n\n",$3,tan($3*3.1416/180)); 
	$$=tan($3*3.1416/180);
}
	;


log10Funct: LOG10 '(' expression ')'  {
	printf("Value of Log10(%lf) is %lf\n\n",$3,(log($3*1.0)/log(10.0))); 
	$$=(log($3*1.0)/log(10.0));
}
	;
logFunct: LOG '(' expression ')'  {
	printf("Value of Log(%lf) is %lf\n\n",$3,(log($3))); 
	$$=(log($3));
}	
;

ifCondition: IF '(' expression ')' '{' then '}' {
			printf("\nifCondition: Condition is %lf\n", $3);
			if($3>=1)
			printf("\nCondition is true. Output: %lf\n", $6);
			}
			|IF '('expression')' '{' then '}' ELSE '{' then '}' {
			printf("\nifCondition: Condition is %lf\n", $3);
			if($3>=1)
			printf("\nCondition is true. Output: %lf\n",   $6);
			else
			printf("%d \n",(int)$10);
			}
			|IF '('expression')' '{' then '}' ELSEIF '('expression')' '{' then '}' ELSE '{' then '}' {
			if($3>1)
			printf("%d \n",(int)$6);
			else if($10==1)
			printf("%d \n",(int)$13);

			else
			printf("%d \n",(int)$17);
			}
			;
then:	NUM					    { $$ = $1; 	}
        
	|  ADD '(' expression ',' expression ')'    {$$ = $3 + $5;}

	|  SUB '('expression ',' expression')'	{ $$ = $3 - $5; }

	|  MUL '('expression ',' expression')'	{ $$ = $3 * $5; }

	|  DIV '('expression ',' expression')'	{ if($5){
				     					$$ = $3 / $5;
				  					}
				  					else{
										$$ = 0;
										printf("\ndivision by zero error\t");
				  					} 	
				    			}
	| expression POW expression	{ $$ = pow($1 , $3);}
	;

for_code: 
	FOR '('NUM '@' NUM '@' NUM ')' DEAD {
	printf("\nFor loop\n");
	int i = $3;
	int j = $5;
	int inc = $7;
	int k;
	for(k=i; k<j; k=k+inc){
		printf("%d+%d\n",k,inc);
		printf("LOOP RUNNING\n");
	}
	printf("LOOP END\n");	
	}
	|FOR '('ID '@' NUM '@' NUM ')' DEAD {
	printf("\nFor loop\n");
	int idex = get_var_ind($3);
	int i = array_var[idex].intValue;
	int j = $5;
	int inc = $7;
	int k;
	for(k=i; k<j; k=k+inc){
		printf("%d+%d\n",k,inc);
		printf("LOOP RUNNING\n");
	}
	printf("LOOP END\n");	
	}
	|FOR '('ID '@' ID '@' NUM ')' '@' {
	printf("\nFor loop\n");
	int idex = get_var_ind($3);
	int i =array_var[idex].intValue;
	idex = get_var_ind($5);
	int j = array_var[idex].intValue;
	int inc = $7;
	int k;
	for(k=i; k<j; k=k+inc){
		printf("%d+%d\n",k,inc);
		printf("LOOP RUNNING\n");
	}
	printf("LOOP END\n");	
	}
	|FOR '('ID  '@' ID  '@' ID ')' DEAD {
	printf("\nFor loop\n");
	int idex = get_var_ind($3);
	int i = array_var[idex].intValue;
	idex = get_var_ind($5);
	int j = array_var[idex].intValue;
	idex = get_var_ind($7);
	int inc = array_var[idex].intValue;
	int k;
	for(k=i; k<j; k=k+inc){
		printf("%d+%d\n",k,inc);
		printf("LOOP RUNNING\n");
	}
	printf("LOOP END\n");	
	}
	|FOR '('NUM  '@' ID  '@' ID ')' DEAD {
	printf("\nFor loop\n");	
	int i = $3;
	int idex = get_var_ind($5);
	int j = array_var[idex].intValue;
	idex = get_var_ind($7);
	int inc = array_var[idex].intValue;
	int k;
	for(k=i; k<j; k=k+inc){
		printf("%d+%d\n",k,inc);
		printf("LOOP RUNNING\n");
	}
	printf("LOOP END\n");	
	}

	|FOR '('NUM  '@' NUM  '@' ID ')' DEAD {
	printf("\nFor loop\n");
	int i = $3;
	int j = $5;
	int idex = get_var_ind($7);
	int inc = array_var[idex].intValue;
	int k;
	for(k=i; k<j; k=k+inc){
		printf("%d+%d\n",k,inc);
		printf("LOOP RUNNING\n");
	}
	printf("LOOP END\n");	
	}
	;
	
	while_code:  WHILE'(' expression ')''{' statement '}'{
	printf("\nWhile Loop\n");

	}
	; 
%%

void yyerror(char *s)
{
	fprintf(stderr, "\n%s", s);
}

int main(){
	freopen("input.txt", "r",stdin);                       
	freopen("output.txt", "w",stdout);
	yyparse();
	return 0;
}