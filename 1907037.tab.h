
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton interface for Bison's Yacc-like parsers in C
   
      Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     JATRA = 258,
     Int = 259,
     Float = 260,
     Char = 261,
     Bool = 262,
     SURU = 263,
     SES = 264,
     ID = 265,
     NUM = 266,
     ASSIGN = 267,
     CHARACTER = 268,
     ADD = 269,
     SUB = 270,
     MUL = 271,
     DIV = 272,
     POW = 273,
     EQL = 274,
     SHOW = 275,
     SIN = 276,
     COS = 277,
     TAN = 278,
     LOG10 = 279,
     LOG = 280,
     LorE = 281,
     GorE = 282,
     ELSEIF = 283,
     GREATER = 284,
     LESSER = 285,
     ELSE = 286,
     IF = 287,
     MINN = 288,
     MAX = 289,
     GCD = 290,
     FOR = 291,
     WHILE = 292,
     Include = 293
   };
#endif



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 1676 of yacc.c  */
#line 61 "1907037.y"

	int ival;
	double floatValue;
	char* stringValue;



/* Line 1676 of yacc.c  */
#line 98 "1907037.tab.h"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE yylval;


