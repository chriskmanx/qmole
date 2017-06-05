
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
     TOK_ANY = 258,
     TOK_ATTRIBUTE = 259,
     TOK_BOOLEAN = 260,
     TOK_BYTE = 261,
     TOK_CALLBACK = 262,
     TOK_LEGACYCALLER = 263,
     TOK_CONST = 264,
     TOK_CREATOR = 265,
     TOK_DATE = 266,
     TOK_DELETER = 267,
     TOK_DICTIONARY = 268,
     TOK_DOUBLE = 269,
     TOK_ELLIPSIS = 270,
     TOK_ENUM = 271,
     TOK_EOL = 272,
     TOK_EXCEPTION = 273,
     TOK_FALSE = 274,
     TOK_FLOAT = 275,
     TOK_GETRAISES = 276,
     TOK_GETTER = 277,
     TOK_IMPLEMENTS = 278,
     TOK_IN = 279,
     TOK_INFINITY = 280,
     TOK_INHERIT = 281,
     TOK_INTERFACE = 282,
     TOK_LONG = 283,
     TOK_MODULE = 284,
     TOK_NAN = 285,
     TOK_NATIVE = 286,
     TOK_NULL_LITERAL = 287,
     TOK_OBJECT = 288,
     TOK_OCTET = 289,
     TOK_OMITTABLE = 290,
     TOK_OPTIONAL = 291,
     TOK_OR = 292,
     TOK_PARTIAL = 293,
     TOK_RAISES = 294,
     TOK_READONLY = 295,
     TOK_SETRAISES = 296,
     TOK_SETTER = 297,
     TOK_SEQUENCE = 298,
     TOK_SHORT = 299,
     TOK_STATIC = 300,
     TOK_STRING = 301,
     TOK_STRINGIFIER = 302,
     TOK_TRUE = 303,
     TOK_TYPEDEF = 304,
     TOK_UNRESTRICTED = 305,
     TOK_UNSIGNED = 306,
     TOK_VOID = 307,
     TOK_POUND_SIGN = 308,
     TOK_IDENTIFIER = 309,
     TOK_INT_LITERAL = 310,
     TOK_FLOAT_LITERAL = 311,
     TOK_STRING_LITERAL = 312,
     TOK_OTHER_LITERAL = 313,
     TOK_JAVADOC = 314
   };
#endif



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 1676 of yacc.c  */
#line 46 "src/webidl-parser.y"

    int attr;
    long value;
    bool isit;
    char* text;
    struct webidl_node *node;



/* Line 1676 of yacc.c  */
#line 121 "build-arm-apple-darwin9-arm-apple-darwin9-release-binary/webidl-parser.h"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif



#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
} YYLTYPE;
# define yyltype YYLTYPE /* obsolescent; will be withdrawn */
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif



