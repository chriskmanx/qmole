
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
     TOK_EXEC = 258,
     TOK_EXIT = 259,
     TOK_RELOAD = 260,
     TOK_RESTART = 261,
     TOK_KILL = 262,
     TOK_WINDOW = 263,
     TOK_CLIENT = 264,
     TOK_FULLSCREEN = 265,
     TOK_GLOBAL = 266,
     TOK_LAYOUT = 267,
     TOK_DEFAULT = 268,
     TOK_STACKED = 269,
     TOK_TABBED = 270,
     TOK_BORDER = 271,
     TOK_NORMAL = 272,
     TOK_NONE = 273,
     TOK_1PIXEL = 274,
     TOK_MODE = 275,
     TOK_TILING = 276,
     TOK_FLOATING = 277,
     TOK_MODE_TOGGLE = 278,
     TOK_ENABLE = 279,
     TOK_DISABLE = 280,
     TOK_WORKSPACE = 281,
     TOK_OUTPUT = 282,
     TOK_TOGGLE = 283,
     TOK_FOCUS = 284,
     TOK_MOVE = 285,
     TOK_OPEN = 286,
     TOK_NEXT = 287,
     TOK_PREV = 288,
     TOK_SPLIT = 289,
     TOK_HORIZONTAL = 290,
     TOK_VERTICAL = 291,
     TOK_UP = 292,
     TOK_DOWN = 293,
     TOK_LEFT = 294,
     TOK_RIGHT = 295,
     TOK_PARENT = 296,
     TOK_CHILD = 297,
     TOK_APPEND_LAYOUT = 298,
     TOK_MARK = 299,
     TOK_RESIZE = 300,
     TOK_GROW = 301,
     TOK_SHRINK = 302,
     TOK_PX = 303,
     TOK_OR = 304,
     TOK_PPT = 305,
     TOK_NOP = 306,
     TOK_BACK_AND_FORTH = 307,
     TOK_NO_STARTUP_ID = 308,
     TOK_CLASS = 309,
     TOK_INSTANCE = 310,
     TOK_WINDOW_ROLE = 311,
     TOK_ID = 312,
     TOK_CON_ID = 313,
     TOK_TITLE = 314,
     STR = 315,
     NUMBER = 316
   };
#endif



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 1676 of yacc.c  */
#line 121 "src/cmdparse.y"

    char *string;
    char chr;
    int number;



/* Line 1676 of yacc.c  */
#line 121 "src/cmdparse.tab.h"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE cmdyylval;


