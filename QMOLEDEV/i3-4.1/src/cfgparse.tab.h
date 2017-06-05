
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
     NUMBER = 258,
     WORD = 259,
     STR = 260,
     STR_NG = 261,
     HEXCOLOR = 262,
     OUTPUT = 263,
     TOKBINDCODE = 264,
     TOKTERMINAL = 265,
     TOKCOMMENT = 266,
     TOKFONT = 267,
     TOKBINDSYM = 268,
     MODIFIER = 269,
     TOKCONTROL = 270,
     TOKSHIFT = 271,
     TOKFLOATING_MODIFIER = 272,
     QUOTEDSTRING = 273,
     TOKWORKSPACE = 274,
     TOKOUTPUT = 275,
     TOKASSIGN = 276,
     TOKSET = 277,
     TOKIPCSOCKET = 278,
     TOKRESTARTSTATE = 279,
     TOKEXEC = 280,
     TOKEXEC_ALWAYS = 281,
     TOKSINGLECOLOR = 282,
     TOKCOLOR = 283,
     TOKARROW = 284,
     TOKMODE = 285,
     TOK_BAR = 286,
     TOK_ORIENTATION = 287,
     TOK_HORIZ = 288,
     TOK_VERT = 289,
     TOK_AUTO = 290,
     TOK_WORKSPACE_LAYOUT = 291,
     TOKNEWWINDOW = 292,
     TOKNEWFLOAT = 293,
     TOK_NORMAL = 294,
     TOK_NONE = 295,
     TOK_1PIXEL = 296,
     TOKFOCUSFOLLOWSMOUSE = 297,
     TOK_FORCE_FOCUS_WRAPPING = 298,
     TOK_FORCE_XINERAMA = 299,
     TOK_WORKSPACE_AUTO_BAF = 300,
     TOKWORKSPACEBAR = 301,
     TOK_DEFAULT = 302,
     TOK_STACKING = 303,
     TOK_TABBED = 304,
     TOKSTACKLIMIT = 305,
     TOK_POPUP_DURING_FULLSCREEN = 306,
     TOK_IGNORE = 307,
     TOK_LEAVE_FULLSCREEN = 308,
     TOK_FOR_WINDOW = 309,
     TOK_BAR_OUTPUT = 310,
     TOK_BAR_TRAY_OUTPUT = 311,
     TOK_BAR_SOCKET_PATH = 312,
     TOK_BAR_MODE = 313,
     TOK_BAR_HIDE = 314,
     TOK_BAR_DOCK = 315,
     TOK_BAR_POSITION = 316,
     TOK_BAR_BOTTOM = 317,
     TOK_BAR_TOP = 318,
     TOK_BAR_STATUS_COMMAND = 319,
     TOK_BAR_FONT = 320,
     TOK_BAR_WORKSPACE_BUTTONS = 321,
     TOK_BAR_VERBOSE = 322,
     TOK_BAR_COLORS = 323,
     TOK_BAR_COLOR_BACKGROUND = 324,
     TOK_BAR_COLOR_STATUSLINE = 325,
     TOK_BAR_COLOR_FOCUSED_WORKSPACE = 326,
     TOK_BAR_COLOR_ACTIVE_WORKSPACE = 327,
     TOK_BAR_COLOR_INACTIVE_WORKSPACE = 328,
     TOK_BAR_COLOR_URGENT_WORKSPACE = 329,
     TOK_NO_STARTUP_ID = 330,
     TOK_MARK = 331,
     TOK_CLASS = 332,
     TOK_INSTANCE = 333,
     TOK_WINDOW_ROLE = 334,
     TOK_ID = 335,
     TOK_CON_ID = 336,
     TOK_TITLE = 337
   };
#endif



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 1676 of yacc.c  */
#line 636 "src/cfgparse.y"

    int number;
    char *string;
    uint32_t *single_color;
    struct Colortriple *color;
    Match *match;
    struct Binding *binding;



/* Line 1676 of yacc.c  */
#line 145 "src/cfgparse.tab.h"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE yylval;


