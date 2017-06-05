/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

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
     STRING = 258,
     ID = 259,
     VERSION_t = 260,
     NAMES = 261,
     MODULE = 262,
     VALUE = 263,
     INTEGER = 264,
     FLOAT = 265,
     STRING_TABLE = 266,
     INTEGER_TYPE = 267,
     FLOAT_TYPE = 268,
     STRING_TYPE = 269,
     ANY_TYPE = 270,
     BOOLEAN_TYPE = 271,
     PROCEDURE = 272,
     PROCEDURES = 273,
     IMPORTED = 274,
     CONTROLS = 275,
     ARGUMENT = 276,
     ARGUMENTS = 277,
     OBJECT = 278,
     CALLBACK = 279,
     END = 280,
     EXPORTED = 281,
     OBJECTS = 282,
     CHAR_SET = 283,
     WIDGET = 284,
     INC_FILE = 285,
     LIST = 286,
     UNMANAGED = 287,
     KEYSYM = 288,
     ICON = 289,
     COMPOUND_STRING = 290,
     SEPARATE = 291,
     BOOL = 292,
     GADGET = 293,
     PRIVATE = 294,
     REASON = 295,
     USER_DEFINED = 296,
     RGB = 297,
     COLOR = 298,
     COLOR_TABLE = 299,
     XBITMAPFILE = 300,
     XPIXMAPFILE = 301,
     FONT = 302,
     FONT_TABLE = 303,
     FONT_UNIT = 304,
     BACKGROUND_COLOR = 305,
     FOREGROUND_COLOR = 306,
     RIGHT_TO_LEFT = 307,
     SIXTEEN_BIT = 308
   };
#endif
/* Tokens.  */
#define STRING 258
#define ID 259
#define VERSION_t 260
#define NAMES 261
#define MODULE 262
#define VALUE 263
#define INTEGER 264
#define FLOAT 265
#define STRING_TABLE 266
#define INTEGER_TYPE 267
#define FLOAT_TYPE 268
#define STRING_TYPE 269
#define ANY_TYPE 270
#define BOOLEAN_TYPE 271
#define PROCEDURE 272
#define PROCEDURES 273
#define IMPORTED 274
#define CONTROLS 275
#define ARGUMENT 276
#define ARGUMENTS 277
#define OBJECT 278
#define CALLBACK 279
#define END 280
#define EXPORTED 281
#define OBJECTS 282
#define CHAR_SET 283
#define WIDGET 284
#define INC_FILE 285
#define LIST 286
#define UNMANAGED 287
#define KEYSYM 288
#define ICON 289
#define COMPOUND_STRING 290
#define SEPARATE 291
#define BOOL 292
#define GADGET 293
#define PRIVATE 294
#define REASON 295
#define USER_DEFINED 296
#define RGB 297
#define COLOR 298
#define COLOR_TABLE 299
#define XBITMAPFILE 300
#define XPIXMAPFILE 301
#define FONT 302
#define FONT_TABLE 303
#define FONT_UNIT 304
#define BACKGROUND_COLOR 305
#define FOREGROUND_COLOR 306
#define RIGHT_TO_LEFT 307
#define SIXTEEN_BIT 308




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 197 "yacc.y"
{
    char	*string;
}
/* Line 1489 of yacc.c.  */
#line 159 "yacc.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;

