
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
     TOK_CASE = 261,
     TOK_CHAR = 262,
     TOK_CONST = 263,
     TOK_CONTEXT = 264,
     TOK_DEFAULT = 265,
     TOK_DOUBLE = 266,
     TOK_ENUM = 267,
     TOK_EXCEPTION = 268,
     TOK_FALSE = 269,
     TOK_FIXED = 270,
     TOK_FLOAT = 271,
     TOK_IN = 272,
     TOK_INOUT = 273,
     TOK_INTERFACE = 274,
     TOK_LONG = 275,
     TOK_MODULE = 276,
     TOK_NATIVE = 277,
     TOK_OBJECT = 278,
     TOK_OCTET = 279,
     TOK_ONEWAY = 280,
     TOK_OP_SCOPE = 281,
     TOK_OP_SHL = 282,
     TOK_OP_SHR = 283,
     TOK_OUT = 284,
     TOK_RAISES = 285,
     TOK_READONLY = 286,
     TOK_SEQUENCE = 287,
     TOK_SHORT = 288,
     TOK_STRING = 289,
     TOK_STRUCT = 290,
     TOK_SWITCH = 291,
     TOK_TRUE = 292,
     TOK_TYPECODE = 293,
     TOK_TYPEDEF = 294,
     TOK_UNION = 295,
     TOK_UNSIGNED = 296,
     TOK_VARARGS = 297,
     TOK_VOID = 298,
     TOK_WCHAR = 299,
     TOK_WSTRING = 300,
     TOK_FLOATP = 301,
     TOK_INTEGER = 302,
     TOK_DECLSPEC = 303,
     TOK_PROP_KEY = 304,
     TOK_PROP_VALUE = 305,
     TOK_NATIVE_TYPE = 306,
     TOK_IDENT = 307,
     TOK_SQSTRING = 308,
     TOK_DQSTRING = 309,
     TOK_FIXEDP = 310,
     TOK_CODEFRAG = 311,
     TOK_SRCFILE = 312
   };
#endif
/* Tokens.  */
#define TOK_ANY 258
#define TOK_ATTRIBUTE 259
#define TOK_BOOLEAN 260
#define TOK_CASE 261
#define TOK_CHAR 262
#define TOK_CONST 263
#define TOK_CONTEXT 264
#define TOK_DEFAULT 265
#define TOK_DOUBLE 266
#define TOK_ENUM 267
#define TOK_EXCEPTION 268
#define TOK_FALSE 269
#define TOK_FIXED 270
#define TOK_FLOAT 271
#define TOK_IN 272
#define TOK_INOUT 273
#define TOK_INTERFACE 274
#define TOK_LONG 275
#define TOK_MODULE 276
#define TOK_NATIVE 277
#define TOK_OBJECT 278
#define TOK_OCTET 279
#define TOK_ONEWAY 280
#define TOK_OP_SCOPE 281
#define TOK_OP_SHL 282
#define TOK_OP_SHR 283
#define TOK_OUT 284
#define TOK_RAISES 285
#define TOK_READONLY 286
#define TOK_SEQUENCE 287
#define TOK_SHORT 288
#define TOK_STRING 289
#define TOK_STRUCT 290
#define TOK_SWITCH 291
#define TOK_TRUE 292
#define TOK_TYPECODE 293
#define TOK_TYPEDEF 294
#define TOK_UNION 295
#define TOK_UNSIGNED 296
#define TOK_VARARGS 297
#define TOK_VOID 298
#define TOK_WCHAR 299
#define TOK_WSTRING 300
#define TOK_FLOATP 301
#define TOK_INTEGER 302
#define TOK_DECLSPEC 303
#define TOK_PROP_KEY 304
#define TOK_PROP_VALUE 305
#define TOK_NATIVE_TYPE 306
#define TOK_IDENT 307
#define TOK_SQSTRING 308
#define TOK_DQSTRING 309
#define TOK_FIXEDP 310
#define TOK_CODEFRAG 311
#define TOK_SRCFILE 312




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 1676 of yacc.c  */
#line 117 "./parser.y"

	IDL_tree tree;
	struct {
		IDL_tree tree;
		gpointer data;
	} treedata;
	GHashTable *hash_table;
	char *str;
	gboolean boolean;
	IDL_declspec_t declspec;
	IDL_longlong_t integer;
	double floatp;
	enum IDL_unaryop unaryop;
	enum IDL_param_attr paramattr;



/* Line 1676 of yacc.c  */
#line 184 "y.tab.h"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE yylval;


