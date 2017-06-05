
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
     SHOW_NEWSGROUPS = 258,
     SHOW_DATE = 259,
     SHOW_FROM = 260,
     SHOW_FULLNAME = 261,
     SHOW_FIRST_NAME = 262,
     SHOW_LAST_NAME = 263,
     SHOW_SENDER_INITIAL = 264,
     SHOW_SUBJECT = 265,
     SHOW_TO = 266,
     SHOW_MESSAGEID = 267,
     SHOW_PERCENT = 268,
     SHOW_CC = 269,
     SHOW_REFERENCES = 270,
     SHOW_MESSAGE = 271,
     SHOW_QUOTED_MESSAGE = 272,
     SHOW_BACKSLASH = 273,
     SHOW_TAB = 274,
     SHOW_MAIL_ADDRESS = 275,
     SHOW_QUOTED_MESSAGE_NO_SIGNATURE = 276,
     SHOW_MESSAGE_NO_SIGNATURE = 277,
     SHOW_EOL = 278,
     SHOW_QUESTION_MARK = 279,
     SHOW_EXCLAMATION_MARK = 280,
     SHOW_PIPE = 281,
     SHOW_OPARENT = 282,
     SHOW_CPARENT = 283,
     SHOW_ACCOUNT_FULL_NAME = 284,
     SHOW_ACCOUNT_MAIL_ADDRESS = 285,
     SHOW_ACCOUNT_NAME = 286,
     SHOW_ACCOUNT_ORGANIZATION = 287,
     SHOW_ACCOUNT_DICT = 288,
     SHOW_ACCOUNT_SIG = 289,
     SHOW_ACCOUNT_SIGPATH = 290,
     SHOW_DICT = 291,
     SHOW_TAGS = 292,
     SHOW_ADDRESSBOOK_COMPLETION_FOR_CC = 293,
     SHOW_ADDRESSBOOK_COMPLETION_FOR_FROM = 294,
     SHOW_ADDRESSBOOK_COMPLETION_FOR_TO = 295,
     QUERY_DATE = 296,
     QUERY_FROM = 297,
     QUERY_FULLNAME = 298,
     QUERY_SUBJECT = 299,
     QUERY_TO = 300,
     QUERY_NEWSGROUPS = 301,
     QUERY_MESSAGEID = 302,
     QUERY_CC = 303,
     QUERY_REFERENCES = 304,
     QUERY_ACCOUNT_FULL_NAME = 305,
     QUERY_ACCOUNT_ORGANIZATION = 306,
     QUERY_ACCOUNT_DICT = 307,
     QUERY_ACCOUNT_SIG = 308,
     QUERY_ACCOUNT_SIGPATH = 309,
     QUERY_DICT = 310,
     QUERY_CC_FOUND_IN_ADDRESSBOOK = 311,
     QUERY_FROM_FOUND_IN_ADDRESSBOOK = 312,
     QUERY_TO_FOUND_IN_ADDRESSBOOK = 313,
     QUERY_NOT_DATE = 314,
     QUERY_NOT_FROM = 315,
     QUERY_NOT_FULLNAME = 316,
     QUERY_NOT_SUBJECT = 317,
     QUERY_NOT_TO = 318,
     QUERY_NOT_NEWSGROUPS = 319,
     QUERY_NOT_MESSAGEID = 320,
     QUERY_NOT_CC = 321,
     QUERY_NOT_REFERENCES = 322,
     QUERY_NOT_ACCOUNT_FULL_NAME = 323,
     QUERY_NOT_ACCOUNT_ORGANIZATION = 324,
     QUERY_NOT_ACCOUNT_DICT = 325,
     QUERY_NOT_ACCOUNT_SIG = 326,
     QUERY_NOT_ACCOUNT_SIGPATH = 327,
     QUERY_NOT_DICT = 328,
     QUERY_NOT_CC_FOUND_IN_ADDRESSBOOK = 329,
     QUERY_NOT_FROM_FOUND_IN_ADDRESSBOOK = 330,
     QUERY_NOT_TO_FOUND_IN_ADDRESSBOOK = 331,
     INSERT_FILE = 332,
     INSERT_PROGRAMOUTPUT = 333,
     INSERT_USERINPUT = 334,
     ATTACH_FILE = 335,
     OPARENT = 336,
     CPARENT = 337,
     CHARACTER = 338,
     SHOW_DATE_EXPR = 339,
     SET_CURSOR_POS = 340
   };
#endif
/* Tokens.  */
#define SHOW_NEWSGROUPS 258
#define SHOW_DATE 259
#define SHOW_FROM 260
#define SHOW_FULLNAME 261
#define SHOW_FIRST_NAME 262
#define SHOW_LAST_NAME 263
#define SHOW_SENDER_INITIAL 264
#define SHOW_SUBJECT 265
#define SHOW_TO 266
#define SHOW_MESSAGEID 267
#define SHOW_PERCENT 268
#define SHOW_CC 269
#define SHOW_REFERENCES 270
#define SHOW_MESSAGE 271
#define SHOW_QUOTED_MESSAGE 272
#define SHOW_BACKSLASH 273
#define SHOW_TAB 274
#define SHOW_MAIL_ADDRESS 275
#define SHOW_QUOTED_MESSAGE_NO_SIGNATURE 276
#define SHOW_MESSAGE_NO_SIGNATURE 277
#define SHOW_EOL 278
#define SHOW_QUESTION_MARK 279
#define SHOW_EXCLAMATION_MARK 280
#define SHOW_PIPE 281
#define SHOW_OPARENT 282
#define SHOW_CPARENT 283
#define SHOW_ACCOUNT_FULL_NAME 284
#define SHOW_ACCOUNT_MAIL_ADDRESS 285
#define SHOW_ACCOUNT_NAME 286
#define SHOW_ACCOUNT_ORGANIZATION 287
#define SHOW_ACCOUNT_DICT 288
#define SHOW_ACCOUNT_SIG 289
#define SHOW_ACCOUNT_SIGPATH 290
#define SHOW_DICT 291
#define SHOW_TAGS 292
#define SHOW_ADDRESSBOOK_COMPLETION_FOR_CC 293
#define SHOW_ADDRESSBOOK_COMPLETION_FOR_FROM 294
#define SHOW_ADDRESSBOOK_COMPLETION_FOR_TO 295
#define QUERY_DATE 296
#define QUERY_FROM 297
#define QUERY_FULLNAME 298
#define QUERY_SUBJECT 299
#define QUERY_TO 300
#define QUERY_NEWSGROUPS 301
#define QUERY_MESSAGEID 302
#define QUERY_CC 303
#define QUERY_REFERENCES 304
#define QUERY_ACCOUNT_FULL_NAME 305
#define QUERY_ACCOUNT_ORGANIZATION 306
#define QUERY_ACCOUNT_DICT 307
#define QUERY_ACCOUNT_SIG 308
#define QUERY_ACCOUNT_SIGPATH 309
#define QUERY_DICT 310
#define QUERY_CC_FOUND_IN_ADDRESSBOOK 311
#define QUERY_FROM_FOUND_IN_ADDRESSBOOK 312
#define QUERY_TO_FOUND_IN_ADDRESSBOOK 313
#define QUERY_NOT_DATE 314
#define QUERY_NOT_FROM 315
#define QUERY_NOT_FULLNAME 316
#define QUERY_NOT_SUBJECT 317
#define QUERY_NOT_TO 318
#define QUERY_NOT_NEWSGROUPS 319
#define QUERY_NOT_MESSAGEID 320
#define QUERY_NOT_CC 321
#define QUERY_NOT_REFERENCES 322
#define QUERY_NOT_ACCOUNT_FULL_NAME 323
#define QUERY_NOT_ACCOUNT_ORGANIZATION 324
#define QUERY_NOT_ACCOUNT_DICT 325
#define QUERY_NOT_ACCOUNT_SIG 326
#define QUERY_NOT_ACCOUNT_SIGPATH 327
#define QUERY_NOT_DICT 328
#define QUERY_NOT_CC_FOUND_IN_ADDRESSBOOK 329
#define QUERY_NOT_FROM_FOUND_IN_ADDRESSBOOK 330
#define QUERY_NOT_TO_FOUND_IN_ADDRESSBOOK 331
#define INSERT_FILE 332
#define INSERT_PROGRAMOUTPUT 333
#define INSERT_USERINPUT 334
#define ATTACH_FILE 335
#define OPARENT 336
#define CPARENT 337
#define CHARACTER 338
#define SHOW_DATE_EXPR 339
#define SET_CURSOR_POS 340




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 1676 of yacc.c  */
#line 571 "quote_fmt_parse.y"

	char chr;
	char str[256];



/* Line 1676 of yacc.c  */
#line 229 "quote_fmt_parse.h"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE yylval;


