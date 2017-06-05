
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
     MATCHER_ALL = 258,
     MATCHER_UNREAD = 259,
     MATCHER_NOT_UNREAD = 260,
     MATCHER_NEW = 261,
     MATCHER_NOT_NEW = 262,
     MATCHER_MARKED = 263,
     MATCHER_NOT_MARKED = 264,
     MATCHER_DELETED = 265,
     MATCHER_NOT_DELETED = 266,
     MATCHER_REPLIED = 267,
     MATCHER_NOT_REPLIED = 268,
     MATCHER_FORWARDED = 269,
     MATCHER_NOT_FORWARDED = 270,
     MATCHER_SUBJECT = 271,
     MATCHER_NOT_SUBJECT = 272,
     MATCHER_FROM = 273,
     MATCHER_NOT_FROM = 274,
     MATCHER_TO = 275,
     MATCHER_NOT_TO = 276,
     MATCHER_CC = 277,
     MATCHER_NOT_CC = 278,
     MATCHER_TO_OR_CC = 279,
     MATCHER_NOT_TO_AND_NOT_CC = 280,
     MATCHER_AGE_GREATER = 281,
     MATCHER_AGE_LOWER = 282,
     MATCHER_NEWSGROUPS = 283,
     MATCHER_NOT_NEWSGROUPS = 284,
     MATCHER_INREPLYTO = 285,
     MATCHER_NOT_INREPLYTO = 286,
     MATCHER_REFERENCES = 287,
     MATCHER_NOT_REFERENCES = 288,
     MATCHER_SCORE_GREATER = 289,
     MATCHER_SCORE_LOWER = 290,
     MATCHER_HEADER = 291,
     MATCHER_NOT_HEADER = 292,
     MATCHER_HEADERS_PART = 293,
     MATCHER_NOT_HEADERS_PART = 294,
     MATCHER_MESSAGE = 295,
     MATCHER_NOT_MESSAGE = 296,
     MATCHER_BODY_PART = 297,
     MATCHER_NOT_BODY_PART = 298,
     MATCHER_TEST = 299,
     MATCHER_NOT_TEST = 300,
     MATCHER_MATCHCASE = 301,
     MATCHER_MATCH = 302,
     MATCHER_REGEXPCASE = 303,
     MATCHER_REGEXP = 304,
     MATCHER_SCORE = 305,
     MATCHER_MOVE = 306,
     MATCHER_FOUND_IN_ADDRESSBOOK = 307,
     MATCHER_NOT_FOUND_IN_ADDRESSBOOK = 308,
     MATCHER_IN = 309,
     MATCHER_COPY = 310,
     MATCHER_DELETE = 311,
     MATCHER_MARK = 312,
     MATCHER_UNMARK = 313,
     MATCHER_LOCK = 314,
     MATCHER_UNLOCK = 315,
     MATCHER_EXECUTE = 316,
     MATCHER_MARK_AS_READ = 317,
     MATCHER_MARK_AS_UNREAD = 318,
     MATCHER_FORWARD = 319,
     MATCHER_MARK_AS_SPAM = 320,
     MATCHER_MARK_AS_HAM = 321,
     MATCHER_FORWARD_AS_ATTACHMENT = 322,
     MATCHER_EOL = 323,
     MATCHER_OR = 324,
     MATCHER_AND = 325,
     MATCHER_COLOR = 326,
     MATCHER_SCORE_EQUAL = 327,
     MATCHER_REDIRECT = 328,
     MATCHER_SIZE_GREATER = 329,
     MATCHER_SIZE_SMALLER = 330,
     MATCHER_SIZE_EQUAL = 331,
     MATCHER_LOCKED = 332,
     MATCHER_NOT_LOCKED = 333,
     MATCHER_PARTIAL = 334,
     MATCHER_NOT_PARTIAL = 335,
     MATCHER_COLORLABEL = 336,
     MATCHER_NOT_COLORLABEL = 337,
     MATCHER_IGNORE_THREAD = 338,
     MATCHER_NOT_IGNORE_THREAD = 339,
     MATCHER_WATCH_THREAD = 340,
     MATCHER_NOT_WATCH_THREAD = 341,
     MATCHER_CHANGE_SCORE = 342,
     MATCHER_SET_SCORE = 343,
     MATCHER_ADD_TO_ADDRESSBOOK = 344,
     MATCHER_STOP = 345,
     MATCHER_HIDE = 346,
     MATCHER_IGNORE = 347,
     MATCHER_WATCH = 348,
     MATCHER_SPAM = 349,
     MATCHER_NOT_SPAM = 350,
     MATCHER_HAS_ATTACHMENT = 351,
     MATCHER_HAS_NO_ATTACHMENT = 352,
     MATCHER_SIGNED = 353,
     MATCHER_NOT_SIGNED = 354,
     MATCHER_TAG = 355,
     MATCHER_NOT_TAG = 356,
     MATCHER_SET_TAG = 357,
     MATCHER_UNSET_TAG = 358,
     MATCHER_TAGGED = 359,
     MATCHER_NOT_TAGGED = 360,
     MATCHER_CLEAR_TAGS = 361,
     MATCHER_ENABLED = 362,
     MATCHER_DISABLED = 363,
     MATCHER_RULENAME = 364,
     MATCHER_ACCOUNT = 365,
     MATCHER_STRING = 366,
     MATCHER_SECTION = 367,
     MATCHER_INTEGER = 368
   };
#endif
/* Tokens.  */
#define MATCHER_ALL 258
#define MATCHER_UNREAD 259
#define MATCHER_NOT_UNREAD 260
#define MATCHER_NEW 261
#define MATCHER_NOT_NEW 262
#define MATCHER_MARKED 263
#define MATCHER_NOT_MARKED 264
#define MATCHER_DELETED 265
#define MATCHER_NOT_DELETED 266
#define MATCHER_REPLIED 267
#define MATCHER_NOT_REPLIED 268
#define MATCHER_FORWARDED 269
#define MATCHER_NOT_FORWARDED 270
#define MATCHER_SUBJECT 271
#define MATCHER_NOT_SUBJECT 272
#define MATCHER_FROM 273
#define MATCHER_NOT_FROM 274
#define MATCHER_TO 275
#define MATCHER_NOT_TO 276
#define MATCHER_CC 277
#define MATCHER_NOT_CC 278
#define MATCHER_TO_OR_CC 279
#define MATCHER_NOT_TO_AND_NOT_CC 280
#define MATCHER_AGE_GREATER 281
#define MATCHER_AGE_LOWER 282
#define MATCHER_NEWSGROUPS 283
#define MATCHER_NOT_NEWSGROUPS 284
#define MATCHER_INREPLYTO 285
#define MATCHER_NOT_INREPLYTO 286
#define MATCHER_REFERENCES 287
#define MATCHER_NOT_REFERENCES 288
#define MATCHER_SCORE_GREATER 289
#define MATCHER_SCORE_LOWER 290
#define MATCHER_HEADER 291
#define MATCHER_NOT_HEADER 292
#define MATCHER_HEADERS_PART 293
#define MATCHER_NOT_HEADERS_PART 294
#define MATCHER_MESSAGE 295
#define MATCHER_NOT_MESSAGE 296
#define MATCHER_BODY_PART 297
#define MATCHER_NOT_BODY_PART 298
#define MATCHER_TEST 299
#define MATCHER_NOT_TEST 300
#define MATCHER_MATCHCASE 301
#define MATCHER_MATCH 302
#define MATCHER_REGEXPCASE 303
#define MATCHER_REGEXP 304
#define MATCHER_SCORE 305
#define MATCHER_MOVE 306
#define MATCHER_FOUND_IN_ADDRESSBOOK 307
#define MATCHER_NOT_FOUND_IN_ADDRESSBOOK 308
#define MATCHER_IN 309
#define MATCHER_COPY 310
#define MATCHER_DELETE 311
#define MATCHER_MARK 312
#define MATCHER_UNMARK 313
#define MATCHER_LOCK 314
#define MATCHER_UNLOCK 315
#define MATCHER_EXECUTE 316
#define MATCHER_MARK_AS_READ 317
#define MATCHER_MARK_AS_UNREAD 318
#define MATCHER_FORWARD 319
#define MATCHER_MARK_AS_SPAM 320
#define MATCHER_MARK_AS_HAM 321
#define MATCHER_FORWARD_AS_ATTACHMENT 322
#define MATCHER_EOL 323
#define MATCHER_OR 324
#define MATCHER_AND 325
#define MATCHER_COLOR 326
#define MATCHER_SCORE_EQUAL 327
#define MATCHER_REDIRECT 328
#define MATCHER_SIZE_GREATER 329
#define MATCHER_SIZE_SMALLER 330
#define MATCHER_SIZE_EQUAL 331
#define MATCHER_LOCKED 332
#define MATCHER_NOT_LOCKED 333
#define MATCHER_PARTIAL 334
#define MATCHER_NOT_PARTIAL 335
#define MATCHER_COLORLABEL 336
#define MATCHER_NOT_COLORLABEL 337
#define MATCHER_IGNORE_THREAD 338
#define MATCHER_NOT_IGNORE_THREAD 339
#define MATCHER_WATCH_THREAD 340
#define MATCHER_NOT_WATCH_THREAD 341
#define MATCHER_CHANGE_SCORE 342
#define MATCHER_SET_SCORE 343
#define MATCHER_ADD_TO_ADDRESSBOOK 344
#define MATCHER_STOP 345
#define MATCHER_HIDE 346
#define MATCHER_IGNORE 347
#define MATCHER_WATCH 348
#define MATCHER_SPAM 349
#define MATCHER_NOT_SPAM 350
#define MATCHER_HAS_ATTACHMENT 351
#define MATCHER_HAS_NO_ATTACHMENT 352
#define MATCHER_SIGNED 353
#define MATCHER_NOT_SIGNED 354
#define MATCHER_TAG 355
#define MATCHER_NOT_TAG 356
#define MATCHER_SET_TAG 357
#define MATCHER_UNSET_TAG 358
#define MATCHER_TAGGED 359
#define MATCHER_NOT_TAGGED 360
#define MATCHER_CLEAR_TAGS 361
#define MATCHER_ENABLED 362
#define MATCHER_DISABLED 363
#define MATCHER_RULENAME 364
#define MATCHER_ACCOUNT 365
#define MATCHER_STRING 366
#define MATCHER_SECTION 367
#define MATCHER_INTEGER 368




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 1676 of yacc.c  */
#line 309 "matcher_parser_parse.y"

	char *str;
	int value;



/* Line 1676 of yacc.c  */
#line 285 "matcher_parser_parse.h"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE yylval;


