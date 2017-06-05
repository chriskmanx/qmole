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
     TEXATOP = 258,
     TEXOVER = 259,
     CHAR = 260,
     STARTMATH = 261,
     STARTDMATH = 262,
     ENDMATH = 263,
     MI = 264,
     MIB = 265,
     MN = 266,
     MO = 267,
     SUP = 268,
     SUB = 269,
     MROWOPEN = 270,
     MROWCLOSE = 271,
     LEFT = 272,
     RIGHT = 273,
     BIG = 274,
     BBIG = 275,
     BIGG = 276,
     BBIGG = 277,
     BIGL = 278,
     BBIGL = 279,
     BIGGL = 280,
     BBIGGL = 281,
     FRAC = 282,
     TFRAC = 283,
     MATHOP = 284,
     MOP = 285,
     MOL = 286,
     MOLL = 287,
     MOF = 288,
     PERIODDELIM = 289,
     OTHERDELIM = 290,
     LEFTDELIM = 291,
     RIGHTDELIM = 292,
     MOS = 293,
     MOB = 294,
     SQRT = 295,
     ROOT = 296,
     BINOM = 297,
     UNDER = 298,
     OVER = 299,
     OVERBRACE = 300,
     UNDERBRACE = 301,
     UNDEROVER = 302,
     TENSOR = 303,
     MULTI = 304,
     ARRAY = 305,
     COLSEP = 306,
     ROWSEP = 307,
     ARRAYOPTS = 308,
     COLLAYOUT = 309,
     COLALIGN = 310,
     ROWALIGN = 311,
     ALIGN = 312,
     EQROWS = 313,
     EQCOLS = 314,
     ROWLINES = 315,
     COLLINES = 316,
     FRAME = 317,
     PADDING = 318,
     ATTRLIST = 319,
     ITALICS = 320,
     BOLD = 321,
     SLASHED = 322,
     RM = 323,
     BB = 324,
     ST = 325,
     END = 326,
     BBLOWERCHAR = 327,
     BBUPPERCHAR = 328,
     BBDIGIT = 329,
     CALCHAR = 330,
     FRAKCHAR = 331,
     CAL = 332,
     FRAK = 333,
     ROWOPTS = 334,
     TEXTSIZE = 335,
     SCSIZE = 336,
     SCSCSIZE = 337,
     DISPLAY = 338,
     TEXTSTY = 339,
     TEXTBOX = 340,
     TEXTSTRING = 341,
     XMLSTRING = 342,
     CELLOPTS = 343,
     ROWSPAN = 344,
     COLSPAN = 345,
     THINSPACE = 346,
     MEDSPACE = 347,
     THICKSPACE = 348,
     QUAD = 349,
     QQUAD = 350,
     NEGSPACE = 351,
     PHANTOM = 352,
     HREF = 353,
     UNKNOWNCHAR = 354,
     EMPTYMROW = 355,
     STATLINE = 356,
     TOGGLE = 357,
     FGHIGHLIGHT = 358,
     BGHIGHLIGHT = 359,
     SPACE = 360,
     INTONE = 361,
     INTTWO = 362,
     INTTHREE = 363,
     BAR = 364,
     WIDEBAR = 365,
     VEC = 366,
     WIDEVEC = 367,
     HAT = 368,
     WIDEHAT = 369,
     CHECK = 370,
     WIDECHECK = 371,
     TILDE = 372,
     WIDETILDE = 373,
     DOT = 374,
     DDOT = 375,
     UNARYMINUS = 376,
     UNARYPLUS = 377,
     BEGINENV = 378,
     ENDENV = 379,
     MATRIX = 380,
     PMATRIX = 381,
     BMATRIX = 382,
     BBMATRIX = 383,
     VMATRIX = 384,
     VVMATRIX = 385,
     SVG = 386,
     ENDSVG = 387,
     SMALLMATRIX = 388,
     CASES = 389,
     ALIGNED = 390,
     GATHERED = 391,
     SUBSTACK = 392,
     PMOD = 393,
     RMCHAR = 394,
     COLOR = 395,
     BGCOLOR = 396
   };
#endif
/* Tokens.  */
#define TEXATOP 258
#define TEXOVER 259
#define CHAR 260
#define STARTMATH 261
#define STARTDMATH 262
#define ENDMATH 263
#define MI 264
#define MIB 265
#define MN 266
#define MO 267
#define SUP 268
#define SUB 269
#define MROWOPEN 270
#define MROWCLOSE 271
#define LEFT 272
#define RIGHT 273
#define BIG 274
#define BBIG 275
#define BIGG 276
#define BBIGG 277
#define BIGL 278
#define BBIGL 279
#define BIGGL 280
#define BBIGGL 281
#define FRAC 282
#define TFRAC 283
#define MATHOP 284
#define MOP 285
#define MOL 286
#define MOLL 287
#define MOF 288
#define PERIODDELIM 289
#define OTHERDELIM 290
#define LEFTDELIM 291
#define RIGHTDELIM 292
#define MOS 293
#define MOB 294
#define SQRT 295
#define ROOT 296
#define BINOM 297
#define UNDER 298
#define OVER 299
#define OVERBRACE 300
#define UNDERBRACE 301
#define UNDEROVER 302
#define TENSOR 303
#define MULTI 304
#define ARRAY 305
#define COLSEP 306
#define ROWSEP 307
#define ARRAYOPTS 308
#define COLLAYOUT 309
#define COLALIGN 310
#define ROWALIGN 311
#define ALIGN 312
#define EQROWS 313
#define EQCOLS 314
#define ROWLINES 315
#define COLLINES 316
#define FRAME 317
#define PADDING 318
#define ATTRLIST 319
#define ITALICS 320
#define BOLD 321
#define SLASHED 322
#define RM 323
#define BB 324
#define ST 325
#define END 326
#define BBLOWERCHAR 327
#define BBUPPERCHAR 328
#define BBDIGIT 329
#define CALCHAR 330
#define FRAKCHAR 331
#define CAL 332
#define FRAK 333
#define ROWOPTS 334
#define TEXTSIZE 335
#define SCSIZE 336
#define SCSCSIZE 337
#define DISPLAY 338
#define TEXTSTY 339
#define TEXTBOX 340
#define TEXTSTRING 341
#define XMLSTRING 342
#define CELLOPTS 343
#define ROWSPAN 344
#define COLSPAN 345
#define THINSPACE 346
#define MEDSPACE 347
#define THICKSPACE 348
#define QUAD 349
#define QQUAD 350
#define NEGSPACE 351
#define PHANTOM 352
#define HREF 353
#define UNKNOWNCHAR 354
#define EMPTYMROW 355
#define STATLINE 356
#define TOGGLE 357
#define FGHIGHLIGHT 358
#define BGHIGHLIGHT 359
#define SPACE 360
#define INTONE 361
#define INTTWO 362
#define INTTHREE 363
#define BAR 364
#define WIDEBAR 365
#define VEC 366
#define WIDEVEC 367
#define HAT 368
#define WIDEHAT 369
#define CHECK 370
#define WIDECHECK 371
#define TILDE 372
#define WIDETILDE 373
#define DOT 374
#define DDOT 375
#define UNARYMINUS 376
#define UNARYPLUS 377
#define BEGINENV 378
#define ENDENV 379
#define MATRIX 380
#define PMATRIX 381
#define BMATRIX 382
#define BBMATRIX 383
#define VMATRIX 384
#define VVMATRIX 385
#define SVG 386
#define ENDSVG 387
#define SMALLMATRIX 388
#define CASES 389
#define ALIGNED 390
#define GATHERED 391
#define SUBSTACK 392
#define PMOD 393
#define RMCHAR 394
#define COLOR 395
#define BGCOLOR 396




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE itex2MML_yylval;

