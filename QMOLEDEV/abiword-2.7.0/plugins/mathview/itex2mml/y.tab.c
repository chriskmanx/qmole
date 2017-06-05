/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0

/* Substitute the variable and function names.  */
#define yyparse itex2MML_yyparse
#define yylex   itex2MML_yylex
#define yyerror itex2MML_yyerror
#define yylval  itex2MML_yylval
#define yychar  itex2MML_yychar
#define yydebug itex2MML_yydebug
#define yynerrs itex2MML_yynerrs


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




/* Copy the first part of user declarations.  */
#line 5 "itex2MML.y"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "itex2MML.h"

#define YYSTYPE char *
#define YYPARSE_PARAM_TYPE char **
#define YYPARSE_PARAM ret_str

#define yytext itex2MML_yytext

 extern int yylex ();

 extern char * yytext;

 static void itex2MML_default_error (const char * msg)
   {
     if (msg)
       fprintf(stderr, "Line: %d Error: %s\n", itex2MML_lineno, msg);
   }

 void (*itex2MML_error) (const char * msg) = itex2MML_default_error;

 static void yyerror (char * s)
   {
     char * msg = itex2MML_copy3 (s, " at token ", yytext);
     if (itex2MML_error)
       (*itex2MML_error) (msg);
     itex2MML_free_string (msg);
   }

 /* Note: If length is 0, then buffer is treated like a string; otherwise only length bytes are written.
  */
 static void itex2MML_default_write (const char * buffer, unsigned long length)
   {
     if (buffer)
       {
	 if (length)
	   fwrite (buffer, 1, length, stdout);
	 else
	   fputs (buffer, stdout);
       }
   }

 static void itex2MML_default_write_mathml (const char * mathml)
   {
     if (itex2MML_write)
       (*itex2MML_write) (mathml, 0);
   }

#ifdef itex2MML_CAPTURE
    static char * itex2MML_output_string = "" ;

    const char * itex2MML_output ()
    {
        char * copy = (char *) malloc(strlen(itex2MML_output_string) +1);
        if (copy)
          {
           if (itex2MML_output_string)
             {
               strcpy(copy, itex2MML_output_string);
               if (itex2MML_output_string != "")
                   free(itex2MML_output_string);
             }
           else
             copy[0] = 0;
          }
        itex2MML_output_string = "";
        return copy;
    }

 static void itex2MML_capture (const char * buffer, unsigned long length)
    {
     if (buffer)
       {
         if (length)
           {
              unsigned long first_length = itex2MML_output_string ? strlen(itex2MML_output_string) : 0;
              char * copy  = (char *) malloc(first_length + length + 1);
              if (copy)
                {
                  if (itex2MML_output_string)
                    {
                       strcpy(copy, itex2MML_output_string);
                       if (itex2MML_output_string != "")
                          free(itex2MML_output_string);
                    }
                  else
                     copy[0] = 0;
                  strncat(copy, buffer, length);
                 }
              itex2MML_output_string = copy;
            }
         else
            {
              char * copy = itex2MML_copy2(itex2MML_output_string, buffer);
              if (itex2MML_output_string != "")
                 free(itex2MML_output_string);
              itex2MML_output_string = copy;
            }
        }
    }

    static void itex2MML_capture_mathml (const char * buffer)
    {
       char * temp = itex2MML_copy2(itex2MML_output_string, buffer);
       if (itex2MML_output_string != "")
         free(itex2MML_output_string);
       itex2MML_output_string = temp;
    }
    void (*itex2MML_write) (const char * buffer, unsigned long length) = itex2MML_capture;
    void (*itex2MML_write_mathml) (const char * mathml) = itex2MML_capture_mathml;
#else
    void (*itex2MML_write) (const char * buffer, unsigned long length) = itex2MML_default_write;
    void (*itex2MML_write_mathml) (const char * mathml) = itex2MML_default_write_mathml;
#endif 

 char * itex2MML_empty_string = "";

 /* Create a copy of a string, adding space for extra chars
  */
 char * itex2MML_copy_string_extra (const char * str, unsigned extra)
   {
     char * copy = (char *) malloc(extra + (str ? strlen (str) : 0) + 1);
     if (copy)
       {
	 if (str)
	   strcpy(copy, str);
	 else
	   copy[0] = 0;
       }
     return copy ? copy : itex2MML_empty_string;
   }

 /* Create a copy of a string, appending two strings
  */
 char * itex2MML_copy3 (const char * first, const char * second, const char * third)
   {
     int  first_length =  first ? strlen( first) : 0;
     int second_length = second ? strlen(second) : 0;
     int  third_length =  third ? strlen( third) : 0;

     char * copy = (char *) malloc(first_length + second_length + third_length + 1);

     if (copy)
       {
	 if (first)
	   strcpy(copy, first);
	 else
	   copy[0] = 0;

	 if (second) strcat(copy, second);
	 if ( third) strcat(copy,  third);
       }
     return copy ? copy : itex2MML_empty_string;
   }

 /* Create a copy of a string, appending a second string
  */
 char * itex2MML_copy2 (const char * first, const char * second)
   {
     return itex2MML_copy3(first, second, 0);
   }

 /* Create a copy of a string
  */
 char * itex2MML_copy_string (const char * str)
   {
     return itex2MML_copy3(str, 0, 0);
   }

 /* Create a copy of a string, escaping unsafe characters for XML
  */
 char * itex2MML_copy_escaped (const char * str)
   {
     unsigned long length = 0;

     const char * ptr1 = str;

     char * ptr2 = 0;
     char * copy = 0;

     if ( str == 0) return itex2MML_empty_string;
     if (*str == 0) return itex2MML_empty_string;

     while (*ptr1)
       {
	 switch (*ptr1)
	   {
	   case '<':  /* &lt;   */
	   case '>':  /* &gt;   */
	     length += 4;
	     break;
	   case '&':  /* &amp;  */
	     length += 5;
	     break;
	   case '\'': /* &apos; */
	   case '"':  /* &quot; */
	   case '-':  /* &#x2d; */
	     length += 6;
	     break;
	   default:
	     length += 1;
	     break;
	   }
	 ++ptr1;
       }

     copy = (char *) malloc (length + 1);

     if (copy)
       {
	 ptr1 = str;
	 ptr2 = copy;

	 while (*ptr1)
	   {
	     switch (*ptr1)
	       {
	       case '<':
		 strcpy (ptr2, "&lt;");
		 ptr2 += 4;
		 break;
	       case '>':
		 strcpy (ptr2, "&gt;");
		 ptr2 += 4;
		 break;
	       case '&':  /* &amp;  */
		 strcpy (ptr2, "&amp;");
		 ptr2 += 5;
		 break;
	       case '\'': /* &apos; */
		 strcpy (ptr2, "&apos;");
		 ptr2 += 6;
		 break;
	       case '"':  /* &quot; */
		 strcpy (ptr2, "&quot;");
		 ptr2 += 6;
		 break;
	       case '-':  /* &#x2d; */
		 strcpy (ptr2, "&#x2d;");
		 ptr2 += 6;
		 break;
	       default:
		 *ptr2++ = *ptr1;
		 break;
	       }
	     ++ptr1;
	   }
	 *ptr2 = 0;
       }
     return copy ? copy : itex2MML_empty_string;
   }

 /* Create a hex character reference string corresponding to code
  */
 char * itex2MML_character_reference (unsigned long int code)
   {
#define ENTITY_LENGTH 10
     char * entity = (char *) malloc(ENTITY_LENGTH);
     sprintf(entity, "&#x%05x;", code);
     return entity;
   }

 void itex2MML_free_string (char * str)
   {
     if (str && str != itex2MML_empty_string)
       free(str);
   }



/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 670 "y.tab.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  165
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   2768

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  142
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  103
/* YYNRULES -- Number of rules.  */
#define YYNRULES  277
/* YYNRULES -- Number of states.  */
#define YYNSTATES  482

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   396

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     5,     6,     8,    10,    13,    16,    18,
      21,    24,    28,    32,    34,    37,    43,    47,    53,    57,
      63,    67,    73,    77,    83,    89,    93,    97,   100,   103,
     105,   107,   109,   111,   113,   115,   117,   119,   121,   123,
     125,   127,   129,   131,   133,   135,   137,   139,   141,   143,
     145,   147,   149,   151,   153,   155,   157,   159,   161,   163,
     165,   167,   169,   171,   173,   175,   177,   179,   181,   183,
     185,   187,   189,   191,   193,   195,   197,   199,   201,   203,
     205,   207,   209,   211,   213,   215,   217,   221,   225,   229,
     231,   233,   235,   237,   240,   243,   246,   249,   252,   255,
     258,   261,   264,   267,   270,   273,   276,   279,   282,   285,
     288,   291,   294,   297,   300,   303,   306,   309,   312,   315,
     317,   319,   321,   323,   325,   327,   329,   331,   333,   335,
     337,   339,   341,   343,   345,   347,   349,   351,   353,   356,
     358,   369,   373,   377,   381,   385,   389,   393,   396,   399,
     402,   405,   408,   411,   414,   417,   420,   425,   427,   430,
     435,   437,   440,   442,   444,   446,   451,   453,   456,   458,
     463,   465,   468,   470,   472,   474,   476,   478,   480,   482,
     485,   489,   495,   499,   508,   515,   522,   524,   527,   532,
     535,   538,   542,   546,   550,   553,   559,   565,   571,   577,
     581,   584,   587,   590,   593,   596,   599,   602,   605,   608,
     611,   614,   617,   620,   623,   626,   630,   634,   638,   643,
     649,   655,   661,   667,   673,   679,   685,   691,   697,   703,
     708,   712,   717,   722,   731,   733,   736,   738,   740,   742,
     744,   746,   748,   750,   752,   754,   756,   759,   762,   765,
     768,   771,   774,   777,   780,   783,   786,   788,   792,   794,
     796,   798,   802,   808,   810,   813,   815,   817,   818,   820,
     826,   828,   831,   833,   835,   837,   839,   842
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
     143,     0,    -1,   144,    -1,    -1,   145,    -1,   146,    -1,
     144,   145,    -1,   144,   146,    -1,     5,    -1,     6,     8,
      -1,     7,     8,    -1,     6,   147,     8,    -1,     7,   147,
       8,    -1,   148,    -1,   147,   148,    -1,   159,    14,   149,
      13,   149,    -1,   159,    14,   149,    -1,   159,    13,   149,
      14,   149,    -1,   159,    13,   149,    -1,   157,    14,   149,
      13,   149,    -1,   157,    14,   149,    -1,   157,    13,   149,
      14,   149,    -1,   157,    13,   149,    -1,   149,    14,   149,
      13,   149,    -1,   149,    13,   149,    14,   149,    -1,   149,
      14,   149,    -1,   149,    13,   149,    -1,    14,   149,    -1,
      13,   149,    -1,   149,    -1,   221,    -1,   154,    -1,   155,
      -1,   157,    -1,   156,    -1,   158,    -1,   160,    -1,   196,
      -1,   197,    -1,   200,    -1,   204,    -1,   214,    -1,   215,
      -1,   216,    -1,   217,    -1,   207,    -1,   208,    -1,   213,
      -1,   209,    -1,   210,    -1,   212,    -1,   211,    -1,   206,
      -1,   205,    -1,   218,    -1,   161,    -1,   169,    -1,   170,
      -1,   171,    -1,   172,    -1,   173,    -1,   174,    -1,   176,
      -1,   177,    -1,   178,    -1,   179,    -1,   182,    -1,   175,
      -1,   185,    -1,   162,    -1,   168,    -1,   188,    -1,   189,
      -1,   190,    -1,   191,    -1,   192,    -1,   193,    -1,   194,
      -1,   195,    -1,   163,    -1,   164,    -1,   165,    -1,   166,
      -1,   167,    -1,   202,    -1,   203,    -1,    15,   149,    16,
      -1,    15,   147,    16,    -1,   150,   147,   151,    -1,   219,
      -1,   220,    -1,   201,    -1,   153,    -1,    17,    36,    -1,
      17,    35,    -1,    17,    34,    -1,    18,    37,    -1,    18,
      35,    -1,    18,    34,    -1,    19,    36,    -1,    19,    37,
      -1,    19,    35,    -1,    20,    36,    -1,    20,    37,    -1,
      20,    35,    -1,    21,    36,    -1,    21,    37,    -1,    21,
      35,    -1,    22,    36,    -1,    22,    37,    -1,    22,    35,
      -1,    23,    36,    -1,    23,    35,    -1,    24,    36,    -1,
      24,    35,    -1,    25,    36,    -1,    25,    35,    -1,    26,
      36,    -1,    26,    35,    -1,    99,    -1,   121,    -1,   122,
      -1,     9,    -1,    10,    -1,    11,    -1,    39,    -1,   159,
      -1,   152,    -1,    12,    -1,    31,    -1,    32,    -1,    37,
      -1,    36,    -1,    35,    -1,    33,    -1,    34,    -1,    38,
      -1,    30,    -1,    29,    86,    -1,   100,    -1,   105,    70,
     106,    71,    70,   107,    71,    70,   108,    71,    -1,   101,
      86,   149,    -1,   102,   149,   149,    -1,   103,    64,   149,
      -1,   104,    64,   149,    -1,   140,    64,   147,    -1,   141,
      64,   147,    -1,    85,    86,    -1,    83,   149,    -1,    84,
     149,    -1,    80,   149,    -1,    81,   149,    -1,    82,   149,
      -1,    65,   149,    -1,    67,   149,    -1,    66,   149,    -1,
      68,    70,   178,    71,    -1,   139,    -1,   178,   139,    -1,
      69,    70,   180,    71,    -1,   181,    -1,   180,   181,    -1,
      72,    -1,    73,    -1,    74,    -1,    78,    70,   183,    71,
      -1,   184,    -1,   183,   184,    -1,    76,    -1,    77,    70,
     186,    71,    -1,   187,    -1,   186,   187,    -1,    75,    -1,
      91,    -1,    92,    -1,    93,    -1,    94,    -1,    95,    -1,
      96,    -1,    97,   149,    -1,    98,    86,   149,    -1,    48,
     149,    15,   198,    16,    -1,    48,   149,   198,    -1,    49,
      15,   198,    16,   149,    15,   198,    16,    -1,    49,    15,
     198,    16,   149,   100,    -1,    49,   100,   149,    15,   198,
      16,    -1,   199,    -1,   198,   199,    -1,    14,   149,    13,
     149,    -1,    14,   149,    -1,    13,   149,    -1,    14,    13,
     149,    -1,    27,   149,   149,    -1,    28,   149,   149,    -1,
     138,   149,    -1,    15,   147,     4,   147,    16,    -1,   150,
     147,     4,   147,   151,    -1,    15,   147,     3,   147,    16,
      -1,   150,   147,     3,   147,   151,    -1,    42,   149,   149,
      -1,    46,   149,    -1,    45,   149,    -1,   109,   149,    -1,
     110,   149,    -1,   111,   149,    -1,   112,   149,    -1,   119,
     149,    -1,   120,   149,    -1,   117,   149,    -1,   118,   149,
      -1,   115,   149,    -1,   116,   149,    -1,   113,   149,    -1,
     114,   149,    -1,    40,   149,    -1,    41,   149,   149,    -1,
      43,   149,   149,    -1,    44,   149,   149,    -1,    47,   149,
     149,   149,    -1,   123,   125,   234,   124,   125,    -1,   123,
     136,   234,   124,   136,    -1,   123,   126,   234,   124,   126,
      -1,   123,   127,   234,   124,   127,    -1,   123,   129,   234,
     124,   129,    -1,   123,   128,   234,   124,   128,    -1,   123,
     130,   234,   124,   130,    -1,   123,   133,   234,   124,   133,
      -1,   123,   134,   234,   124,   134,    -1,   123,   135,   234,
     124,   135,    -1,   123,   131,    87,   132,    -1,   123,   131,
     132,    -1,   137,    15,   234,    16,    -1,    50,    15,   234,
      16,    -1,    50,    15,    53,    15,   222,    16,   234,    16,
      -1,   223,    -1,   222,   223,    -1,   224,    -1,   225,    -1,
     226,    -1,   227,    -1,   228,    -1,   229,    -1,   230,    -1,
     231,    -1,   232,    -1,   233,    -1,    54,    64,    -1,    55,
      64,    -1,    56,    64,    -1,    57,    64,    -1,    58,    64,
      -1,    59,    64,    -1,    60,    64,    -1,    61,    64,    -1,
      62,    64,    -1,    63,    64,    -1,   235,    -1,   234,    52,
     235,    -1,   236,    -1,   237,    -1,   240,    -1,   236,    51,
     240,    -1,    79,    15,   238,    16,   236,    -1,   239,    -1,
     238,   239,    -1,   225,    -1,   226,    -1,    -1,   147,    -1,
      88,    15,   241,    16,   147,    -1,   242,    -1,   241,   242,
      -1,   225,    -1,   226,    -1,   243,    -1,   244,    -1,    89,
      64,    -1,    90,    64,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   284,   284,   287,   288,   289,   290,   291,   293,   295,
     296,   297,   310,   324,   328,   334,   353,   367,   386,   400,
     419,   433,   452,   466,   476,   486,   493,   500,   504,   508,
     513,   514,   515,   516,   517,   521,   525,   526,   527,   528,
     529,   530,   531,   532,   533,   534,   535,   536,   537,   538,
     539,   540,   541,   542,   543,   544,   545,   546,   547,   548,
     549,   550,   551,   552,   553,   554,   555,   556,   557,   558,
     559,   560,   561,   562,   563,   564,   565,   566,   567,   568,
     569,   570,   571,   572,   573,   574,   575,   579,   583,   591,
     592,   593,   594,   596,   601,   606,   612,   616,   620,   625,
     630,   634,   638,   643,   647,   651,   656,   660,   664,   669,
     673,   677,   682,   687,   692,   697,   702,   707,   712,   718,
     722,   726,   730,   732,   738,   740,   746,   747,   748,   753,
     758,   763,   767,   772,   776,   780,   784,   789,   794,   800,
     804,   815,   823,   831,   839,   847,   854,   862,   867,   872,
     877,   882,   887,   892,   897,   902,   907,   912,   916,   922,
     927,   931,   937,   941,   945,   953,   958,   962,   968,   973,
     978,   982,   988,   993,   997,  1001,  1005,  1009,  1013,  1017,
    1022,  1030,  1037,  1045,  1055,  1064,  1072,  1076,  1082,  1087,
    1091,  1095,  1100,  1107,  1115,  1120,  1127,  1141,  1148,  1162,
    1170,  1175,  1180,  1184,  1189,  1193,  1198,  1203,  1208,  1212,
    1217,  1221,  1226,  1230,  1235,  1240,  1248,  1256,  1264,  1273,
    1277,  1281,  1285,  1289,  1293,  1297,  1301,  1305,  1309,  1313,
    1317,  1321,  1326,  1330,  1338,  1342,  1348,  1352,  1356,  1360,
    1364,  1368,  1372,  1376,  1380,  1384,  1389,  1394,  1399,  1404,
    1409,  1414,  1419,  1424,  1429,  1434,  1441,  1445,  1451,  1455,
    1460,  1464,  1470,  1478,  1482,  1488,  1492,  1497,  1500,  1504,
    1512,  1516,  1522,  1526,  1530,  1534,  1539,  1544
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "TEXATOP", "TEXOVER", "CHAR",
  "STARTMATH", "STARTDMATH", "ENDMATH", "MI", "MIB", "MN", "MO", "SUP",
  "SUB", "MROWOPEN", "MROWCLOSE", "LEFT", "RIGHT", "BIG", "BBIG", "BIGG",
  "BBIGG", "BIGL", "BBIGL", "BIGGL", "BBIGGL", "FRAC", "TFRAC", "MATHOP",
  "MOP", "MOL", "MOLL", "MOF", "PERIODDELIM", "OTHERDELIM", "LEFTDELIM",
  "RIGHTDELIM", "MOS", "MOB", "SQRT", "ROOT", "BINOM", "UNDER", "OVER",
  "OVERBRACE", "UNDERBRACE", "UNDEROVER", "TENSOR", "MULTI", "ARRAY",
  "COLSEP", "ROWSEP", "ARRAYOPTS", "COLLAYOUT", "COLALIGN", "ROWALIGN",
  "ALIGN", "EQROWS", "EQCOLS", "ROWLINES", "COLLINES", "FRAME", "PADDING",
  "ATTRLIST", "ITALICS", "BOLD", "SLASHED", "RM", "BB", "ST", "END",
  "BBLOWERCHAR", "BBUPPERCHAR", "BBDIGIT", "CALCHAR", "FRAKCHAR", "CAL",
  "FRAK", "ROWOPTS", "TEXTSIZE", "SCSIZE", "SCSCSIZE", "DISPLAY",
  "TEXTSTY", "TEXTBOX", "TEXTSTRING", "XMLSTRING", "CELLOPTS", "ROWSPAN",
  "COLSPAN", "THINSPACE", "MEDSPACE", "THICKSPACE", "QUAD", "QQUAD",
  "NEGSPACE", "PHANTOM", "HREF", "UNKNOWNCHAR", "EMPTYMROW", "STATLINE",
  "TOGGLE", "FGHIGHLIGHT", "BGHIGHLIGHT", "SPACE", "INTONE", "INTTWO",
  "INTTHREE", "BAR", "WIDEBAR", "VEC", "WIDEVEC", "HAT", "WIDEHAT",
  "CHECK", "WIDECHECK", "TILDE", "WIDETILDE", "DOT", "DDOT", "UNARYMINUS",
  "UNARYPLUS", "BEGINENV", "ENDENV", "MATRIX", "PMATRIX", "BMATRIX",
  "BBMATRIX", "VMATRIX", "VVMATRIX", "SVG", "ENDSVG", "SMALLMATRIX",
  "CASES", "ALIGNED", "GATHERED", "SUBSTACK", "PMOD", "RMCHAR", "COLOR",
  "BGCOLOR", "$accept", "doc", "xmlmmlTermList", "char", "expression",
  "compoundTermList", "compoundTerm", "closedTerm", "left", "right",
  "bigdelim", "unrecognized", "unaryminus", "unaryplus", "mi", "mib", "mn",
  "mob", "mo", "emptymrow", "space", "statusline", "toggle", "fghighlight",
  "bghighlight", "color", "textstring", "displaystyle", "textstyle",
  "textsize", "scriptsize", "scriptscriptsize", "italics", "slashed",
  "bold", "roman", "rmchars", "bbold", "bbchars", "bbchar", "frak",
  "frakletters", "frakletter", "cal", "calletters", "calletter",
  "thinspace", "medspace", "thickspace", "quad", "qquad", "negspace",
  "phantom", "href", "tensor", "multi", "subsupList", "subsupTerm",
  "mfrac", "pmod", "texover", "texatop", "binom", "munderbrace",
  "moverbrace", "bar", "vec", "dot", "ddot", "tilde", "check", "hat",
  "msqrt", "mroot", "munder", "mover", "munderover", "mathenv", "substack",
  "array", "arrayopts", "anarrayopt", "collayout", "colalign", "rowalign",
  "align", "eqrows", "eqcols", "rowlines", "collines", "frame", "padding",
  "tableRowList", "tableRow", "simpleTableRow", "optsTableRow", "rowopts",
  "arowopt", "tableCell", "cellopts", "acellopt", "rowspan", "colspan", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,   372,   373,   374,
     375,   376,   377,   378,   379,   380,   381,   382,   383,   384,
     385,   386,   387,   388,   389,   390,   391,   392,   393,   394,
     395,   396
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,   142,   143,   144,   144,   144,   144,   144,   145,   146,
     146,   146,   146,   147,   147,   148,   148,   148,   148,   148,
     148,   148,   148,   148,   148,   148,   148,   148,   148,   148,
     149,   149,   149,   149,   149,   149,   149,   149,   149,   149,
     149,   149,   149,   149,   149,   149,   149,   149,   149,   149,
     149,   149,   149,   149,   149,   149,   149,   149,   149,   149,
     149,   149,   149,   149,   149,   149,   149,   149,   149,   149,
     149,   149,   149,   149,   149,   149,   149,   149,   149,   149,
     149,   149,   149,   149,   149,   149,   149,   149,   149,   149,
     149,   149,   149,   150,   150,   150,   151,   151,   151,   152,
     152,   152,   152,   152,   152,   152,   152,   152,   152,   152,
     152,   152,   152,   152,   152,   152,   152,   152,   152,   153,
     154,   155,   156,   157,   158,   159,   160,   160,   160,   160,
     160,   160,   160,   160,   160,   160,   160,   160,   160,   161,
     162,   163,   164,   165,   166,   167,   167,   168,   169,   170,
     171,   172,   173,   174,   175,   176,   177,   178,   178,   179,
     180,   180,   181,   181,   181,   182,   183,   183,   184,   185,
     186,   186,   187,   188,   189,   190,   191,   192,   193,   194,
     195,   196,   196,   197,   197,   197,   198,   198,   199,   199,
     199,   199,   200,   200,   201,   202,   202,   203,   203,   204,
     205,   206,   207,   207,   208,   208,   209,   210,   211,   211,
     212,   212,   213,   213,   214,   215,   216,   217,   218,   219,
     219,   219,   219,   219,   219,   219,   219,   219,   219,   219,
     219,   220,   221,   221,   222,   222,   223,   223,   223,   223,
     223,   223,   223,   223,   223,   223,   224,   225,   226,   227,
     228,   229,   230,   231,   232,   233,   234,   234,   235,   235,
     236,   236,   237,   238,   238,   239,   239,   240,   240,   240,
     241,   241,   242,   242,   242,   242,   243,   244
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     0,     1,     1,     2,     2,     1,     2,
       2,     3,     3,     1,     2,     5,     3,     5,     3,     5,
       3,     5,     3,     5,     5,     3,     3,     2,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     3,     3,     3,     1,
       1,     1,     1,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     2,     1,
      10,     3,     3,     3,     3,     3,     3,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     4,     1,     2,     4,
       1,     2,     1,     1,     1,     4,     1,     2,     1,     4,
       1,     2,     1,     1,     1,     1,     1,     1,     1,     2,
       3,     5,     3,     8,     6,     6,     1,     2,     4,     2,
       2,     3,     3,     3,     2,     5,     5,     5,     5,     3,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     3,     3,     3,     4,     5,
       5,     5,     5,     5,     5,     5,     5,     5,     5,     4,
       3,     4,     4,     8,     1,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     3,     1,     1,
       1,     3,     5,     1,     2,     1,     1,     0,     1,     5,
       1,     2,     1,     1,     1,     1,     2,     2
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       3,     8,     0,     0,     0,     2,     4,     5,     9,   122,
     123,   124,   128,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   137,   129,
     130,   134,   135,   133,   132,   131,   136,   125,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   173,   174,   175,   176,   177,   178,     0,     0,
     119,   139,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   120,
     121,     0,     0,     0,   157,     0,     0,     0,    13,    29,
       0,   127,    92,    31,    32,    34,    33,    35,   126,    36,
      55,    69,    79,    80,    81,    82,    83,    70,    56,    57,
      58,    59,    60,    61,    67,    62,    63,    64,    65,    66,
      68,    71,    72,    73,    74,    75,    76,    77,    78,    37,
      38,    39,    91,    84,    85,    40,    53,    52,    45,    46,
      48,    49,    51,    50,    47,    41,    42,    43,    44,    54,
      89,    90,    30,    10,     0,     1,     6,     7,    28,    33,
     126,    27,     0,    29,    95,    94,    93,   101,    99,   100,
     104,   102,   103,   107,   105,   106,   110,   108,   109,   112,
     111,   114,   113,   116,   115,   118,   117,     0,     0,   138,
     214,     0,     0,     0,     0,   201,   200,     0,     0,     0,
       0,   267,   153,   155,   154,     0,     0,     0,     0,   150,
     151,   152,   148,   149,   147,   179,     0,     0,     0,     0,
       0,     0,   202,   203,   204,   205,   212,   213,   210,   211,
     208,   209,   206,   207,   267,   267,   267,   267,   267,   267,
       0,   267,   267,   267,   267,   267,   194,     0,     0,    11,
      14,     0,     0,     0,     0,     0,     0,     0,   158,    12,
       0,     0,    87,    86,   192,   193,   215,   199,   216,   217,
       0,     0,     0,     0,   182,   186,     0,     0,     0,     0,
       0,   268,     0,   256,   258,   259,   260,     0,   162,   163,
     164,     0,   160,   172,     0,   170,   168,     0,   166,   180,
     141,   142,   143,   144,     0,     0,     0,     0,     0,     0,
       0,     0,   230,     0,     0,     0,     0,     0,   145,   146,
      26,    25,     0,     0,     0,    88,    22,    20,    18,    16,
       0,     0,   218,   190,     0,   189,     0,   187,     0,     0,
       0,     0,     0,   232,   267,   267,   156,   159,   161,   169,
     171,   165,   167,     0,     0,     0,     0,     0,     0,     0,
     229,     0,     0,     0,     0,   231,     0,     0,     0,     0,
      98,    97,    96,     0,     0,     0,     0,   197,   195,   191,
       0,   181,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   234,   236,   237,   238,   239,
     240,   241,   242,   243,   244,   245,   265,   266,     0,   263,
       0,     0,   272,   273,     0,   270,   274,   275,   257,   261,
       0,   219,   221,   222,   224,   223,   225,   226,   227,   228,
     220,    24,    23,   198,   196,    21,    19,    17,    15,   188,
       0,   184,   185,   246,   247,   248,   249,   250,   251,   252,
     253,   254,   255,   267,   235,   267,   264,   276,   277,     0,
     271,     0,     0,     0,   262,   269,     0,   183,   233,     0,
       0,   140
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     4,     5,     6,     7,   291,    98,    99,   100,   335,
     101,   102,   103,   104,   105,   169,   107,   170,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   301,   302,
     129,   307,   308,   130,   304,   305,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   284,   285,   141,   142,
     143,   144,   145,   146,   147,   148,   149,   150,   151,   152,
     153,   154,   155,   156,   157,   158,   159,   160,   161,   162,
     404,   405,   406,   407,   408,   409,   410,   411,   412,   413,
     414,   415,   292,   293,   294,   295,   418,   419,   296,   424,
     425,   426,   427
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -341
static const yytype_int16 yypact[] =
{
     167,  -341,  1028,  1162,    50,   167,  -341,  -341,  -341,  -341,
    -341,  -341,  -341,  2627,  2627,  2361,   141,   143,   146,   149,
     152,    21,    61,    68,   165,  2627,  2627,    -9,  -341,  -341,
    -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,  2627,  2627,
    2627,  2627,  2627,  2627,  2627,  2627,  2627,   -12,    39,  2627,
    2627,  2627,    -8,     1,     3,     9,  2627,  2627,  2627,  2627,
    2627,     5,  -341,  -341,  -341,  -341,  -341,  -341,  2627,     8,
    -341,  -341,    26,  2627,    81,    96,    95,  2627,  2627,  2627,
    2627,  2627,  2627,  2627,  2627,  2627,  2627,  2627,  2627,  -341,
    -341,     6,   183,  2627,  -341,   103,   144,  1296,  -341,    52,
    2361,  -341,  -341,  -341,  -341,  -341,   189,  -341,   191,  -341,
    -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,
    -341,  -341,  -341,  -341,  -341,  -341,  -341,   -87,  -341,  -341,
    -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,
    -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,
    -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,
    -341,  -341,  -341,  -341,  1430,  -341,  -341,  -341,  -341,  -341,
    -341,  -341,   755,    56,  -341,  -341,  -341,  -341,  -341,  -341,
    -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,
    -341,  -341,  -341,  -341,  -341,  -341,  -341,  2627,  2627,  -341,
    -341,  2627,  2627,  2627,  2627,  -341,  -341,  2627,   179,   193,
    2627,  1563,  -341,  -341,  -341,    71,   123,   140,   142,  -341,
    -341,  -341,  -341,  -341,  -341,  -341,  2627,  2627,  2627,  2627,
    2627,   110,  -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,
    -341,  -341,  -341,  -341,  1696,  1696,  1696,  1696,  1696,  1696,
     -77,  1696,  1696,  1696,  1696,  1696,  -341,  2361,  2361,  -341,
    -341,  2627,  2627,   894,  2627,  2627,  2627,  2627,  -341,  -341,
    2361,  2361,  -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,
    2627,  2627,  2494,   193,   193,  -341,   112,   202,   204,   205,
     206,  2361,    23,  -341,   171,  -341,  -341,   -65,  -341,  -341,
    -341,   -13,  -341,  -341,   -28,  -341,  -341,   -31,  -341,  -341,
    -341,  -341,  -341,  -341,   153,   -44,   -32,   -24,   -19,   -18,
     -17,    91,  -341,   -16,   -15,   -14,   -11,    35,  2361,  2361,
     211,   213,  2361,  2361,   109,  -341,   214,   217,   218,   221,
    1829,  1962,  -341,  -341,  2627,   222,   145,  -341,  2627,   193,
      94,   156,    12,  -341,  1696,  2095,  -341,  -341,  -341,  -341,
    -341,  -341,  -341,   157,   106,   111,   113,   108,   125,   128,
    -341,   126,   105,   127,   124,  -341,  2627,  2627,  2228,  2228,
    -341,  -341,  -341,  2627,  2627,  2627,  2627,  -341,  -341,  -341,
    2627,  -341,   -10,   150,   177,   199,   200,   201,   203,   219,
     224,   227,   228,   229,    60,  -341,  -341,  -341,  -341,  -341,
    -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,    30,  -341,
     232,   237,  -341,  -341,    -7,  -341,  -341,  -341,  -341,  -341,
     159,  -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,
    -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,
     193,  -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,
    -341,  -341,  -341,  1696,  -341,  2095,  -341,  -341,  -341,  2361,
    -341,   231,   155,    37,   171,  2361,   207,  -341,  -341,   162,
     233,  -341
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -341,  -341,  -341,   298,   300,    29,   -34,   371,  -341,  -165,
    -341,  -341,  -341,  -341,  -341,    -2,  -341,    27,  -341,  -341,
    -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,
    -341,  -341,  -341,  -341,  -341,  -341,    93,  -341,  -341,    10,
    -341,  -341,     2,  -341,  -341,    11,  -341,  -341,  -341,  -341,
    -341,  -341,  -341,  -341,  -341,  -341,  -202,  -282,  -341,  -341,
    -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,
    -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,  -341,
    -341,   -94,  -341,  -340,  -325,  -341,  -341,  -341,  -341,  -341,
    -341,  -341,  -230,   -42,  -152,  -341,  -341,  -104,   -39,  -341,
    -107,  -341,  -341
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_uint16 yytable[] =
{
     106,   106,   347,   209,   347,   450,   356,   286,   354,   469,
     321,   416,   422,   106,   315,   316,   317,   318,   319,   320,
     354,   323,   324,   325,   326,   327,   417,   423,   354,   108,
     108,    97,   164,   354,   354,   354,   354,   354,   354,   353,
     361,   354,   108,   359,   172,   306,   465,   303,   395,   396,
     165,   375,   268,   478,   211,   322,   189,   190,   357,   298,
     299,   300,   215,   260,   347,   261,   262,   395,   396,   261,
     262,   216,   273,   217,   268,   354,   463,   199,   416,   218,
     364,   346,   420,   421,   422,   395,   396,   354,   210,   354,
     451,   224,   365,   417,   226,   106,   191,   192,   106,   423,
     366,   420,   421,   193,   194,   367,   368,   369,   371,   372,
     373,   347,   227,   374,   394,   395,   396,   397,   398,   399,
     400,   401,   402,   403,   108,   281,   282,   108,   348,   263,
     260,   244,   245,   246,   247,   248,   249,   250,   260,   251,
     252,   253,   254,   380,   381,   229,   382,   393,   394,   395,
     396,   397,   398,   399,   400,   401,   402,   403,   281,   282,
     230,   391,   106,   281,   282,   231,   452,   257,   281,   282,
     106,   477,     1,     2,     3,   174,   175,   176,   177,   178,
     179,   180,   181,   182,   183,   184,   185,   186,   187,   188,
     347,   108,   281,   282,   283,   298,   299,   300,   255,   108,
     195,   196,   264,   265,   266,   267,   281,   282,   258,   106,
      94,   395,   396,   443,   444,   303,   314,   349,   306,   350,
     351,   352,   355,   370,   363,   376,   377,   430,   383,   260,
     384,   431,   385,   473,   386,   390,   434,   432,   108,   438,
     433,   453,   106,   106,   106,   106,   106,   106,   472,   106,
     106,   106,   106,   106,   435,   106,   106,   260,   436,   437,
     440,   106,   439,   454,   455,   456,   471,   457,   106,   106,
     480,   108,   108,   108,   108,   108,   108,   479,   108,   108,
     108,   108,   108,   458,   108,   108,   328,   329,   459,   106,
     108,   460,   461,   462,   260,   260,   467,   108,   108,   340,
     341,   468,   476,   166,   481,   167,   260,   260,   297,   362,
     464,   358,   428,   474,   466,   360,   429,   470,   108,     0,
       0,     0,     0,     0,     0,     0,   106,   106,     0,     0,
     106,   106,     0,     0,     0,     0,     0,     0,   106,   106,
       0,     0,     0,     0,   260,   260,     0,     0,     0,     0,
       0,     0,   106,   106,     0,   108,   108,     0,     0,   108,
     108,   378,   379,     0,     0,     0,     0,   108,   108,     0,
       0,     0,     0,     0,     0,     0,   106,   106,     0,     0,
       0,   108,   108,     0,   168,   171,   173,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   197,   198,     0,     0,
       0,     0,     0,     0,     0,   108,   108,     0,     0,   200,
     201,   202,   203,   204,   205,   206,   207,   208,     0,     0,
     212,   213,   214,     0,     0,     0,     0,   219,   220,   221,
     222,   223,     0,     0,     0,     0,     0,     0,     0,   225,
       0,   260,     0,     0,   228,     0,     0,     0,   232,   233,
     234,   235,   236,   237,   238,   239,   240,   241,   242,   243,
       0,   106,     0,   106,   256,     0,     0,   106,     0,     0,
       0,     0,     0,   106,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     108,     0,   108,     0,     0,     0,   108,     0,   475,     0,
       0,     0,   108,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   274,   275,
       0,     0,   276,   277,   278,   279,     0,     0,   280,     0,
       0,   287,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   309,   310,   311,
     312,   313,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   330,   331,     0,   336,   337,   338,   339,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   342,   343,   345,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   389,     0,     0,     0,   392,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   441,   442,     0,
       0,     0,     0,     0,   445,   446,   447,   448,   270,   271,
       0,   449,     0,     0,     9,    10,    11,    12,    13,    14,
      15,   272,    16,     0,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      49,    50,    51,    52,    53,     0,     0,     0,     0,     0,
       0,     0,    54,    55,     0,    56,    57,    58,    59,    60,
      61,     0,     0,     0,     0,     0,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,     0,     0,     0,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    92,    93,    94,    95,    96,   332,   333,     0,
       0,     0,     0,     9,    10,    11,    12,    13,    14,    15,
       0,    16,   334,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    49,
      50,    51,    52,    53,     0,     0,     0,     0,     0,     0,
       0,    54,    55,     0,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
       0,     0,     0,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    92,    93,    94,    95,    96,     8,     9,    10,    11,
      12,    13,    14,    15,     0,    16,     0,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    49,    50,    51,    52,    53,     0,     0,
       0,     0,     0,     0,     0,    54,    55,     0,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,     0,     0,     0,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    92,    93,    94,    95,    96,
     163,     9,    10,    11,    12,    13,    14,    15,     0,    16,
       0,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    49,    50,    51,
      52,    53,     0,     0,     0,     0,     0,     0,     0,    54,
      55,     0,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,     0,     0,
       0,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    92,
      93,    94,    95,    96,   259,     9,    10,    11,    12,    13,
      14,    15,     0,    16,     0,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    49,    50,    51,    52,    53,     0,     0,     0,     0,
       0,     0,     0,    54,    55,     0,    56,    57,    58,    59,
      60,    61,     0,     0,     0,     0,     0,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,     0,     0,     0,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    92,    93,    94,    95,    96,   269,     9,
      10,    11,    12,    13,    14,    15,     0,    16,     0,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    49,    50,    51,    52,    53,
       0,     0,     0,     0,     0,     0,     0,    54,    55,     0,
      56,    57,    58,    59,    60,    61,     0,     0,     0,     0,
       0,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,     0,     0,     0,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    92,    93,    94,
      95,    96,     9,    10,    11,    12,    13,    14,    15,     0,
      16,     0,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,     0,     0,   288,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    49,    50,
      51,    52,    53,     0,     0,     0,     0,     0,     0,     0,
      54,    55,   289,    56,    57,    58,    59,    60,    61,     0,
       0,   290,     0,     0,    62,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    73,    74,    75,    76,     0,
       0,     0,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      92,    93,    94,    95,    96,     9,    10,    11,    12,    13,
      14,    15,     0,    16,     0,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    49,    50,    51,    52,    53,     0,     0,     0,     0,
       0,     0,     0,    54,    55,   289,    56,    57,    58,    59,
      60,    61,     0,     0,   290,     0,     0,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,     0,     0,     0,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    92,    93,    94,    95,    96,     9,    10,
      11,    12,    13,    14,    15,   387,    16,     0,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    49,    50,    51,    52,    53,     0,
       0,     0,     0,     0,     0,     0,    54,    55,     0,    56,
      57,    58,    59,    60,    61,     0,     0,     0,     0,     0,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,     0,     0,     0,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    92,    93,    94,    95,
      96,     9,    10,    11,    12,    13,    14,    15,   388,    16,
       0,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    49,    50,    51,
      52,    53,     0,     0,     0,     0,     0,     0,     0,    54,
      55,     0,    56,    57,    58,    59,    60,    61,     0,     0,
       0,     0,     0,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,     0,     0,
       0,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    92,
      93,    94,    95,    96,     9,    10,    11,    12,    13,    14,
      15,     0,    16,     0,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      49,    50,    51,    52,    53,     0,     0,     0,     0,     0,
       0,     0,    54,    55,     0,    56,    57,    58,    59,    60,
      61,     0,     0,   290,     0,     0,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,     0,     0,     0,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    92,    93,    94,    95,    96,     9,    10,    11,
      12,    13,    14,    15,     0,    16,   334,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    49,    50,    51,    52,    53,     0,     0,
       0,     0,     0,     0,     0,    54,    55,     0,    56,    57,
      58,    59,    60,    61,     0,     0,     0,     0,     0,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,     0,     0,     0,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    92,    93,    94,    95,    96,
       9,    10,    11,    12,    13,    14,    15,     0,    16,     0,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    49,    50,    51,    52,
      53,     0,     0,     0,     0,     0,     0,     0,    54,    55,
       0,    56,    57,    58,    59,    60,    61,     0,     0,     0,
       0,     0,    62,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,    76,     0,     0,     0,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    92,    93,
      94,    95,    96,     9,    10,    11,    12,   344,     0,    15,
       0,    16,     0,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    49,
      50,    51,    52,    53,     0,     0,     0,     0,     0,     0,
       0,    54,    55,     0,    56,    57,    58,    59,    60,    61,
       0,     0,     0,     0,     0,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
       0,     0,     0,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    92,    93,    94,    95,    96,     9,    10,    11,    12,
       0,     0,    15,     0,    16,     0,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    49,    50,    51,    52,    53,     0,     0,     0,
       0,     0,     0,     0,    54,    55,     0,    56,    57,    58,
      59,    60,    61,     0,     0,     0,     0,     0,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    73,
      74,    75,    76,     0,     0,     0,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    92,    93,    94,    95,    96
};

static const yytype_int16 yycheck[] =
{
       2,     3,   284,    15,   286,    15,    71,   209,    52,    16,
      87,   351,   352,    15,   244,   245,   246,   247,   248,   249,
      52,   251,   252,   253,   254,   255,   351,   352,    52,     2,
       3,     2,     3,    52,    52,    52,    52,    52,    52,    16,
      71,    52,    15,    71,    15,    76,    16,    75,    55,    56,
       0,    16,   139,    16,    15,   132,    35,    36,    71,    72,
      73,    74,    70,    97,   346,    13,    14,    55,    56,    13,
      14,    70,    16,    70,   139,    52,    16,    86,   418,    70,
     124,   283,    89,    90,   424,    55,    56,    52,   100,    52,
     100,    86,   124,   418,    86,    97,    35,    36,   100,   424,
     124,    89,    90,    35,    36,   124,   124,   124,   124,   124,
     124,   393,    86,   124,    54,    55,    56,    57,    58,    59,
      60,    61,    62,    63,    97,    13,    14,   100,    16,   100,
     164,   125,   126,   127,   128,   129,   130,   131,   172,   133,
     134,   135,   136,    34,    35,    64,    37,   349,    54,    55,
      56,    57,    58,    59,    60,    61,    62,    63,    13,    14,
      64,    16,   164,    13,    14,    70,    16,    64,    13,    14,
     172,    16,     5,     6,     7,    34,    35,    36,    35,    36,
      37,    35,    36,    37,    35,    36,    37,    35,    36,    37,
     472,   164,    13,    14,    15,    72,    73,    74,    15,   172,
      35,    36,    13,    14,    13,    14,    13,    14,    64,   211,
     139,    55,    56,   378,   379,    75,   106,    15,    76,    15,
      15,    15,    51,   132,    71,    14,    13,    70,    14,   263,
      13,   125,    14,   463,    13,    13,   128,   126,   211,   134,
     127,    64,   244,   245,   246,   247,   248,   249,   450,   251,
     252,   253,   254,   255,   129,   257,   258,   291,   130,   133,
     136,   263,   135,    64,    64,    64,   107,    64,   270,   271,
     108,   244,   245,   246,   247,   248,   249,    70,   251,   252,
     253,   254,   255,    64,   257,   258,   257,   258,    64,   291,
     263,    64,    64,    64,   328,   329,    64,   270,   271,   270,
     271,    64,    71,     5,    71,     5,   340,   341,   215,   307,
     404,   301,   354,   465,   418,   304,   355,   424,   291,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   328,   329,    -1,    -1,
     332,   333,    -1,    -1,    -1,    -1,    -1,    -1,   340,   341,
      -1,    -1,    -1,    -1,   378,   379,    -1,    -1,    -1,    -1,
      -1,    -1,   354,   355,    -1,   328,   329,    -1,    -1,   332,
     333,   332,   333,    -1,    -1,    -1,    -1,   340,   341,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   378,   379,    -1,    -1,
      -1,   354,   355,    -1,    13,    14,    15,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    25,    26,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   378,   379,    -1,    -1,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    50,    51,    -1,    -1,    -1,    -1,    56,    57,    58,
      59,    60,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    68,
      -1,   475,    -1,    -1,    73,    -1,    -1,    -1,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      -1,   463,    -1,   465,    93,    -1,    -1,   469,    -1,    -1,
      -1,    -1,    -1,   475,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     463,    -1,   465,    -1,    -1,    -1,   469,    -1,   469,    -1,
      -1,    -1,   475,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   197,   198,
      -1,    -1,   201,   202,   203,   204,    -1,    -1,   207,    -1,
      -1,   210,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   226,   227,   228,
     229,   230,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   261,   262,    -1,   264,   265,   266,   267,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   280,   281,   282,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   344,    -1,    -1,    -1,   348,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   376,   377,    -1,
      -1,    -1,    -1,    -1,   383,   384,   385,   386,     3,     4,
      -1,   390,    -1,    -1,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      65,    66,    67,    68,    69,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    77,    78,    -1,    80,    81,    82,    83,    84,
      85,    -1,    -1,    -1,    -1,    -1,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,    -1,    -1,    -1,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   137,   138,   139,   140,   141,     3,     4,    -1,
      -1,    -1,    -1,     9,    10,    11,    12,    13,    14,    15,
      -1,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    65,
      66,    67,    68,    69,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    77,    78,    -1,    80,    81,    82,    83,    84,    85,
      -1,    -1,    -1,    -1,    -1,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,   101,   102,   103,   104,   105,
      -1,    -1,    -1,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   137,   138,   139,   140,   141,     8,     9,    10,    11,
      12,    13,    14,    15,    -1,    17,    -1,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    65,    66,    67,    68,    69,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    77,    78,    -1,    80,    81,
      82,    83,    84,    85,    -1,    -1,    -1,    -1,    -1,    91,
      92,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     102,   103,   104,   105,    -1,    -1,    -1,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   137,   138,   139,   140,   141,
       8,     9,    10,    11,    12,    13,    14,    15,    -1,    17,
      -1,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    65,    66,    67,
      68,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    77,
      78,    -1,    80,    81,    82,    83,    84,    85,    -1,    -1,
      -1,    -1,    -1,    91,    92,    93,    94,    95,    96,    97,
      98,    99,   100,   101,   102,   103,   104,   105,    -1,    -1,
      -1,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,
     138,   139,   140,   141,     8,     9,    10,    11,    12,    13,
      14,    15,    -1,    17,    -1,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    65,    66,    67,    68,    69,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    77,    78,    -1,    80,    81,    82,    83,
      84,    85,    -1,    -1,    -1,    -1,    -1,    91,    92,    93,
      94,    95,    96,    97,    98,    99,   100,   101,   102,   103,
     104,   105,    -1,    -1,    -1,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   137,   138,   139,   140,   141,     8,     9,
      10,    11,    12,    13,    14,    15,    -1,    17,    -1,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    65,    66,    67,    68,    69,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    77,    78,    -1,
      80,    81,    82,    83,    84,    85,    -1,    -1,    -1,    -1,
      -1,    91,    92,    93,    94,    95,    96,    97,    98,    99,
     100,   101,   102,   103,   104,   105,    -1,    -1,    -1,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,   123,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,   138,   139,
     140,   141,     9,    10,    11,    12,    13,    14,    15,    -1,
      17,    -1,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    -1,    -1,    53,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    65,    66,
      67,    68,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    -1,
      -1,    88,    -1,    -1,    91,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,   103,   104,   105,    -1,
      -1,    -1,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     137,   138,   139,   140,   141,     9,    10,    11,    12,    13,
      14,    15,    -1,    17,    -1,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    65,    66,    67,    68,    69,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    -1,    -1,    88,    -1,    -1,    91,    92,    93,
      94,    95,    96,    97,    98,    99,   100,   101,   102,   103,
     104,   105,    -1,    -1,    -1,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   137,   138,   139,   140,   141,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    65,    66,    67,    68,    69,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    77,    78,    -1,    80,
      81,    82,    83,    84,    85,    -1,    -1,    -1,    -1,    -1,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,   103,   104,   105,    -1,    -1,    -1,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   137,   138,   139,   140,
     141,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    65,    66,    67,
      68,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    77,
      78,    -1,    80,    81,    82,    83,    84,    85,    -1,    -1,
      -1,    -1,    -1,    91,    92,    93,    94,    95,    96,    97,
      98,    99,   100,   101,   102,   103,   104,   105,    -1,    -1,
      -1,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,   122,   123,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,
     138,   139,   140,   141,     9,    10,    11,    12,    13,    14,
      15,    -1,    17,    -1,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      65,    66,    67,    68,    69,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    77,    78,    -1,    80,    81,    82,    83,    84,
      85,    -1,    -1,    88,    -1,    -1,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,    -1,    -1,    -1,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   137,   138,   139,   140,   141,     9,    10,    11,
      12,    13,    14,    15,    -1,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    65,    66,    67,    68,    69,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    77,    78,    -1,    80,    81,
      82,    83,    84,    85,    -1,    -1,    -1,    -1,    -1,    91,
      92,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     102,   103,   104,   105,    -1,    -1,    -1,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   137,   138,   139,   140,   141,
       9,    10,    11,    12,    13,    14,    15,    -1,    17,    -1,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    65,    66,    67,    68,
      69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    77,    78,
      -1,    80,    81,    82,    83,    84,    85,    -1,    -1,    -1,
      -1,    -1,    91,    92,    93,    94,    95,    96,    97,    98,
      99,   100,   101,   102,   103,   104,   105,    -1,    -1,    -1,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,   122,   123,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   137,   138,
     139,   140,   141,     9,    10,    11,    12,    13,    -1,    15,
      -1,    17,    -1,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    65,
      66,    67,    68,    69,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    77,    78,    -1,    80,    81,    82,    83,    84,    85,
      -1,    -1,    -1,    -1,    -1,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,   101,   102,   103,   104,   105,
      -1,    -1,    -1,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,   123,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   137,   138,   139,   140,   141,     9,    10,    11,    12,
      -1,    -1,    15,    -1,    17,    -1,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    65,    66,    67,    68,    69,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    77,    78,    -1,    80,    81,    82,
      83,    84,    85,    -1,    -1,    -1,    -1,    -1,    91,    92,
      93,    94,    95,    96,    97,    98,    99,   100,   101,   102,
     103,   104,   105,    -1,    -1,    -1,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     123,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   137,   138,   139,   140,   141
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     5,     6,     7,   143,   144,   145,   146,     8,     9,
      10,    11,    12,    13,    14,    15,    17,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    65,
      66,    67,    68,    69,    77,    78,    80,    81,    82,    83,
      84,    85,    91,    92,    93,    94,    95,    96,    97,    98,
      99,   100,   101,   102,   103,   104,   105,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   137,   138,   139,   140,   141,   147,   148,   149,
     150,   152,   153,   154,   155,   156,   157,   158,   159,   160,
     161,   162,   163,   164,   165,   166,   167,   168,   169,   170,
     171,   172,   173,   174,   175,   176,   177,   178,   179,   182,
     185,   188,   189,   190,   191,   192,   193,   194,   195,   196,
     197,   200,   201,   202,   203,   204,   205,   206,   207,   208,
     209,   210,   211,   212,   213,   214,   215,   216,   217,   218,
     219,   220,   221,     8,   147,     0,   145,   146,   149,   157,
     159,   149,   147,   149,    34,    35,    36,    35,    36,    37,
      35,    36,    37,    35,    36,    37,    35,    36,    37,    35,
      36,    35,    36,    35,    36,    35,    36,   149,   149,    86,
     149,   149,   149,   149,   149,   149,   149,   149,   149,    15,
     100,    15,   149,   149,   149,    70,    70,    70,    70,   149,
     149,   149,   149,   149,    86,   149,    86,    86,   149,    64,
      64,    70,   149,   149,   149,   149,   149,   149,   149,   149,
     149,   149,   149,   149,   125,   126,   127,   128,   129,   130,
     131,   133,   134,   135,   136,    15,   149,    64,    64,     8,
     148,    13,    14,   147,    13,    14,    13,    14,   139,     8,
       3,     4,    16,    16,   149,   149,   149,   149,   149,   149,
     149,    13,    14,    15,   198,   199,   198,   149,    53,    79,
      88,   147,   234,   235,   236,   237,   240,   178,    72,    73,
      74,   180,   181,    75,   186,   187,    76,   183,   184,   149,
     149,   149,   149,   149,   106,   234,   234,   234,   234,   234,
     234,    87,   132,   234,   234,   234,   234,   234,   147,   147,
     149,   149,     3,     4,    18,   151,   149,   149,   149,   149,
     147,   147,   149,   149,    13,   149,   198,   199,    16,    15,
      15,    15,    15,    16,    52,    51,    71,    71,   181,    71,
     187,    71,   184,    71,   124,   124,   124,   124,   124,   124,
     132,   124,   124,   124,   124,    16,    14,    13,   147,   147,
      34,    35,    37,    14,    13,    14,    13,    16,    16,   149,
      13,    16,   149,   198,    54,    55,    56,    57,    58,    59,
      60,    61,    62,    63,   222,   223,   224,   225,   226,   227,
     228,   229,   230,   231,   232,   233,   225,   226,   238,   239,
      89,    90,   225,   226,   241,   242,   243,   244,   235,   240,
      70,   125,   126,   127,   128,   129,   130,   133,   134,   135,
     136,   149,   149,   151,   151,   149,   149,   149,   149,   149,
      15,   100,    16,    64,    64,    64,    64,    64,    64,    64,
      64,    64,    64,    16,   223,    16,   239,    64,    64,    16,
     242,   107,   198,   234,   236,   147,    71,    16,    16,    70,
     108,    71
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *bottom, yytype_int16 *top)
#else
static void
yy_stack_print (bottom, top)
    yytype_int16 *bottom;
    yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      fprintf (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss = yyssa;
  yytype_int16 *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 284 "itex2MML.y"
    {/* all processing done in body*/}
    break;

  case 3:
#line 287 "itex2MML.y"
    {/* nothing - do nothing*/}
    break;

  case 4:
#line 288 "itex2MML.y"
    {/* proc done in body*/}
    break;

  case 5:
#line 289 "itex2MML.y"
    {/* all proc. in body*/}
    break;

  case 6:
#line 290 "itex2MML.y"
    {/* all proc. in body*/}
    break;

  case 7:
#line 291 "itex2MML.y"
    {/* all proc. in body*/}
    break;

  case 8:
#line 293 "itex2MML.y"
    {printf("%s", (yyvsp[(1) - (1)]));}
    break;

  case 9:
#line 295 "itex2MML.y"
    {/* empty math group - ignore*/}
    break;

  case 10:
#line 296 "itex2MML.y"
    {/* ditto */}
    break;

  case 11:
#line 297 "itex2MML.y"
    {
  char ** r = (char **) ret_str;
  char * s = itex2MML_copy3("<math xmlns='http://www.w3.org/1998/Math/MathML' display='inline'>", (yyvsp[(2) - (3)]), "</math>");
  itex2MML_free_string((yyvsp[(2) - (3)]));
  if (r) {
    (*r) = (s == itex2MML_empty_string) ? 0 : s;
  }
  else {
    if (itex2MML_write_mathml)
      (*itex2MML_write_mathml) (s);
    itex2MML_free_string(s);
  }
}
    break;

  case 12:
#line 310 "itex2MML.y"
    {
  char ** r = (char **) ret_str;
  char * s = itex2MML_copy3("<math xmlns='http://www.w3.org/1998/Math/MathML' display='block'>", (yyvsp[(2) - (3)]), "</math>");
  itex2MML_free_string((yyvsp[(2) - (3)]));
  if (r) {
    (*r) = (s == itex2MML_empty_string) ? 0 : s;
  }
  else {
    if (itex2MML_write_mathml)
      (*itex2MML_write_mathml) (s);
    itex2MML_free_string(s);
  }
}
    break;

  case 13:
#line 324 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string((yyvsp[(1) - (1)]));
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 14:
#line 328 "itex2MML.y"
    {
  (yyval) = itex2MML_copy2((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));
  itex2MML_free_string((yyvsp[(1) - (2)]));
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 15:
#line 334 "itex2MML.y"
    {
  if (itex2MML_displaymode == 1) {
    char * s1 = itex2MML_copy3("<munderover>", (yyvsp[(1) - (5)]), " ");
    char * s2 = itex2MML_copy3((yyvsp[(3) - (5)]), " ", (yyvsp[(5) - (5)]));
    (yyval) = itex2MML_copy3(s1, s2, "</munderover>");
    itex2MML_free_string(s1);
    itex2MML_free_string(s2);
  }
  else {
    char * s1 = itex2MML_copy3("<msubsup>", (yyvsp[(1) - (5)]), " ");
    char * s2 = itex2MML_copy3((yyvsp[(3) - (5)]), " ", (yyvsp[(5) - (5)]));
    (yyval) = itex2MML_copy3(s1, s2, "</msubsup>");
    itex2MML_free_string(s1);
    itex2MML_free_string(s2);
  }
  itex2MML_free_string((yyvsp[(1) - (5)]));
  itex2MML_free_string((yyvsp[(3) - (5)]));
  itex2MML_free_string((yyvsp[(5) - (5)]));
}
    break;

  case 16:
#line 353 "itex2MML.y"
    {
  if (itex2MML_displaymode == 1) {
    char * s1 = itex2MML_copy3("<munder>", (yyvsp[(1) - (3)]), " ");
    (yyval) = itex2MML_copy3(s1, (yyvsp[(3) - (3)]), "</munder>");
    itex2MML_free_string(s1);
  }
  else {
    char * s1 = itex2MML_copy3("<msub>", (yyvsp[(1) - (3)]), " ");
    (yyval) = itex2MML_copy3(s1, (yyvsp[(3) - (3)]), "</msub>");
    itex2MML_free_string(s1);
  }
  itex2MML_free_string((yyvsp[(1) - (3)]));
  itex2MML_free_string((yyvsp[(3) - (3)]));
}
    break;

  case 17:
#line 367 "itex2MML.y"
    {
  if (itex2MML_displaymode == 1) {
    char * s1 = itex2MML_copy3("<munderover>", (yyvsp[(1) - (5)]), " ");
    char * s2 = itex2MML_copy3((yyvsp[(5) - (5)]), " ", (yyvsp[(3) - (5)]));
    (yyval) = itex2MML_copy3(s1, s2, "</munderover>");
    itex2MML_free_string(s1);
    itex2MML_free_string(s2);
  }
  else {
    char * s1 = itex2MML_copy3("<msubsup>", (yyvsp[(1) - (5)]), " ");
    char * s2 = itex2MML_copy3((yyvsp[(5) - (5)]), " ", (yyvsp[(3) - (5)]));
    (yyval) = itex2MML_copy3(s1, s2, "</msubsup>");
    itex2MML_free_string(s1);
    itex2MML_free_string(s2);
  }
  itex2MML_free_string((yyvsp[(1) - (5)]));
  itex2MML_free_string((yyvsp[(3) - (5)]));
  itex2MML_free_string((yyvsp[(5) - (5)]));
}
    break;

  case 18:
#line 386 "itex2MML.y"
    {
  if (itex2MML_displaymode == 1) {
    char * s1 = itex2MML_copy3("<mover>", (yyvsp[(1) - (3)]), " ");
    (yyval) = itex2MML_copy3(s1, (yyvsp[(3) - (3)]), "</mover>");
    itex2MML_free_string(s1);
  }
  else {
    char * s1 = itex2MML_copy3("<msup>", (yyvsp[(1) - (3)]), " ");
    (yyval) = itex2MML_copy3(s1, (yyvsp[(3) - (3)]), "</msup>");
    itex2MML_free_string(s1);
  }
  itex2MML_free_string((yyvsp[(1) - (3)]));
  itex2MML_free_string((yyvsp[(3) - (3)]));
}
    break;

  case 19:
#line 400 "itex2MML.y"
    {
  if (itex2MML_displaymode == 1) {
    char * s1 = itex2MML_copy3("<munderover>", (yyvsp[(1) - (5)]), " ");
    char * s2 = itex2MML_copy3((yyvsp[(3) - (5)]), " ", (yyvsp[(5) - (5)]));
    (yyval) = itex2MML_copy3(s1, s2, "</munderover>");
    itex2MML_free_string(s1);
    itex2MML_free_string(s2);
  }
  else {
    char * s1 = itex2MML_copy3("<msubsup>", (yyvsp[(1) - (5)]), " ");
    char * s2 = itex2MML_copy3((yyvsp[(3) - (5)]), " ", (yyvsp[(5) - (5)]));
    (yyval) = itex2MML_copy3(s1, s2, "</msubsup>");
    itex2MML_free_string(s1);
    itex2MML_free_string(s2);
  }
  itex2MML_free_string((yyvsp[(1) - (5)]));
  itex2MML_free_string((yyvsp[(3) - (5)]));
  itex2MML_free_string((yyvsp[(5) - (5)]));
}
    break;

  case 20:
#line 419 "itex2MML.y"
    {
  if (itex2MML_displaymode == 1) {
    char * s1 = itex2MML_copy3("<munder>", (yyvsp[(1) - (3)]), " ");
    (yyval) = itex2MML_copy3(s1, (yyvsp[(3) - (3)]), "</munder>");
    itex2MML_free_string(s1);
  }
  else {
    char * s1 = itex2MML_copy3("<msub>", (yyvsp[(1) - (3)]), " ");
    (yyval) = itex2MML_copy3(s1, (yyvsp[(3) - (3)]), "</msub>");
    itex2MML_free_string(s1);
  }
  itex2MML_free_string((yyvsp[(1) - (3)]));
  itex2MML_free_string((yyvsp[(3) - (3)]));
}
    break;

  case 21:
#line 433 "itex2MML.y"
    {
  if (itex2MML_displaymode == 1) {
    char * s1 = itex2MML_copy3("<munderover>", (yyvsp[(1) - (5)]), " ");
    char * s2 = itex2MML_copy3((yyvsp[(5) - (5)]), " ", (yyvsp[(3) - (5)]));
    (yyval) = itex2MML_copy3(s1, s2, "</munderover>");
    itex2MML_free_string(s1);
    itex2MML_free_string(s2);
  }
  else {
    char * s1 = itex2MML_copy3("<msubsup>", (yyvsp[(1) - (5)]), " ");
    char * s2 = itex2MML_copy3((yyvsp[(5) - (5)]), " ", (yyvsp[(3) - (5)]));
    (yyval) = itex2MML_copy3(s1, s2, "</msubsup>");
    itex2MML_free_string(s1);
    itex2MML_free_string(s2);
  }
  itex2MML_free_string((yyvsp[(1) - (5)]));
  itex2MML_free_string((yyvsp[(3) - (5)]));
  itex2MML_free_string((yyvsp[(5) - (5)]));
}
    break;

  case 22:
#line 452 "itex2MML.y"
    {
  if (itex2MML_displaymode == 1) {
    char * s1 = itex2MML_copy3("<mover>", (yyvsp[(1) - (3)]), " ");
    (yyval) = itex2MML_copy3(s1, (yyvsp[(3) - (3)]), "</mover>");
    itex2MML_free_string(s1);
  }
  else {
    char * s1 = itex2MML_copy3("<msup>", (yyvsp[(1) - (3)]), " ");
    (yyval) = itex2MML_copy3(s1, (yyvsp[(3) - (3)]), "</msup>");
    itex2MML_free_string(s1);
  }
  itex2MML_free_string((yyvsp[(1) - (3)]));
  itex2MML_free_string((yyvsp[(3) - (3)]));
}
    break;

  case 23:
#line 466 "itex2MML.y"
    {
  char * s1 = itex2MML_copy3("<msubsup>", (yyvsp[(1) - (5)]), " ");
  char * s2 = itex2MML_copy3((yyvsp[(3) - (5)]), " ", (yyvsp[(5) - (5)]));
  (yyval) = itex2MML_copy3(s1, s2, "</msubsup>");
  itex2MML_free_string(s1);
  itex2MML_free_string(s2);
  itex2MML_free_string((yyvsp[(1) - (5)]));
  itex2MML_free_string((yyvsp[(3) - (5)]));
  itex2MML_free_string((yyvsp[(5) - (5)]));
}
    break;

  case 24:
#line 476 "itex2MML.y"
    {
  char * s1 = itex2MML_copy3("<msubsup>", (yyvsp[(1) - (5)]), " ");
  char * s2 = itex2MML_copy3((yyvsp[(5) - (5)]), " ", (yyvsp[(3) - (5)]));
  (yyval) = itex2MML_copy3(s1, s2, "</msubsup>");
  itex2MML_free_string(s1);
  itex2MML_free_string(s2);
  itex2MML_free_string((yyvsp[(1) - (5)]));
  itex2MML_free_string((yyvsp[(3) - (5)]));
  itex2MML_free_string((yyvsp[(5) - (5)]));
}
    break;

  case 25:
#line 486 "itex2MML.y"
    {
  char * s1 = itex2MML_copy3("<msub>", (yyvsp[(1) - (3)]), " ");
  (yyval) = itex2MML_copy3(s1, (yyvsp[(3) - (3)]), "</msub>");
  itex2MML_free_string(s1);
  itex2MML_free_string((yyvsp[(1) - (3)]));
  itex2MML_free_string((yyvsp[(3) - (3)]));
}
    break;

  case 26:
#line 493 "itex2MML.y"
    {
  char * s1 = itex2MML_copy3("<msup>", (yyvsp[(1) - (3)]), " ");
  (yyval) = itex2MML_copy3(s1, (yyvsp[(3) - (3)]), "</msup>");
  itex2MML_free_string(s1);
  itex2MML_free_string((yyvsp[(1) - (3)]));
  itex2MML_free_string((yyvsp[(3) - (3)]));
}
    break;

  case 27:
#line 500 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<msub><mo></mo>", (yyvsp[(2) - (2)]), "</msub>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 28:
#line 504 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<msup><mo></mo>", (yyvsp[(2) - (2)]), "</msup>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 29:
#line 508 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string((yyvsp[(1) - (1)]));
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 34:
#line 517 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mi>", (yyvsp[(1) - (1)]), "</mi>");
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 35:
#line 521 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mn>", (yyvsp[(1) - (1)]), "</mn>");
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 86:
#line 575 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string((yyvsp[(2) - (3)]));
  itex2MML_free_string((yyvsp[(2) - (3)]));
}
    break;

  case 87:
#line 579 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mrow>", (yyvsp[(2) - (3)]), "</mrow>");
  itex2MML_free_string((yyvsp[(2) - (3)]));
}
    break;

  case 88:
#line 583 "itex2MML.y"
    {
  char * s1 = itex2MML_copy3("<mrow>", (yyvsp[(1) - (3)]), (yyvsp[(2) - (3)]));
  (yyval) = itex2MML_copy3(s1, (yyvsp[(3) - (3)]), "</mrow>");
  itex2MML_free_string(s1);
  itex2MML_free_string((yyvsp[(1) - (3)]));
  itex2MML_free_string((yyvsp[(2) - (3)]));
  itex2MML_free_string((yyvsp[(3) - (3)]));
}
    break;

  case 93:
#line 596 "itex2MML.y"
    {
  itex2MML_rowposn = 2;
  (yyval) = itex2MML_copy3("<mo>", (yyvsp[(2) - (2)]), "</mo>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 94:
#line 601 "itex2MML.y"
    {
  itex2MML_rowposn = 2;
  (yyval) = itex2MML_copy3("<mo>", (yyvsp[(2) - (2)]), "</mo>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 95:
#line 606 "itex2MML.y"
    {
  itex2MML_rowposn = 2;
  (yyval) = itex2MML_copy_string("");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 96:
#line 612 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mo>", (yyvsp[(2) - (2)]), "</mo>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 97:
#line 616 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mo>", (yyvsp[(2) - (2)]), "</mo>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 98:
#line 620 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string("");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 99:
#line 625 "itex2MML.y"
    {
  itex2MML_rowposn = 2;
  (yyval) = itex2MML_copy3("<mo maxsize=\"1.2em\" minsize=\"1.2em\">", (yyvsp[(2) - (2)]), "</mo>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 100:
#line 630 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mo maxsize=\"1.2em\" minsize=\"1.2em\">", (yyvsp[(2) - (2)]), "</mo>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 101:
#line 634 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mo maxsize=\"1.2em\" minsize=\"1.2em\">", (yyvsp[(2) - (2)]), "</mo>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 102:
#line 638 "itex2MML.y"
    {
  itex2MML_rowposn = 2;
  (yyval) = itex2MML_copy3("<mo maxsize=\"1.8em\" minsize=\"1.8em\">", (yyvsp[(2) - (2)]), "</mo>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 103:
#line 643 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mo maxsize=\"1.8em\" minsize=\"1.8em\">", (yyvsp[(2) - (2)]), "</mo>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 104:
#line 647 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mo maxsize=\"1.8em\" minsize=\"1.8em\">", (yyvsp[(2) - (2)]), "</mo>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 105:
#line 651 "itex2MML.y"
    {
  itex2MML_rowposn = 2;
  (yyval) = itex2MML_copy3("<mo maxsize=\"2.4em\" minsize=\"2.4em\">", (yyvsp[(2) - (2)]), "</mo>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 106:
#line 656 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mo maxsize=\"2.4em\" minsize=\"2.4em\">", (yyvsp[(2) - (2)]), "</mo>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 107:
#line 660 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mo maxsize=\"2.4em\" minsize=\"2.4em\">", (yyvsp[(2) - (2)]), "</mo>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 108:
#line 664 "itex2MML.y"
    {
  itex2MML_rowposn = 2;
  (yyval) = itex2MML_copy3("<mo maxsize=\"3em\" minsize=\"3em\">", (yyvsp[(2) - (2)]), "</mo>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 109:
#line 669 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mo maxsize=\"3em\" minsize=\"3em\">", (yyvsp[(2) - (2)]), "</mo>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 110:
#line 673 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mo maxsize=\"3em\" minsize=\"3em\">", (yyvsp[(2) - (2)]), "</mo>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 111:
#line 677 "itex2MML.y"
    {
  itex2MML_rowposn = 2;
  (yyval) = itex2MML_copy3("<mo maxsize=\"1.2em\" minsize=\"1.2em\">", (yyvsp[(2) - (2)]), "</mo>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 112:
#line 682 "itex2MML.y"
    {
  itex2MML_rowposn = 2;
  (yyval) = itex2MML_copy3("<mo maxsize=\"1.2em\" minsize=\"1.2em\">", (yyvsp[(2) - (2)]), "</mo>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 113:
#line 687 "itex2MML.y"
    {
  itex2MML_rowposn = 2;
  (yyval) = itex2MML_copy3("<mo maxsize=\"1.8em\" minsize=\"1.8em\">", (yyvsp[(2) - (2)]), "</mo>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 114:
#line 692 "itex2MML.y"
    {
  itex2MML_rowposn = 2;
  (yyval) = itex2MML_copy3("<mo maxsize=\"1.8em\" minsize=\"1.8em\">", (yyvsp[(2) - (2)]), "</mo>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 115:
#line 697 "itex2MML.y"
    {
  itex2MML_rowposn = 2;
  (yyval) = itex2MML_copy3("<mo maxsize=\"2.4em\" minsize=\"2.4em\">", (yyvsp[(2) - (2)]), "</mo>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 116:
#line 702 "itex2MML.y"
    {
  itex2MML_rowposn = 2;
  (yyval) = itex2MML_copy3("<mo maxsize=\"2.4em\" minsize=\"2.4em\">", (yyvsp[(2) - (2)]), "</mo>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 117:
#line 707 "itex2MML.y"
    {
  itex2MML_rowposn = 2;
  (yyval) = itex2MML_copy3("<mo maxsize=\"3em\" minsize=\"3em\">", (yyvsp[(2) - (2)]), "</mo>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 118:
#line 712 "itex2MML.y"
    {
  itex2MML_rowposn = 2;
  (yyval) = itex2MML_copy3("<mo maxsize=\"3em\" minsize=\"3em\">", (yyvsp[(2) - (2)]), "</mo>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 119:
#line 718 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string("<merror><mtext>Unknown character</mtext></merror>");
}
    break;

  case 120:
#line 722 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string("<mo lspace=\"verythinmathspace\" rspace=\"0em\">&minus;</mo>");
}
    break;

  case 121:
#line 726 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string("<mo lspace=\"verythinmathspace\" rspace=\"0em\">+</mo>");
}
    break;

  case 123:
#line 732 "itex2MML.y"
    {
  itex2MML_rowposn=2;
  (yyval) = itex2MML_copy3("<mi>", (yyvsp[(1) - (1)]), "</mi>");
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 125:
#line 740 "itex2MML.y"
    {
  itex2MML_rowposn = 2;
  (yyval) = itex2MML_copy3("<mo lspace=\"thinmathspace\" rspace=\"thinmathspace\">", (yyvsp[(1) - (1)]), "</mo>");
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 128:
#line 748 "itex2MML.y"
    {
  itex2MML_rowposn = 2;
  (yyval) = itex2MML_copy3("<mo>", (yyvsp[(1) - (1)]), "</mo>");
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 129:
#line 753 "itex2MML.y"
    {
  itex2MML_rowposn = 2;
  (yyval) = itex2MML_copy3("<mo>", (yyvsp[(1) - (1)]), "</mo>");
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 130:
#line 758 "itex2MML.y"
    {
  itex2MML_rowposn = 2;
  (yyval) = itex2MML_copy3("<mstyle scriptlevel=\"0\"><mo>", (yyvsp[(1) - (1)]), "</mo></mstyle>");
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 131:
#line 763 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mo stretchy=\"false\">", (yyvsp[(1) - (1)]), "</mo>");
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 132:
#line 767 "itex2MML.y"
    {
  itex2MML_rowposn = 2;
  (yyval) = itex2MML_copy3("<mo stretchy=\"false\">", (yyvsp[(1) - (1)]), "</mo>");
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 133:
#line 772 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mo stretchy=\"false\">", (yyvsp[(1) - (1)]), "</mo>");
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 134:
#line 776 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mo stretchy=\"false\">", (yyvsp[(1) - (1)]), "</mo>");
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 135:
#line 780 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mo>", (yyvsp[(1) - (1)]), "</mo>");
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 136:
#line 784 "itex2MML.y"
    {
  itex2MML_rowposn=2;
  (yyval) = itex2MML_copy3("<mo lspace=\"mediummathspace\" rspace=\"mediummathspace\">", (yyvsp[(1) - (1)]), "</mo>");
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 137:
#line 789 "itex2MML.y"
    {
  itex2MML_rowposn = 2;
  (yyval) = itex2MML_copy3("<mo lspace=\"0em\" rspace=\"thinmathspace\">", (yyvsp[(1) - (1)]), "</mo>");
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 138:
#line 794 "itex2MML.y"
    {
  itex2MML_rowposn = 2;
  (yyval) = itex2MML_copy3("<mo lspace=\"0em\" rspace=\"thinmathspace\">", (yyvsp[(2) - (2)]), "</mo>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 139:
#line 800 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string("<mrow></mrow>");
}
    break;

  case 140:
#line 804 "itex2MML.y"
    {
  char * s1 = itex2MML_copy3("<mspace height=\"", (yyvsp[(3) - (10)]), "ex\" depth=\"");
  char * s2 = itex2MML_copy3((yyvsp[(6) - (10)]), "ex\" width=\"", (yyvsp[(9) - (10)]));
  (yyval) = itex2MML_copy3(s1, s2, "em\"></mspace>");
  itex2MML_free_string(s1);
  itex2MML_free_string(s2);
  itex2MML_free_string((yyvsp[(3) - (10)]));
  itex2MML_free_string((yyvsp[(6) - (10)]));
  itex2MML_free_string((yyvsp[(9) - (10)]));
}
    break;

  case 141:
#line 815 "itex2MML.y"
    {
  char * s1 = itex2MML_copy3("<maction actiontype=\"statusline\">", (yyvsp[(3) - (3)]), "<mtext>");
  (yyval) = itex2MML_copy3(s1, (yyvsp[(2) - (3)]), "</mtext></maction>");
  itex2MML_free_string(s1);
  itex2MML_free_string((yyvsp[(2) - (3)]));
  itex2MML_free_string((yyvsp[(3) - (3)]));
}
    break;

  case 142:
#line 823 "itex2MML.y"
    {
  char * s1 = itex2MML_copy3("<maction actiontype=\"toggle\" selection=\"2\">", (yyvsp[(2) - (3)]), " ");
  (yyval) = itex2MML_copy3(s1, (yyvsp[(3) - (3)]), "</maction>");
  itex2MML_free_string(s1);
  itex2MML_free_string((yyvsp[(2) - (3)]));
  itex2MML_free_string((yyvsp[(3) - (3)]));
}
    break;

  case 143:
#line 831 "itex2MML.y"
    {
  char * s1 = itex2MML_copy3("<maction actiontype=\"highlight\" other='color=", (yyvsp[(2) - (3)]), "'>");
  (yyval) = itex2MML_copy3(s1, (yyvsp[(3) - (3)]), "</maction>");
  itex2MML_free_string(s1);
  itex2MML_free_string((yyvsp[(2) - (3)]));
  itex2MML_free_string((yyvsp[(3) - (3)]));
}
    break;

  case 144:
#line 839 "itex2MML.y"
    {
  char * s1 = itex2MML_copy3("<maction actiontype=\"highlight\" other='background=", (yyvsp[(2) - (3)]), "'>");
  (yyval) = itex2MML_copy3(s1, (yyvsp[(3) - (3)]), "</maction>");
  itex2MML_free_string(s1);
  itex2MML_free_string((yyvsp[(2) - (3)]));
  itex2MML_free_string((yyvsp[(3) - (3)]));
}
    break;

  case 145:
#line 847 "itex2MML.y"
    {
  char * s1 = itex2MML_copy3("<mstyle mathcolor=", (yyvsp[(2) - (3)]), ">");
  (yyval) = itex2MML_copy3(s1, (yyvsp[(3) - (3)]), "</mstyle>");
  itex2MML_free_string(s1);
  itex2MML_free_string((yyvsp[(2) - (3)]));
  itex2MML_free_string((yyvsp[(3) - (3)]));
}
    break;

  case 146:
#line 854 "itex2MML.y"
    {
  char * s1 = itex2MML_copy3("<mstyle mathbackground=", (yyvsp[(2) - (3)]), ">");
  (yyval) = itex2MML_copy3(s1, (yyvsp[(3) - (3)]), "</mstyle>");
  itex2MML_free_string(s1);
  itex2MML_free_string((yyvsp[(2) - (3)]));
  itex2MML_free_string((yyvsp[(3) - (3)]));
}
    break;

  case 147:
#line 862 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mtext>", (yyvsp[(2) - (2)]), "</mtext>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 148:
#line 867 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mstyle displaystyle=\"true\">", (yyvsp[(2) - (2)]), "</mstyle>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 149:
#line 872 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mstyle displaystyle=\"false\">", (yyvsp[(2) - (2)]), "</mstyle>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 150:
#line 877 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mstyle scriptlevel=\"0\">", (yyvsp[(2) - (2)]), "</mstyle>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 151:
#line 882 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mstyle scriptlevel=\"1\">", (yyvsp[(2) - (2)]), "</mstyle>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 152:
#line 887 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mstyle scriptlevel=\"2\">", (yyvsp[(2) - (2)]), "</mstyle>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 153:
#line 892 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mstyle mathvariant=\"italic\">", (yyvsp[(2) - (2)]), "</mstyle>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 154:
#line 897 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mrow><mpadded width=\"0.125em\"><mo>&#xff0f;</mo></mpadded>", (yyvsp[(2) - (2)]), "</mrow>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 155:
#line 902 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mstyle mathvariant=\"bold\">", (yyvsp[(2) - (2)]), "</mstyle>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 156:
#line 907 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mi mathvariant=\"normal\">", (yyvsp[(3) - (4)]), "</mi>");
  itex2MML_free_string((yyvsp[(3) - (4)]));
}
    break;

  case 157:
#line 912 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string((yyvsp[(1) - (1)]));
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 158:
#line 916 "itex2MML.y"
    {
  (yyval) = itex2MML_copy2((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));
  itex2MML_free_string((yyvsp[(1) - (2)]));
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 159:
#line 922 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mi>", (yyvsp[(3) - (4)]), "</mi>");
  itex2MML_free_string((yyvsp[(3) - (4)]));
}
    break;

  case 160:
#line 927 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string((yyvsp[(1) - (1)]));
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 161:
#line 931 "itex2MML.y"
    {
  (yyval) = itex2MML_copy2((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));
  itex2MML_free_string((yyvsp[(1) - (2)]));
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 162:
#line 937 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("&", (yyvsp[(1) - (1)]), "opf;");
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 163:
#line 941 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("&", (yyvsp[(1) - (1)]), "opf;");
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 164:
#line 945 "itex2MML.y"
    {
  /* Blackboard digits 0-9 correspond to Unicode characters 0x1D7D8-0x1D7E1 */
  char * end = (yyvsp[(1) - (1)]) + 1;
  int code = 0x1D7D8 + strtoul((yyvsp[(1) - (1)]), &end, 10);
  (yyval) = itex2MML_character_reference(code);
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 165:
#line 953 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mi>", (yyvsp[(3) - (4)]), "</mi>");
  itex2MML_free_string((yyvsp[(3) - (4)]));
}
    break;

  case 166:
#line 958 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string((yyvsp[(1) - (1)]));
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 167:
#line 962 "itex2MML.y"
    {
  (yyval) = itex2MML_copy2((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));
  itex2MML_free_string((yyvsp[(1) - (2)]));
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 168:
#line 968 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("&", (yyvsp[(1) - (1)]), "fr;");
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 169:
#line 973 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mi>", (yyvsp[(3) - (4)]), "</mi>");
  itex2MML_free_string((yyvsp[(3) - (4)]));
}
    break;

  case 170:
#line 978 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string((yyvsp[(1) - (1)]));
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 171:
#line 982 "itex2MML.y"
    {
  (yyval) = itex2MML_copy2((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]));
  itex2MML_free_string((yyvsp[(1) - (2)]));
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 172:
#line 988 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("&", (yyvsp[(1) - (1)]), "scr;");
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 173:
#line 993 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string("<mspace width=\"thinmathspace\"></mspace>");
}
    break;

  case 174:
#line 997 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string("<mspace width=\"mediummathspace\"></mspace>");
}
    break;

  case 175:
#line 1001 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string("<mspace width=\"thickmathspace\"></mspace>");
}
    break;

  case 176:
#line 1005 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string("<mspace width=\"1em\"></mspace>");
}
    break;

  case 177:
#line 1009 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string("<mspace width=\"2em\"></mspace>");
}
    break;

  case 178:
#line 1013 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string("<mspace width=\"-0.1667 em\"></mspace>");
}
    break;

  case 179:
#line 1017 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mphantom>", (yyvsp[(2) - (2)]), "</mphantom>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 180:
#line 1022 "itex2MML.y"
    {
  char * s1 = itex2MML_copy3("<mrow xmlns:xlink=\"http://www.w3.org/1999/xlink\" xlink:type=\"simple\" xlink:href=\"", (yyvsp[(2) - (3)]), "\">");
  (yyval) = itex2MML_copy3(s1, (yyvsp[(3) - (3)]), "</mrow>");
  itex2MML_free_string(s1);
  itex2MML_free_string((yyvsp[(2) - (3)]));
  itex2MML_free_string((yyvsp[(3) - (3)]));
}
    break;

  case 181:
#line 1030 "itex2MML.y"
    {
  char * s1 = itex2MML_copy3("<mmultiscripts>", (yyvsp[(2) - (5)]), (yyvsp[(4) - (5)]));
  (yyval) = itex2MML_copy2(s1, "</mmultiscripts>");
  itex2MML_free_string(s1);
  itex2MML_free_string((yyvsp[(2) - (5)]));
  itex2MML_free_string((yyvsp[(4) - (5)]));
}
    break;

  case 182:
#line 1037 "itex2MML.y"
    {
  char * s1 = itex2MML_copy3("<mmultiscripts>", (yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]));
  (yyval) = itex2MML_copy2(s1, "</mmultiscripts>");
  itex2MML_free_string(s1);
  itex2MML_free_string((yyvsp[(2) - (3)]));
  itex2MML_free_string((yyvsp[(3) - (3)]));
}
    break;

  case 183:
#line 1045 "itex2MML.y"
    {
  char * s1 = itex2MML_copy3("<mmultiscripts>", (yyvsp[(5) - (8)]), (yyvsp[(7) - (8)]));
  char * s2 = itex2MML_copy3("<mprescripts></mprescripts>", (yyvsp[(3) - (8)]), "</mmultiscripts>");
  (yyval) = itex2MML_copy2(s1, s2);
  itex2MML_free_string(s1);
  itex2MML_free_string(s2);
  itex2MML_free_string((yyvsp[(3) - (8)]));
  itex2MML_free_string((yyvsp[(5) - (8)]));
  itex2MML_free_string((yyvsp[(7) - (8)]));
}
    break;

  case 184:
#line 1055 "itex2MML.y"
    {
  char * s1 = itex2MML_copy2("<mmultiscripts>", (yyvsp[(5) - (6)]));
  char * s2 = itex2MML_copy3("<mprescripts></mprescripts>", (yyvsp[(3) - (6)]), "</mmultiscripts>");
  (yyval) = itex2MML_copy2(s1, s2);
  itex2MML_free_string(s1);
  itex2MML_free_string(s2);
  itex2MML_free_string((yyvsp[(3) - (6)]));
  itex2MML_free_string((yyvsp[(5) - (6)]));
}
    break;

  case 185:
#line 1064 "itex2MML.y"
    {
  char * s1 = itex2MML_copy3("<mmultiscripts>", (yyvsp[(3) - (6)]), (yyvsp[(5) - (6)]));
  (yyval) = itex2MML_copy2(s1, "</mmultiscripts>");
  itex2MML_free_string(s1);
  itex2MML_free_string((yyvsp[(3) - (6)]));
  itex2MML_free_string((yyvsp[(5) - (6)])); 
}
    break;

  case 186:
#line 1072 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string((yyvsp[(1) - (1)]));
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 187:
#line 1076 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3((yyvsp[(1) - (2)]), " ", (yyvsp[(2) - (2)]));
  itex2MML_free_string((yyvsp[(1) - (2)]));
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 188:
#line 1082 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3((yyvsp[(2) - (4)]), " ", (yyvsp[(4) - (4)]));
  itex2MML_free_string((yyvsp[(2) - (4)]));
  itex2MML_free_string((yyvsp[(4) - (4)]));
}
    break;

  case 189:
#line 1087 "itex2MML.y"
    {
  (yyval) = itex2MML_copy2((yyvsp[(2) - (2)]), " <none></none>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 190:
#line 1091 "itex2MML.y"
    {
  (yyval) = itex2MML_copy2("<none></none> ", (yyvsp[(2) - (2)]));
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 191:
#line 1095 "itex2MML.y"
    {
  (yyval) = itex2MML_copy2("<none></none> ", (yyvsp[(3) - (3)]));
  itex2MML_free_string((yyvsp[(3) - (3)]));
}
    break;

  case 192:
#line 1100 "itex2MML.y"
    {
  char * s1 = itex2MML_copy3("<mfrac>", (yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]));
  (yyval) = itex2MML_copy2(s1, "</mfrac>");
  itex2MML_free_string(s1);
  itex2MML_free_string((yyvsp[(2) - (3)]));
  itex2MML_free_string((yyvsp[(3) - (3)]));
}
    break;

  case 193:
#line 1107 "itex2MML.y"
    {
  char * s1 = itex2MML_copy3("<mstyle displaystyle=\"false\"><mfrac>", (yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]));
  (yyval) = itex2MML_copy2(s1, "</mfrac></mstyle>");
  itex2MML_free_string(s1);
  itex2MML_free_string((yyvsp[(2) - (3)]));
  itex2MML_free_string((yyvsp[(3) - (3)]));
}
    break;

  case 194:
#line 1115 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3( "<mo lspace=\"mediummathspace\">(</mo><mo rspace=\"thinmathspace\">mod</mo>", (yyvsp[(2) - (2)]), "<mo rspace=\"mediummathspace\">)</mo>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 195:
#line 1120 "itex2MML.y"
    {
  char * s1 = itex2MML_copy3("<mfrac><mrow>", (yyvsp[(2) - (5)]), "</mrow><mrow>");
  (yyval) = itex2MML_copy3(s1, (yyvsp[(4) - (5)]), "</mrow></mfrac>");
  itex2MML_free_string(s1);
  itex2MML_free_string((yyvsp[(2) - (5)]));
  itex2MML_free_string((yyvsp[(4) - (5)]));
}
    break;

  case 196:
#line 1127 "itex2MML.y"
    {
  char * s1 = itex2MML_copy3("<mrow>", (yyvsp[(1) - (5)]), "<mfrac><mrow>");
  char * s2 = itex2MML_copy3((yyvsp[(2) - (5)]), "</mrow><mrow>", (yyvsp[(4) - (5)]));
  char * s3 = itex2MML_copy3("</mrow></mfrac>", (yyvsp[(5) - (5)]), "</mrow>");
  (yyval) = itex2MML_copy3(s1, s2, s3);
  itex2MML_free_string(s1);
  itex2MML_free_string(s2);
  itex2MML_free_string(s3);
  itex2MML_free_string((yyvsp[(1) - (5)]));
  itex2MML_free_string((yyvsp[(2) - (5)]));
  itex2MML_free_string((yyvsp[(4) - (5)]));
  itex2MML_free_string((yyvsp[(5) - (5)]));
}
    break;

  case 197:
#line 1141 "itex2MML.y"
    {
  char * s1 = itex2MML_copy3("<mfrac linethickness=\"0\"><mrow>", (yyvsp[(2) - (5)]), "</mrow><mrow>");
  (yyval) = itex2MML_copy3(s1, (yyvsp[(4) - (5)]), "</mrow></mfrac>");
  itex2MML_free_string(s1);
  itex2MML_free_string((yyvsp[(2) - (5)]));
  itex2MML_free_string((yyvsp[(4) - (5)]));
}
    break;

  case 198:
#line 1148 "itex2MML.y"
    {
  char * s1 = itex2MML_copy3("<mrow>", (yyvsp[(1) - (5)]), "<mfrac linethickness=\"0\"><mrow>");
  char * s2 = itex2MML_copy3((yyvsp[(2) - (5)]), "</mrow><mrow>", (yyvsp[(4) - (5)]));
  char * s3 = itex2MML_copy3("</mrow></mfrac>", (yyvsp[(5) - (5)]), "</mrow>");
  (yyval) = itex2MML_copy3(s1, s2, s3);
  itex2MML_free_string(s1);
  itex2MML_free_string(s2);
  itex2MML_free_string(s3);
  itex2MML_free_string((yyvsp[(1) - (5)]));
  itex2MML_free_string((yyvsp[(2) - (5)]));
  itex2MML_free_string((yyvsp[(4) - (5)]));
  itex2MML_free_string((yyvsp[(5) - (5)]));
}
    break;

  case 199:
#line 1162 "itex2MML.y"
    {
  char * s1 = itex2MML_copy3("<mrow><mo>(</mo><mfrac linethickness=\"0\">", (yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]));
  (yyval) = itex2MML_copy2(s1, "</mfrac><mo>)</mo></mrow>");
  itex2MML_free_string(s1);
  itex2MML_free_string((yyvsp[(2) - (3)]));
  itex2MML_free_string((yyvsp[(3) - (3)]));
}
    break;

  case 200:
#line 1170 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<munder>", (yyvsp[(2) - (2)]), "<mo>&UnderBrace;</mo></munder>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 201:
#line 1175 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mover>", (yyvsp[(2) - (2)]), "<mo>&OverBrace;</mo></mover>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 202:
#line 1180 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mover>", (yyvsp[(2) - (2)]), "<mo stretchy=\"false\">&OverBar;</mo></mover>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 203:
#line 1184 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mover>", (yyvsp[(2) - (2)]), "<mo>&OverBar;</mo></mover>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 204:
#line 1189 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mover>", (yyvsp[(2) - (2)]), "<mo stretchy=\"false\">&RightVector;</mo></mover>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 205:
#line 1193 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mover>", (yyvsp[(2) - (2)]), "<mo>&RightVector;</mo></mover>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 206:
#line 1198 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mover>", (yyvsp[(2) - (2)]), "<mo>&dot;</mo></mover>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 207:
#line 1203 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mover>", (yyvsp[(2) - (2)]), "<mo>&Dot;</mo></mover>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 208:
#line 1208 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mover>", (yyvsp[(2) - (2)]), "<mo stretchy=\"false\">&tilde;</mo></mover>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 209:
#line 1212 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mover>", (yyvsp[(2) - (2)]), "<mo>&tilde;</mo></mover>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 210:
#line 1217 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mover>", (yyvsp[(2) - (2)]), "<mo stretchy=\"false\">&#x2c7;</mo></mover>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 211:
#line 1221 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mover>", (yyvsp[(2) - (2)]), "<mo>&#x2c7;</mo></mover>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 212:
#line 1226 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mover>", (yyvsp[(2) - (2)]), "<mo stretchy=\"false\">&#x302;</mo></mover>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 213:
#line 1230 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mover>", (yyvsp[(2) - (2)]), "<mo>&#x302;</mo></mover>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 214:
#line 1235 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<msqrt>", (yyvsp[(2) - (2)]), "</msqrt>");
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 215:
#line 1240 "itex2MML.y"
    {
  char * s1 = itex2MML_copy3("<mroot>", (yyvsp[(3) - (3)]), (yyvsp[(2) - (3)]));
  (yyval) = itex2MML_copy2(s1, "</mroot>");
  itex2MML_free_string(s1);
  itex2MML_free_string((yyvsp[(2) - (3)]));
  itex2MML_free_string((yyvsp[(3) - (3)]));
}
    break;

  case 216:
#line 1248 "itex2MML.y"
    {
  char * s1 = itex2MML_copy3("<munder>", (yyvsp[(3) - (3)]), (yyvsp[(2) - (3)]));
  (yyval) = itex2MML_copy2(s1, "</munder>");
  itex2MML_free_string(s1);
  itex2MML_free_string((yyvsp[(2) - (3)]));
  itex2MML_free_string((yyvsp[(3) - (3)]));
}
    break;

  case 217:
#line 1256 "itex2MML.y"
    {
  char * s1 = itex2MML_copy3("<mover>", (yyvsp[(3) - (3)]), (yyvsp[(2) - (3)]));
  (yyval) = itex2MML_copy2(s1, "</mover>");
  itex2MML_free_string(s1);
  itex2MML_free_string((yyvsp[(2) - (3)]));
  itex2MML_free_string((yyvsp[(3) - (3)]));
}
    break;

  case 218:
#line 1264 "itex2MML.y"
    {
  char * s1 = itex2MML_copy3("<munderover>", (yyvsp[(4) - (4)]), (yyvsp[(2) - (4)]));
  (yyval) = itex2MML_copy3(s1, (yyvsp[(3) - (4)]), "</munderover>");
  itex2MML_free_string(s1);
  itex2MML_free_string((yyvsp[(2) - (4)]));
  itex2MML_free_string((yyvsp[(3) - (4)]));
  itex2MML_free_string((yyvsp[(4) - (4)]));
}
    break;

  case 219:
#line 1273 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mrow><mtable rowspacing=\"0.5ex\">", (yyvsp[(3) - (5)]), "</mtable></mrow>");
  itex2MML_free_string((yyvsp[(3) - (5)]));
}
    break;

  case 220:
#line 1277 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mrow><mtable rowspacing=\"1.0ex\">", (yyvsp[(3) - (5)]), "</mtable></mrow>");
  itex2MML_free_string((yyvsp[(3) - (5)]));
}
    break;

  case 221:
#line 1281 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mrow><mo>(</mo><mrow><mtable rowspacing=\"0.5ex\">", (yyvsp[(3) - (5)]), "</mtable></mrow><mo>)</mo></mrow>");
  itex2MML_free_string((yyvsp[(3) - (5)]));
}
    break;

  case 222:
#line 1285 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mrow><mo>[</mo><mrow><mtable rowspacing=\"0.5ex\">", (yyvsp[(3) - (5)]), "</mtable></mrow><mo>]</mo></mrow>");
  itex2MML_free_string((yyvsp[(3) - (5)]));
}
    break;

  case 223:
#line 1289 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mrow><mo>&VerticalBar;</mo><mrow><mtable rowspacing=\"0.5ex\">", (yyvsp[(3) - (5)]), "</mtable></mrow><mo>&VerticalBar;</mo></mrow>");
  itex2MML_free_string((yyvsp[(3) - (5)]));
}
    break;

  case 224:
#line 1293 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mrow><mo>{</mo><mrow><mtable rowspacing=\"0.5ex\">", (yyvsp[(3) - (5)]), "</mtable></mrow><mo>}</mo></mrow>");
  itex2MML_free_string((yyvsp[(3) - (5)]));
}
    break;

  case 225:
#line 1297 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mrow><mo>&DoubleVerticalBar;</mo><mrow><mtable rowspacing=\"0.5ex\">", (yyvsp[(3) - (5)]), "</mtable></mrow><mo>&DoubleVerticalBar;</mo></mrow>");
  itex2MML_free_string((yyvsp[(3) - (5)]));
}
    break;

  case 226:
#line 1301 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mstyle scriptlevel=\"2\"><mrow><mtable rowspacing=\"0.5ex\">", (yyvsp[(3) - (5)]), "</mtable></mrow></mstyle>");
  itex2MML_free_string((yyvsp[(3) - (5)]));
}
    break;

  case 227:
#line 1305 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mrow><mo>{</mo><mrow><mtable columnalign=\"left left\">", (yyvsp[(3) - (5)]), "</mtable></mrow></mrow>");
  itex2MML_free_string((yyvsp[(3) - (5)]));
}
    break;

  case 228:
#line 1309 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mrow><mtable columnalign=\"right left right left right left right left right left\" columnspacing=\"0em\">", (yyvsp[(3) - (5)]), "</mtable></mrow>");
  itex2MML_free_string((yyvsp[(3) - (5)]));
}
    break;

  case 229:
#line 1313 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<semantics><annotation-xml encoding=\"SVG1.1\">", (yyvsp[(3) - (4)]), "</annotation-xml></semantics>");
  itex2MML_free_string((yyvsp[(3) - (4)]));
}
    break;

  case 230:
#line 1317 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string(" ");
}
    break;

  case 231:
#line 1321 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mrow><mtable columnalign=\"center\" rowspacing=\"0.5ex\">", (yyvsp[(3) - (4)]), "</mtable></mrow>");
  itex2MML_free_string((yyvsp[(3) - (4)]));
}
    break;

  case 232:
#line 1326 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mrow><mtable>", (yyvsp[(3) - (4)]), "</mtable></mrow>");
  itex2MML_free_string((yyvsp[(3) - (4)]));
}
    break;

  case 233:
#line 1330 "itex2MML.y"
    {
  char * s1 = itex2MML_copy3("<mrow><mtable ", (yyvsp[(5) - (8)]), ">");
  (yyval) = itex2MML_copy3(s1, (yyvsp[(7) - (8)]), "</mtable></mrow>");
  itex2MML_free_string(s1);
  itex2MML_free_string((yyvsp[(5) - (8)]));
  itex2MML_free_string((yyvsp[(7) - (8)]));
}
    break;

  case 234:
#line 1338 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string((yyvsp[(1) - (1)]));
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 235:
#line 1342 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3((yyvsp[(1) - (2)]), " ", (yyvsp[(2) - (2)]));
  itex2MML_free_string((yyvsp[(1) - (2)]));
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 236:
#line 1348 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string((yyvsp[(1) - (1)]));
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 237:
#line 1352 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string((yyvsp[(1) - (1)]));
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 238:
#line 1356 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string((yyvsp[(1) - (1)]));
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 239:
#line 1360 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string((yyvsp[(1) - (1)]));
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 240:
#line 1364 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string((yyvsp[(1) - (1)]));
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 241:
#line 1368 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string((yyvsp[(1) - (1)]));
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 242:
#line 1372 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string((yyvsp[(1) - (1)]));
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 243:
#line 1376 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string((yyvsp[(1) - (1)]));
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 244:
#line 1380 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string((yyvsp[(1) - (1)]));
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 245:
#line 1384 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string((yyvsp[(1) - (1)]));
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 246:
#line 1389 "itex2MML.y"
    {
  (yyval) = itex2MML_copy2("columnalign=", (yyvsp[(2) - (2)]));
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 247:
#line 1394 "itex2MML.y"
    {
  (yyval) = itex2MML_copy2("columnalign=", (yyvsp[(2) - (2)]));
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 248:
#line 1399 "itex2MML.y"
    {
  (yyval) = itex2MML_copy2("rowalign=", (yyvsp[(2) - (2)]));
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 249:
#line 1404 "itex2MML.y"
    {
  (yyval) = itex2MML_copy2("align=", (yyvsp[(2) - (2)]));
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 250:
#line 1409 "itex2MML.y"
    {
  (yyval) = itex2MML_copy2("equalrows=", (yyvsp[(2) - (2)]));
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 251:
#line 1414 "itex2MML.y"
    {
  (yyval) = itex2MML_copy2("equalcolumns=", (yyvsp[(2) - (2)]));
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 252:
#line 1419 "itex2MML.y"
    {
  (yyval) = itex2MML_copy2("rowlines=", (yyvsp[(2) - (2)]));
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 253:
#line 1424 "itex2MML.y"
    {
  (yyval) = itex2MML_copy2("columnlines=", (yyvsp[(2) - (2)]));
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 254:
#line 1429 "itex2MML.y"
    {
  (yyval) = itex2MML_copy2("frame=", (yyvsp[(2) - (2)]));
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 255:
#line 1434 "itex2MML.y"
    {
  char * s1 = itex2MML_copy3("rowspacing=", (yyvsp[(2) - (2)]), " columnspacing=");
  (yyval) = itex2MML_copy2(s1, (yyvsp[(2) - (2)]));
  itex2MML_free_string(s1);
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 256:
#line 1441 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string((yyvsp[(1) - (1)]));
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 257:
#line 1445 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3((yyvsp[(1) - (3)]), " ", (yyvsp[(3) - (3)]));
  itex2MML_free_string((yyvsp[(1) - (3)]));
  itex2MML_free_string((yyvsp[(3) - (3)]));
}
    break;

  case 258:
#line 1451 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mtr>", (yyvsp[(1) - (1)]), "</mtr>");
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 259:
#line 1455 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string((yyvsp[(1) - (1)]));
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 260:
#line 1460 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string((yyvsp[(1) - (1)]));
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 261:
#line 1464 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3((yyvsp[(1) - (3)]), " ", (yyvsp[(3) - (3)]));
  itex2MML_free_string((yyvsp[(1) - (3)]));
  itex2MML_free_string((yyvsp[(3) - (3)]));
}
    break;

  case 262:
#line 1470 "itex2MML.y"
    {
  char * s1 = itex2MML_copy3("<mtr ", (yyvsp[(3) - (5)]), ">");
  (yyval) = itex2MML_copy3(s1, (yyvsp[(5) - (5)]), "</mtr>");
  itex2MML_free_string(s1);
  itex2MML_free_string((yyvsp[(3) - (5)]));
  itex2MML_free_string((yyvsp[(5) - (5)]));
}
    break;

  case 263:
#line 1478 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string((yyvsp[(1) - (1)]));
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 264:
#line 1482 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3((yyvsp[(1) - (2)]), " ", (yyvsp[(2) - (2)]));
  itex2MML_free_string((yyvsp[(1) - (2)]));
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 265:
#line 1488 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string((yyvsp[(1) - (1)]));
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 266:
#line 1492 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string((yyvsp[(1) - (1)]));
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 267:
#line 1497 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string("<mtd></mtd>");
}
    break;

  case 268:
#line 1500 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3("<mtd>", (yyvsp[(1) - (1)]), "</mtd>");
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 269:
#line 1504 "itex2MML.y"
    {
  char * s1 = itex2MML_copy3("<mtd ", (yyvsp[(3) - (5)]), ">");
  (yyval) = itex2MML_copy3(s1, (yyvsp[(5) - (5)]), "</mtd>");
  itex2MML_free_string(s1);
  itex2MML_free_string((yyvsp[(3) - (5)]));
  itex2MML_free_string((yyvsp[(5) - (5)]));
}
    break;

  case 270:
#line 1512 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string((yyvsp[(1) - (1)]));
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 271:
#line 1516 "itex2MML.y"
    {
  (yyval) = itex2MML_copy3((yyvsp[(1) - (2)]), " ", (yyvsp[(2) - (2)]));
  itex2MML_free_string((yyvsp[(1) - (2)]));
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 272:
#line 1522 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string((yyvsp[(1) - (1)]));
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 273:
#line 1526 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string((yyvsp[(1) - (1)]));
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 274:
#line 1530 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string((yyvsp[(1) - (1)]));
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 275:
#line 1534 "itex2MML.y"
    {
  (yyval) = itex2MML_copy_string((yyvsp[(1) - (1)]));
  itex2MML_free_string((yyvsp[(1) - (1)]));
}
    break;

  case 276:
#line 1539 "itex2MML.y"
    {
  (yyval) = itex2MML_copy2("rowspan=", (yyvsp[(2) - (2)]));
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;

  case 277:
#line 1544 "itex2MML.y"
    {
  (yyval) = itex2MML_copy2("columnspan=", (yyvsp[(2) - (2)]));
  itex2MML_free_string((yyvsp[(2) - (2)]));
}
    break;


/* Line 1267 of yacc.c.  */
#line 4777 "y.tab.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}


#line 1549 "itex2MML.y"


char * itex2MML_parse (const char * buffer, unsigned long length)
{
  char * mathml = 0;

  int result;

  itex2MML_setup (buffer, length);
  itex2MML_restart ();

  result = itex2MML_yyparse (&mathml);

  if (result && mathml) /* shouldn't happen? */
    {
      itex2MML_free_string (mathml);
      mathml = 0;
    }
  return mathml;
}

int itex2MML_filter (const char * buffer, unsigned long length)
{
  itex2MML_setup (buffer, length);
  itex2MML_restart ();

  return itex2MML_yyparse (0);
}

#define ITEX_DELIMITER_DOLLAR 0
#define ITEX_DELIMITER_DOUBLE 1
#define ITEX_DELIMITER_SQUARE 2

static char * itex2MML_last_error = 0;

static void itex2MML_keep_error (const char * msg)
{
  if (itex2MML_last_error)
    {
      itex2MML_free_string (itex2MML_last_error);
      itex2MML_last_error = 0;
    }
  itex2MML_last_error = itex2MML_copy_escaped (msg);
}

int itex2MML_html_filter (const char * buffer, unsigned long length)
{
  return itex2MML_do_html_filter (buffer, length, 0);
}

int itex2MML_strict_html_filter (const char * buffer, unsigned long length)
{
  return itex2MML_do_html_filter (buffer, length, 1);
}

int itex2MML_do_html_filter (const char * buffer, unsigned long length, const int forbid_markup)
{
  int result = 0;

  int type = 0;
  int skip = 0;
  int match = 0;

  const char * ptr1 = buffer;
  const char * ptr2 = 0;

  const char * end = buffer + length;

  char * mathml = 0;

  void (*save_error_fn) (const char * msg) = itex2MML_error;

  itex2MML_error = itex2MML_keep_error;

 _until_math:
  ptr2 = ptr1;

  while (ptr2 < end)
    {
      if (*ptr2 == '$') break;
      if ((*ptr2 == '\\') && (ptr2 + 1 < end))
	{
	  if (*(ptr2+1) == '[') break;
	}
      ++ptr2;
    }
  if (itex2MML_write)
    (*itex2MML_write) (ptr1, ptr2 - ptr1);

  if (ptr2 == end) goto _finish;

 _until_html:
  ptr1 = ptr2;

  if (ptr2 + 1 < end)
    {
      if ((*ptr2 == '\\') && (*(ptr2+1) == '['))
	{
	  type = ITEX_DELIMITER_SQUARE;
	  ptr2 += 2;
	}
      else if ((*ptr2 == '$') && (*(ptr2+1) == '$'))
	{
	  type = ITEX_DELIMITER_DOUBLE;
	  ptr2 += 2;
	}
      else
	{
	  type = ITEX_DELIMITER_DOLLAR;
	  ptr2 += 2;
	}
    }
  else goto _finish;

  skip = 0;
  match = 0;

  while (ptr2 < end)
    {
      switch (*ptr2)
	{
	case '<':
	case '>':
	  if (forbid_markup == 1) skip = 1;
	  break;

	case '\\':
	  if (ptr2 + 1 < end)
	    {
	      if (*(ptr2 + 1) == '[')
		{
		  skip = 1;
		}
	      else if (*(ptr2 + 1) == ']')
		{
		  if (type == ITEX_DELIMITER_SQUARE)
		    {
		      ptr2 += 2;
		      match = 1;
		    }
		  else
		    {
		      skip = 1;
		    }
		}
	    }
	  break;

	case '$':
	  if (type == ITEX_DELIMITER_SQUARE)
	    {
	      skip = 1;
	    }
	  else if (ptr2 + 1 < end)
	    {
	      if (*(ptr2 + 1) == '$')
		{
		  if (type == ITEX_DELIMITER_DOLLAR)
		    {
		      ptr2++;
		      match = 1;
		    }
		  else
		    {
		      ptr2 += 2;
		      match = 1;
		    }
		}
	      else
		{
		  if (type == ITEX_DELIMITER_DOLLAR)
		    {
		      ptr2++;
		      match = 1;
		    }
		  else
		    {
		      skip = 1;
		    }
		}
	    }
	  else
	    {
	      if (type == ITEX_DELIMITER_DOLLAR)
		{
		  ptr2++;
		  match = 1;
		}
	      else
		{
		  skip = 1;
		}
	    }
	  break;

	default:
	  break;
	}
      if (skip || match) break;

      ++ptr2;
    }
  if (skip)
    {
      if (type == ITEX_DELIMITER_DOLLAR)
	{
	  if (itex2MML_write)
	    (*itex2MML_write) (ptr1, 1);
	  ptr1++;
	}
      else
	{
	  if (itex2MML_write)
	    (*itex2MML_write) (ptr1, 2);
	  ptr1 += 2;
	}
      goto _until_math;
    }
  if (match)
    {
      mathml = itex2MML_parse (ptr1, ptr2 - ptr1);

      if (mathml)
	{
	  if (itex2MML_write_mathml)
	    (*itex2MML_write_mathml) (mathml);
	  itex2MML_free_string (mathml);
	  mathml = 0;
	}
      else
	{
	  ++result;
	  if (itex2MML_write)
	    {
	      if (type == ITEX_DELIMITER_DOLLAR)
		(*itex2MML_write) ("<math xmlns='http://www.w3.org/1998/Math/MathML' display='inline'><merror><mtext>", 0);
	      else
		(*itex2MML_write) ("<math xmlns='http://www.w3.org/1998/Math/MathML' display='block'><merror><mtext>", 0);

	      (*itex2MML_write) (itex2MML_last_error, 0);
	      (*itex2MML_write) ("</mtext></merror></math>", 0);
	    }
	}
      ptr1 = ptr2;

      goto _until_math;
    }
  if (itex2MML_write)
    (*itex2MML_write) (ptr1, ptr2 - ptr1);

 _finish:
  if (itex2MML_last_error)
    {
      itex2MML_free_string (itex2MML_last_error);
      itex2MML_last_error = 0;
    }
  itex2MML_error = save_error_fn;

  return result;
}

