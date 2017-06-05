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




/* Copy the first part of user declarations.  */
#line 1 "yacc.y"

/**
 *
 * $Id: yacc.y,v 1.1 2004/08/28 19:28:18 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2001 LessTif Development Team
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *
 *  Original author:  Geoffrey W. Ritchey
 *                    codesmit@southwind.net
 *
*/ 

#include <LTconfig.h>

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#include "uil.h"	
#include "glue.h"

extern int LineNumber;
extern char *FileName;

#ifdef	YYTEXT_POINTER
extern char *yytext;
#else
extern unsigned char yytext[];	/* Unsigned needed on HP/UX */
#endif
extern int yylex(void);

/* For the prototype police.
   GNU bison up to 1.28 doesn't allow us to use the
   const qualifier here. Shame on it ...
 */ 
void yyerror(char *s);
void yyExit(int line, char *file, char *fmt, ...);


static int False = 0;
static int True = 1;


#define YYDEBUG	1
int yydebug = 1;


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
typedef union YYSTYPE
#line 197 "yacc.y"
{
    char	*string;
}
/* Line 187 of yacc.c.  */
#line 271 "yacc.c"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 284 "yacc.c"

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
#define YYFINAL  4
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   460

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  68
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  30
/* YYNRULES -- Number of rules.  */
#define YYNRULES  136
/* YYNRULES -- Number of states.  */
#define YYNSTATES  385

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   308

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,    67,     2,     2,    64,     2,
      59,    60,    65,    63,    61,    62,     2,    66,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    58,    54,
       2,    55,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    56,     2,    57,     2,     2,     2,     2,
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
      45,    46,    47,    48,    49,    50,    51,    52,    53
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,    11,    12,    16,    20,    24,    28,    29,
      34,    39,    44,    50,    51,    57,    63,    64,    70,    78,
      87,    96,   108,   117,   124,   131,   132,   138,   142,   144,
     150,   156,   162,   163,   169,   173,   177,   183,   187,   193,
     202,   209,   213,   217,   219,   221,   228,   232,   238,   244,
     247,   252,   256,   258,   260,   262,   264,   266,   268,   270,
     272,   274,   283,   292,   301,   302,   308,   309,   316,   325,
     335,   345,   356,   357,   360,   363,   366,   367,   373,   377,
     382,   389,   397,   405,   414,   420,   425,   430,   431,   437,
     443,   450,   455,   456,   458,   462,   466,   470,   472,   476,
     480,   482,   485,   487,   491,   493,   495,   500,   505,   510,
     515,   520,   529,   534,   543,   548,   555,   557,   561,   570,
     579,   584,   589,   595,   599,   609,   618,   619,   626,   627,
     631,   635,   639,   641,   643,   645,   647
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      69,     0,    -1,     7,     4,    71,    70,    25,     7,    54,
      -1,    -1,    70,     8,    73,    -1,    70,    17,    79,    -1,
      70,    31,    82,    -1,    70,    23,    84,    -1,    -1,     5,
      55,     3,    71,    -1,     6,    55,     4,    71,    -1,    28,
      55,     4,    71,    -1,    27,    55,    56,    72,    57,    -1,
      -1,     4,    55,    29,    54,    72,    -1,     4,    55,    38,
      54,    72,    -1,    -1,     4,    58,    90,    54,    73,    -1,
       4,    58,    48,    59,    74,    60,    54,    -1,     4,    58,
      11,    59,    78,    60,    54,    73,    -1,     4,    58,    44,
      59,    77,    60,    54,    73,    -1,     4,    58,    39,    21,
      59,     3,    61,    81,    60,    54,    73,    -1,     4,    58,
      40,    59,     3,    60,    54,    73,    -1,     4,    58,    26,
      90,    54,    73,    -1,     4,    58,    19,    81,    54,    73,
      -1,    -1,    75,    55,     4,    61,    74,    -1,    75,    55,
       4,    -1,     4,    -1,    28,    59,     3,    76,    60,    -1,
      61,    52,    55,    37,    76,    -1,    61,    53,    55,    37,
      76,    -1,    -1,     4,    55,     3,    61,    77,    -1,     4,
      55,     3,    -1,    50,    55,     3,    -1,    50,    55,     3,
      61,    77,    -1,    51,    55,     3,    -1,    51,    55,     3,
      61,    77,    -1,    43,    59,     3,    60,    55,     3,    61,
      77,    -1,    43,    59,     3,    60,    55,     3,    -1,    78,
      61,     3,    -1,    78,    61,     4,    -1,     3,    -1,     4,
      -1,    79,     4,    59,    80,    60,    54,    -1,    79,     4,
      54,    -1,    79,     4,    59,    60,    54,    -1,     4,    59,
      80,    60,    54,    -1,     4,    54,    -1,     4,    59,    60,
      54,    -1,    80,    61,    81,    -1,    81,    -1,    12,    -1,
      13,    -1,    15,    -1,    14,    -1,    16,    -1,    35,    -1,
      43,    -1,    48,    -1,    82,     4,    58,    22,    56,    83,
      57,    54,    -1,    82,     4,    58,    20,    56,    87,    57,
      54,    -1,    82,     4,    58,    24,    56,    95,    57,    54,
      -1,    -1,    83,     4,    55,    90,    54,    -1,    -1,    84,
       4,    58,    19,     4,    54,    -1,    84,     4,    58,     4,
      56,    85,    57,    54,    -1,    84,     4,    58,     4,    29,
      56,    85,    57,    54,    -1,    84,     4,    58,    26,     4,
      56,    85,    57,    54,    -1,    84,     4,    58,    41,    17,
       4,    56,    85,    57,    54,    -1,    -1,    85,    86,    -1,
      85,    88,    -1,    85,    94,    -1,    -1,    20,    56,    87,
      57,    54,    -1,    20,     4,    54,    -1,    87,     4,     4,
      54,    -1,    87,     4,    56,    85,    57,    54,    -1,    87,
       4,    32,    56,    85,    57,    54,    -1,    87,    32,     4,
      56,    85,    57,    54,    -1,    87,     4,    58,     4,    56,
      85,    57,    54,    -1,    87,    32,     4,     4,    54,    -1,
      87,    20,     4,    54,    -1,    87,    41,     4,    54,    -1,
      -1,    22,    56,    89,    57,    54,    -1,    89,     4,    55,
      90,    54,    -1,    89,     4,    55,     4,     4,    54,    -1,
      89,    22,     4,    54,    -1,    -1,    91,    -1,    90,    62,
      91,    -1,    90,    63,    91,    -1,    90,    64,    92,    -1,
      92,    -1,    91,    65,    92,    -1,    91,    66,    92,    -1,
       4,    -1,     9,    49,    -1,     9,    -1,    67,    75,     3,
      -1,     3,    -1,    37,    -1,    33,    59,     4,    60,    -1,
      47,    59,     3,    60,    -1,    33,    59,     3,    60,    -1,
      46,    59,     3,    60,    -1,    45,    59,     3,    60,    -1,
      34,    59,    44,    55,     4,    61,    78,    60,    -1,    34,
      59,    78,    60,    -1,    42,    59,     9,    61,     9,    61,
       9,    60,    -1,    43,    59,     3,    60,    -1,    43,    59,
       3,    61,     4,    60,    -1,    93,    -1,    59,    90,    60,
      -1,    35,    59,     3,    61,    36,    55,    37,    60,    -1,
      35,    59,     4,    61,    36,    55,    37,    60,    -1,    35,
      59,     4,    60,    -1,    35,    59,     3,    60,    -1,    24,
      56,    95,    57,    54,    -1,    24,     4,    54,    -1,    95,
       4,    55,    17,     4,    59,    97,    60,    54,    -1,    95,
       4,    55,    18,    56,    96,    57,    54,    -1,    -1,    96,
       4,    59,    97,    60,    54,    -1,    -1,    97,    61,     9,
      -1,    97,    61,     4,    -1,    97,    61,     3,    -1,     9,
      -1,     4,    -1,     3,    -1,    93,    -1,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   203,   203,   207,   212,   217,   222,   226,   231,   237,
     241,   246,   250,   255,   261,   265,   270,   275,   279,   283,
     287,   291,   295,   299,   303,   308,   313,   317,   323,   327,
     333,   337,   342,   347,   351,   355,   359,   363,   367,   371,
     375,   381,   385,   389,   393,   399,   403,   408,   413,   417,
     422,   429,   433,   439,   443,   447,   451,   455,   459,   463,
     467,   473,   478,   483,   489,   494,   499,   504,   508,   513,
     517,   523,   528,   533,   538,   543,   549,   554,   558,   565,
     569,   573,   578,   582,   586,   590,   594,   599,   604,   610,
     614,   618,   623,   628,   632,   636,   640,   646,   650,   654,
     660,   664,   669,   673,   678,   682,   686,   690,   694,   698,
     703,   707,   711,   715,   719,   723,   727,   731,   737,   742,
     746,   750,   756,   760,   766,   773,   778,   783,   788,   792,
     796,   800,   804,   808,   812,   816,   821
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "STRING", "ID", "VERSION_t", "NAMES",
  "MODULE", "VALUE", "INTEGER", "FLOAT", "STRING_TABLE", "INTEGER_TYPE",
  "FLOAT_TYPE", "STRING_TYPE", "ANY_TYPE", "BOOLEAN_TYPE", "PROCEDURE",
  "PROCEDURES", "IMPORTED", "CONTROLS", "ARGUMENT", "ARGUMENTS", "OBJECT",
  "CALLBACK", "END", "EXPORTED", "OBJECTS", "CHAR_SET", "WIDGET",
  "INC_FILE", "LIST", "UNMANAGED", "KEYSYM", "ICON", "COMPOUND_STRING",
  "SEPARATE", "BOOL", "GADGET", "PRIVATE", "REASON", "USER_DEFINED", "RGB",
  "COLOR", "COLOR_TABLE", "XBITMAPFILE", "XPIXMAPFILE", "FONT",
  "FONT_TABLE", "FONT_UNIT", "BACKGROUND_COLOR", "FOREGROUND_COLOR",
  "RIGHT_TO_LEFT", "SIXTEEN_BIT", "';'", "'='", "'{'", "'}'", "':'", "'('",
  "')'", "','", "'-'", "'+'", "'&'", "'*'", "'/'", "'#'", "$accept",
  "input", "body", "module", "object_list", "initializers", "font_list",
  "character_set", "extra_cs_parms", "color_list", "string_list",
  "procedure_list", "type_list", "type", "list", "list_arg", "object",
  "features", "controls", "control_list", "arguments", "argument_list",
  "add_expr", "mult_expr", "prim_expr", "compound_string", "callbacks",
  "callback_list", "procedure_call_list", "proc_argument_list", 0
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
     305,   306,   307,   308,    59,    61,   123,   125,    58,    40,
      41,    44,    45,    43,    38,    42,    47,    35
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    68,    69,    69,    70,    70,    70,    70,    70,    71,
      71,    71,    71,    71,    72,    72,    72,    73,    73,    73,
      73,    73,    73,    73,    73,    73,    74,    74,    75,    75,
      76,    76,    76,    77,    77,    77,    77,    77,    77,    77,
      77,    78,    78,    78,    78,    79,    79,    79,    79,    79,
      79,    80,    80,    81,    81,    81,    81,    81,    81,    81,
      81,    82,    82,    82,    82,    83,    83,    84,    84,    84,
      84,    84,    84,    85,    85,    85,    85,    86,    86,    87,
      87,    87,    87,    87,    87,    87,    87,    87,    88,    89,
      89,    89,    89,    90,    90,    90,    90,    91,    91,    91,
      92,    92,    92,    92,    92,    92,    92,    92,    92,    92,
      92,    92,    92,    92,    92,    92,    92,    92,    93,    93,
      93,    93,    94,    94,    95,    95,    95,    96,    96,    97,
      97,    97,    97,    97,    97,    97,    97
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     7,     0,     3,     3,     3,     3,     0,     4,
       4,     4,     5,     0,     5,     5,     0,     5,     7,     8,
       8,    11,     8,     6,     6,     0,     5,     3,     1,     5,
       5,     5,     0,     5,     3,     3,     5,     3,     5,     8,
       6,     3,     3,     1,     1,     6,     3,     5,     5,     2,
       4,     3,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     8,     8,     8,     0,     5,     0,     6,     8,     9,
       9,    10,     0,     2,     2,     2,     0,     5,     3,     4,
       6,     7,     7,     8,     5,     4,     4,     0,     5,     5,
       6,     4,     0,     1,     3,     3,     3,     1,     3,     3,
       1,     2,     1,     3,     1,     1,     4,     4,     4,     4,
       4,     8,     4,     8,     4,     6,     1,     3,     8,     8,
       4,     4,     5,     3,     9,     8,     0,     6,     0,     3,
       3,     3,     1,     1,     1,     1,     0
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       3,     0,     0,    13,     1,     0,     0,     0,     0,     8,
       0,     0,     0,     0,     0,    13,    13,    16,    13,    25,
       0,    72,     0,    64,     9,    10,     0,     0,    11,     0,
       4,     0,     5,     7,     0,     6,     0,    12,     0,    49,
       0,     0,     0,     2,     0,     0,     0,   104,   100,   102,
       0,     0,     0,     0,     0,     0,   105,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    93,
      97,   116,    53,    54,    56,    55,    57,    58,    59,    60,
       0,     0,    52,    46,     0,     0,     0,    16,    16,   101,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    28,     0,     0,    25,
       0,     0,     0,     0,     0,    50,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    14,    15,    43,
      44,     0,    25,    25,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   117,     0,   103,    17,    94,    95,
      96,    98,    99,    48,    51,    47,     0,     0,    76,     0,
       0,     0,    87,    66,   126,     0,     0,    24,    23,   108,
     106,     0,   112,   121,     0,   120,     0,     0,     0,     0,
     114,     0,     0,     0,     0,     0,     0,   110,   109,   107,
       0,     0,    32,    45,    76,     0,    67,    76,     0,     0,
       0,     0,    25,    41,    42,     0,     0,     0,     0,    25,
       0,     0,    34,     0,    35,    37,    25,    18,    27,     0,
       0,     0,     0,     0,     0,     0,    73,    74,    75,     0,
      76,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      19,     0,     0,     0,     0,    22,     0,   115,     0,     0,
       0,     0,    20,     0,     0,     0,    29,     0,     0,    87,
      92,     0,   126,    68,     0,     0,     0,     0,    76,     0,
       0,     0,     0,    62,     0,    61,     0,    63,     0,     0,
       0,     0,     0,    33,     0,    36,    38,    26,     0,     0,
      69,    78,     0,     0,   123,     0,    70,     0,    79,    76,
       0,     0,    85,     0,    76,    86,     0,     0,     0,   111,
     118,   119,    25,   113,    40,    32,    32,     0,     0,     0,
       0,     0,    71,     0,     0,    76,    84,     0,    65,     0,
     128,    21,     0,    30,    31,    77,     0,     0,    88,   122,
       0,    80,     0,     0,   136,     0,    39,   100,     0,    91,
      81,     0,    82,   134,   133,   132,   135,     0,     0,     0,
       0,    89,    83,     0,     0,   136,   125,    90,   124,   131,
     130,   129,     0,     0,   127
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     2,    14,     9,    27,    30,   152,   153,   230,   148,
     131,    32,    81,    82,    35,   210,    33,   205,   236,   209,
     237,   303,    68,    69,    70,    71,   238,   211,   355,   367
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -350
static const yytype_int16 yypact[] =
{
       3,    16,    38,   215,  -350,   -20,    -6,    22,    47,  -350,
      80,    76,    56,   106,   214,   215,   215,   118,   215,   124,
     134,  -350,   149,  -350,  -350,  -350,   103,   116,  -350,    73,
    -350,   -38,   173,   181,   146,   184,    77,  -350,     8,  -350,
     111,   113,   160,  -350,   178,   193,   204,  -350,  -350,   195,
     205,   190,    27,   211,   247,   248,  -350,   259,   250,   251,
     252,   253,   254,   255,   256,   257,    27,    11,   186,    71,
    -350,  -350,  -350,  -350,  -350,  -350,  -350,  -350,  -350,  -350,
     237,   147,  -350,  -350,   127,    18,   175,   118,   118,  -350,
     220,   241,   192,   231,   130,   249,   258,   265,   299,   315,
     101,   316,   317,   318,    11,   209,  -350,   263,   320,   124,
      27,    27,    27,    27,    27,  -350,   270,   190,   273,   202,
      74,   321,   322,   311,   274,   275,   276,  -350,  -350,  -350,
    -350,   216,   124,   124,   269,   277,   278,   218,   221,   223,
     331,   279,   280,   225,   281,   283,   285,   288,   284,   286,
     287,   289,   290,   293,  -350,   332,  -350,  -350,    71,    71,
    -350,  -350,  -350,  -350,  -350,  -350,   291,   282,  -350,   297,
     296,   349,  -350,  -350,  -350,   300,   271,  -350,  -350,  -350,
    -350,   351,  -350,  -350,   323,  -350,   324,   295,   303,   352,
    -350,   354,   359,   360,   361,   362,   312,  -350,  -350,  -350,
     313,   364,   308,  -350,  -350,   133,  -350,  -350,   314,    75,
       9,    14,   124,  -350,  -350,   310,   319,   325,   190,   124,
     326,   328,   329,   333,   330,   334,   124,  -350,   335,   235,
     337,   141,    28,   327,    29,   338,  -350,  -350,  -350,   144,
    -350,    61,   368,   369,   371,   340,   343,   345,   346,   348,
    -350,   220,   339,   341,   344,  -350,   370,  -350,   101,   350,
     101,   101,  -350,    11,   353,   355,  -350,   357,   358,  -350,
    -350,   363,  -350,  -350,   365,   156,   366,   347,  -350,   373,
     367,    94,   372,  -350,    27,  -350,   272,  -350,   232,   356,
     374,   375,   376,  -350,   378,  -350,  -350,  -350,   377,   381,
    -350,  -350,    88,    24,  -350,    19,  -350,   379,  -350,  -350,
     159,   382,  -350,   383,  -350,  -350,   197,   380,   384,  -350,
    -350,  -350,   124,  -350,   385,   308,   308,   387,   388,   396,
     390,   391,  -350,   162,   393,  -350,  -350,   169,  -350,   389,
    -350,  -350,   101,  -350,  -350,  -350,    54,   395,  -350,  -350,
     397,  -350,   172,   398,   206,    25,  -350,   402,   203,  -350,
    -350,   399,  -350,  -350,  -350,  -350,  -350,   236,   400,   401,
     403,  -350,  -350,   404,   208,   206,  -350,  -350,  -350,  -350,
    -350,  -350,   238,   406,  -350
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -350,  -350,  -350,   212,   213,  -108,   119,   342,   -23,  -252,
     -91,  -350,   301,   -49,  -350,  -350,  -350,  -200,  -350,   117,
    -350,  -350,   -52,   194,    35,  -349,  -350,   135,  -350,    40
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_uint16 yytable[] =
{
      92,   157,    91,   137,   231,   366,   293,   239,   295,   296,
       1,    47,    48,   246,   105,   106,    39,    49,   248,    50,
       3,    40,   120,   248,   177,   178,   366,    51,   328,   368,
      47,    48,   268,   271,    52,    10,    49,   121,     4,   107,
     275,    53,    54,    55,   122,    56,   329,    57,    58,    11,
      59,    60,    61,    62,    63,    64,    65,    47,   357,   123,
      53,    54,    55,    49,    56,   276,   247,    66,   164,    59,
      60,   249,    62,    63,    64,    67,   331,    12,   310,   241,
      16,   330,   369,    15,   269,   272,    66,    53,    54,    55,
     356,    56,   241,   277,    67,   242,    59,    60,   313,    62,
      63,    64,    13,   167,   250,   144,    45,   243,   242,   333,
      18,   255,    17,    66,   337,    46,   244,   278,   262,   279,
     243,    67,    26,    72,    73,    74,    75,    76,    29,   244,
     168,    38,   245,   129,   130,   352,   113,   114,    31,    72,
      73,    74,    75,    76,   145,   327,    77,   160,   161,   162,
     314,   146,   147,   232,    78,   233,    34,   234,    36,    79,
     288,   232,    77,   233,   232,   234,   233,    83,   234,   254,
      78,    80,    84,    37,   136,    79,   232,    41,   233,   232,
     234,   233,   232,   234,   233,    42,   234,   118,    44,   232,
     235,   233,   232,   234,   233,   124,   234,   125,   267,   126,
      43,   274,    72,    73,    74,    75,    76,   116,   117,   363,
     364,   379,   380,   307,   341,   365,   334,   381,    85,   350,
       5,     6,    19,   129,   130,    77,   353,    24,    25,   361,
      28,    20,   316,    78,   134,   135,    86,    21,    79,    22,
     109,    55,     7,     8,    89,    23,   133,    87,   110,   111,
     112,   338,   138,   139,   110,   111,   112,   371,    88,   110,
     111,   112,   166,   117,    90,   110,   111,   112,   141,   154,
      93,   110,   111,   112,   213,   214,   175,   176,   182,   176,
      96,   183,   184,   185,   186,   190,   191,   264,   265,   317,
     318,   115,   319,   176,   358,   132,   373,   374,   383,   374,
     127,   128,   343,   344,   158,   159,    94,    95,   142,    97,
      98,    99,   100,   101,   102,   103,   104,   140,   143,   149,
     150,   151,   155,   156,   163,   169,   170,   165,   171,   179,
     172,   173,   174,   181,   187,   202,   192,   180,   204,   188,
     194,   189,   193,   195,   196,   203,   197,   198,   201,   199,
     200,   206,   207,   208,   212,   215,   218,   219,   221,   216,
     217,   220,   222,   223,   224,   225,   226,   227,   228,   229,
     240,   251,   280,   281,   252,   282,   289,   311,   290,   292,
     253,   324,   297,   270,   339,   119,   302,   256,   257,     0,
     258,   260,   273,   259,   283,   261,   263,   266,   284,   285,
     347,   286,   287,   309,   291,   294,   370,   305,   298,   108,
     299,   300,   301,     0,   325,   382,   320,   304,   326,   306,
     308,   312,     0,     0,     0,     0,   315,     0,     0,   322,
       0,     0,     0,   332,   321,     0,   323,   336,   335,     0,
     340,   345,     0,   346,   348,   349,   342,   351,   354,   359,
       0,   360,   362,   372,     0,   376,     0,   377,   378,   375,
     384
};

static const yytype_int16 yycheck[] =
{
      52,   109,    51,    94,   204,   354,   258,   207,   260,   261,
       7,     3,     4,     4,    66,     4,    54,     9,     4,    11,
       4,    59,     4,     4,   132,   133,   375,    19,     4,     4,
       3,     4,     4,     4,    26,    55,     9,    19,     0,    28,
     240,    33,    34,    35,    26,    37,    22,    39,    40,    55,
      42,    43,    44,    45,    46,    47,    48,     3,     4,    41,
      33,    34,    35,     9,    37,     4,    57,    59,   117,    42,
      43,    57,    45,    46,    47,    67,    57,    55,   278,     4,
       4,    57,    57,     3,    56,    56,    59,    33,    34,    35,
     342,    37,     4,    32,    67,    20,    42,    43,     4,    45,
      46,    47,    55,    29,   212,     4,    29,    32,    20,   309,
       4,   219,    56,    59,   314,    38,    41,    56,   226,    58,
      32,    67,     4,    12,    13,    14,    15,    16,     4,    41,
      56,    58,    57,     3,     4,   335,    65,    66,     4,    12,
      13,    14,    15,    16,    43,    57,    35,   112,   113,   114,
      56,    50,    51,    20,    43,    22,     7,    24,    55,    48,
     251,    20,    35,    22,    20,    24,    22,    54,    24,   218,
      43,    60,    59,    57,    44,    48,    20,     4,    22,    20,
      24,    22,    20,    24,    22,     4,    24,    60,     4,    20,
      57,    22,    20,    24,    22,    20,    24,    22,    57,    24,
      54,    57,    12,    13,    14,    15,    16,    60,    61,     3,
       4,     3,     4,    57,   322,     9,    57,     9,    58,    57,
       5,     6,     8,     3,     4,    35,    57,    15,    16,    57,
      18,    17,   284,    43,     3,     4,    58,    23,    48,    25,
      54,    35,    27,    28,    49,    31,    54,    54,    62,    63,
      64,    54,     3,     4,    62,    63,    64,    54,    54,    62,
      63,    64,    60,    61,    59,    62,    63,    64,     3,    60,
      59,    62,    63,    64,     3,     4,    60,    61,    60,    61,
      21,    60,    61,    60,    61,    60,    61,    52,    53,    17,
      18,    54,    60,    61,   346,    54,    60,    61,    60,    61,
      87,    88,   325,   326,   110,   111,    59,    59,     9,    59,
      59,    59,    59,    59,    59,    59,    59,    59,     3,     3,
       3,     3,    59,     3,    54,     4,     4,    54,    17,    60,
      56,    56,    56,    55,     3,     3,    55,    60,    56,    60,
      55,    61,    59,    55,    60,    54,    60,    60,    55,    60,
      60,    54,    56,     4,    54,     4,    61,    54,     4,    36,
      36,     9,     3,     3,     3,     3,    54,    54,     4,    61,
      56,    61,     4,     4,    55,     4,    37,     4,    37,     9,
      55,     3,   263,    56,     4,    84,   269,    61,    60,    -1,
      61,    61,    54,    60,    54,    61,    61,    60,    55,    54,
       4,    55,    54,    56,    60,    55,     4,   272,    55,    67,
      55,    54,    54,    -1,    37,   375,    60,    54,    37,    54,
      54,    54,    -1,    -1,    -1,    -1,    54,    -1,    -1,    54,
      -1,    -1,    -1,    54,    60,    -1,    60,    54,    56,    -1,
      56,    54,    -1,    55,    54,    54,    61,    54,    59,    54,
      -1,    54,    54,    54,    -1,    54,    -1,    54,    54,    59,
      54
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     7,    69,     4,     0,     5,     6,    27,    28,    71,
      55,    55,    55,    55,    70,     3,     4,    56,     4,     8,
      17,    23,    25,    31,    71,    71,     4,    72,    71,     4,
      73,     4,    79,    84,     7,    82,    55,    57,    58,    54,
      59,     4,     4,    54,     4,    29,    38,     3,     4,     9,
      11,    19,    26,    33,    34,    35,    37,    39,    40,    42,
      43,    44,    45,    46,    47,    48,    59,    67,    90,    91,
      92,    93,    12,    13,    14,    15,    16,    35,    43,    48,
      60,    80,    81,    54,    59,    58,    58,    54,    54,    49,
      59,    81,    90,    59,    59,    59,    21,    59,    59,    59,
      59,    59,    59,    59,    59,    90,     4,    28,    75,    54,
      62,    63,    64,    65,    66,    54,    60,    61,    60,    80,
       4,    19,    26,    41,    20,    22,    24,    72,    72,     3,
       4,    78,    54,    54,     3,     4,    44,    78,     3,     4,
      59,     3,     9,     3,     4,    43,    50,    51,    77,     3,
       3,     3,    74,    75,    60,    59,     3,    73,    91,    91,
      92,    92,    92,    54,    81,    54,    60,    29,    56,     4,
       4,    17,    56,    56,    56,    60,    61,    73,    73,    60,
      60,    55,    60,    60,    61,    60,    61,     3,    60,    61,
      60,    61,    55,    59,    55,    55,    60,    60,    60,    60,
      60,    55,     3,    54,    56,    85,    54,    56,     4,    87,
      83,    95,    54,     3,     4,     4,    36,    36,    61,    54,
       9,     4,     3,     3,     3,     3,    54,    54,     4,    61,
      76,    85,    20,    22,    24,    57,    86,    88,    94,    85,
      56,     4,    20,    32,    41,    57,     4,    57,     4,    57,
      73,    61,    55,    55,    81,    73,    61,    60,    61,    60,
      61,    61,    73,    61,    52,    53,    60,    57,     4,    56,
      56,     4,    56,    54,    57,    85,     4,    32,    56,    58,
       4,     4,     4,    54,    55,    54,    55,    54,    78,    37,
      37,    60,     9,    77,    55,    77,    77,    74,    55,    55,
      54,    54,    87,    89,    54,    95,    54,    57,    54,    56,
      85,     4,    54,     4,    56,    54,    90,    17,    18,    60,
      60,    60,    54,    60,     3,    37,    37,    57,     4,    22,
      57,    57,    54,    85,    57,    56,    54,    85,    54,     4,
      56,    73,    61,    76,    76,    54,    55,     4,    54,    54,
      57,    54,    85,    57,    59,    96,    77,     4,    90,    54,
      54,    57,    54,     3,     4,     9,    93,    97,     4,    57,
       4,    54,    54,    60,    61,    59,    54,    54,    54,     3,
       4,     9,    97,    60,    54
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
#line 204 "yacc.y"
    {
		}
    break;

  case 3:
#line 207 "yacc.y"
    {
		    __MrmWarn(LOC,"Empty input file\n");
		}
    break;

  case 4:
#line 213 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		    (yyval.string) = (yyvsp[(1) - (3)].string);
		}
    break;

  case 5:
#line 218 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		    (yyval.string) = (yyvsp[(1) - (3)].string);
		}
    break;

  case 6:
#line 223 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		}
    break;

  case 7:
#line 227 "yacc.y"
    {
		    (yyval.string) = body_OBJECT_object((yyvsp[(1) - (3)].string),(yyvsp[(3) - (3)].string));
		}
    break;

  case 8:
#line 231 "yacc.y"
    {
		    (yyval.string) = NULL;
		}
    break;

  case 9:
#line 238 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		}
    break;

  case 10:
#line 242 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		    (yyval.string) = (yyvsp[(1) - (4)].string);
		}
    break;

  case 11:
#line 247 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		}
    break;

  case 12:
#line 251 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		}
    break;

  case 13:
#line 255 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		    (yyval.string) = NULL;
		}
    break;

  case 14:
#line 262 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		}
    break;

  case 15:
#line 266 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		}
    break;

  case 16:
#line 270 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		}
    break;

  case 17:
#line 276 "yacc.y"
    {
		    MakeTable((yyvsp[(1) - (5)].string),(yyvsp[(3) - (5)].string), 0);
		}
    break;

  case 18:
#line 280 "yacc.y"
    {
		    MakeTable((yyvsp[(1) - (7)].string), (yyvsp[(5) - (7)].string), 0);
		}
    break;

  case 19:
#line 284 "yacc.y"
    {
		    MakeTable((yyvsp[(1) - (8)].string),(yyvsp[(5) - (8)].string),0);
		}
    break;

  case 20:
#line 288 "yacc.y"
    {
		    MakeTable((yyvsp[(1) - (8)].string), (yyvsp[(5) - (8)].string), 0);
		}
    break;

  case 21:
#line 292 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		}
    break;

  case 22:
#line 296 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		}
    break;

  case 23:
#line 300 "yacc.y"
    {
		    MakeTable((yyvsp[(1) - (6)].string), (yyvsp[(4) - (6)].string), 1);
		}
    break;

  case 25:
#line 308 "yacc.y"
    {
			(yyval.string) = NULL;
	       	}
    break;

  case 26:
#line 314 "yacc.y"
    {
		    (yyval.string) = AddFont((yyvsp[(1) - (5)].string), (yyvsp[(3) - (5)].string), (yyvsp[(5) - (5)].string));
		}
    break;

  case 27:
#line 318 "yacc.y"
    {
		    (yyval.string) = AddFont((yyvsp[(1) - (3)].string), (yyvsp[(3) - (3)].string), NULL);
		}
    break;

  case 28:
#line 324 "yacc.y"
    {
		    (yyval.string) = CharSetName(NULL, (yyvsp[(1) - (1)].string));
		}
    break;

  case 29:
#line 328 "yacc.y"
    {
		    (yyval.string) = CharSetName((yyvsp[(4) - (5)].string), (yyvsp[(3) - (5)].string));
		}
    break;

  case 30:
#line 334 "yacc.y"
    {
		    (yyval.string) = CharSetRToL((yyvsp[(5) - (5)].string), (int)(long)(yyvsp[(4) - (5)].string));
		}
    break;

  case 31:
#line 338 "yacc.y"
    {
		    (yyval.string) = CharSet16Bit((yyvsp[(5) - (5)].string), (int)(long)(yyvsp[(4) - (5)].string));
		}
    break;

  case 32:
#line 342 "yacc.y"
    {
		    (yyval.string) = MakeNewCharSet();
		}
    break;

  case 33:
#line 348 "yacc.y"
    {
		    (yyval.string) = AddColor((yyvsp[(1) - (5)].string),(yyvsp[(3) - (5)].string), (yyvsp[(5) - (5)].string), 1);
		}
    break;

  case 34:
#line 352 "yacc.y"
    {
		    (yyval.string) = AddColor((yyvsp[(1) - (3)].string),(yyvsp[(3) - (3)].string), NULL, 1);
		}
    break;

  case 35:
#line 356 "yacc.y"
    {
		    (yyval.string) = AddColor(__MrmStore("\"_bg\""),(yyvsp[(3) - (3)].string), NULL, 0);
		}
    break;

  case 36:
#line 360 "yacc.y"
    {
		    (yyval.string) = AddColor(__MrmStore("\"_bg\""), (yyvsp[(3) - (5)].string), (yyvsp[(5) - (5)].string), 0);
		}
    break;

  case 37:
#line 364 "yacc.y"
    {
		    (yyval.string) = AddColor(__MrmStore("\"_fg\""), (yyvsp[(3) - (3)].string), NULL, 0);
		}
    break;

  case 38:
#line 368 "yacc.y"
    {
		    (yyval.string) = AddColor(__MrmStore("\"_fg\""), (yyvsp[(3) - (5)].string), (yyvsp[(5) - (5)].string), 0);
		}
    break;

  case 39:
#line 372 "yacc.y"
    {
		    (yyval.string) = AddColor((yyvsp[(3) - (8)].string), (yyvsp[(6) - (8)].string), (yyvsp[(8) - (8)].string), 0);
		}
    break;

  case 40:
#line 376 "yacc.y"
    {
		    (yyval.string) = AddColor((yyvsp[(3) - (6)].string), (yyvsp[(6) - (6)].string), NULL, 0);
		}
    break;

  case 41:
#line 382 "yacc.y"
    {
		    (yyval.string) = InsertString((yyvsp[(1) - (3)].string), (yyvsp[(3) - (3)].string));
		}
    break;

  case 42:
#line 386 "yacc.y"
    {
		    (yyval.string) = InsertString((yyvsp[(1) - (3)].string), (yyvsp[(3) - (3)].string));
		}
    break;

  case 43:
#line 390 "yacc.y"
    {
		    (yyval.string) = InsertString(NULL,(yyvsp[(1) - (1)].string));
		}
    break;

  case 44:
#line 394 "yacc.y"
    {
		    (yyval.string) = InsertString(NULL, (yyvsp[(1) - (1)].string));
		}
    break;

  case 45:
#line 400 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		}
    break;

  case 46:
#line 404 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		    (yyval.string) = (yyvsp[(1) - (3)].string);
		}
    break;

  case 47:
#line 409 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		    (yyval.string) = (yyvsp[(1) - (5)].string);
		}
    break;

  case 48:
#line 414 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		}
    break;

  case 49:
#line 418 "yacc.y"
    {
		    __MrmWarn(LOC, "NO OP\n");
		    (yyval.string) = (yyvsp[(1) - (2)].string);
		}
    break;

  case 50:
#line 423 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		    (yyval.string) = (yyvsp[(1) - (4)].string);
		}
    break;

  case 51:
#line 430 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		}
    break;

  case 52:
#line 434 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		}
    break;

  case 53:
#line 440 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		}
    break;

  case 54:
#line 444 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		}
    break;

  case 55:
#line 448 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		}
    break;

  case 56:
#line 452 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		}
    break;

  case 57:
#line 456 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		}
    break;

  case 58:
#line 460 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		}
    break;

  case 59:
#line 464 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		}
    break;

  case 60:
#line 468 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		}
    break;

  case 61:
#line 474 "yacc.y"
    {
		    AddAttributeList((yyvsp[(2) - (8)].string), (yyvsp[(6) - (8)].string));
		    (yyval.string) = NULL;
		}
    break;

  case 62:
#line 479 "yacc.y"
    {
		    AddControlList((yyvsp[(2) - (8)].string), (yyvsp[(6) - (8)].string));
		    (yyval.string) = NULL;
		}
    break;

  case 63:
#line 484 "yacc.y"
    {
		    AddCallbackList((yyvsp[(2) - (8)].string), (yyvsp[(6) - (8)].string));
		    (yyval.string) = NULL;
		}
    break;

  case 64:
#line 489 "yacc.y"
    {
		    (yyval.string) = NULL;
		}
    break;

  case 65:
#line 495 "yacc.y"
    {
		    (yyval.string) = (char *)arglist_arglist_ID_addexpr((yyvsp[(1) - (5)].string), (yyvsp[(2) - (5)].string), (yyvsp[(4) - (5)].string));
		}
    break;

  case 66:
#line 499 "yacc.y"
    {
		    (yyval.string) = NULL;
		}
    break;

  case 67:
#line 505 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		}
    break;

  case 68:
#line 509 "yacc.y"
    {
		    ID_ID_features((yyvsp[(2) - (8)].string),(yyvsp[(4) - (8)].string),(yyvsp[(6) - (8)].string));
		    (yyval.string) = (yyvsp[(6) - (8)].string);
		}
    break;

  case 69:
#line 514 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		}
    break;

  case 70:
#line 518 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		    ID_ID_features((yyvsp[(2) - (9)].string), (yyvsp[(5) - (9)].string), (yyvsp[(7) - (9)].string));
		    (yyval.string) = (yyvsp[(7) - (9)].string);
		}
    break;

  case 71:
#line 524 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		}
    break;

  case 72:
#line 528 "yacc.y"
    {
		    /* __MrmWarn(LOC,"Empty object\n"); */
		}
    break;

  case 73:
#line 534 "yacc.y"
    {
		    features_controls((yyvsp[(1) - (2)].string),(yyvsp[(2) - (2)].string));
		    (yyval.string) = (yyvsp[(1) - (2)].string);
		}
    break;

  case 74:
#line 539 "yacc.y"
    {
		    features_arguments((yyvsp[(1) - (2)].string),(yyvsp[(2) - (2)].string));
		    (yyval.string) = (yyvsp[(1) - (2)].string);
		}
    break;

  case 75:
#line 544 "yacc.y"
    {
		    features_callbacks((yyvsp[(1) - (2)].string),(yyvsp[(2) - (2)].string));
		    (yyval.string) = (yyvsp[(1) - (2)].string);
		}
    break;

  case 76:
#line 549 "yacc.y"
    {
		    (yyval.string) = (char *)Features_NULL();
		}
    break;

  case 77:
#line 555 "yacc.y"
    {
		    (yyval.string) = (yyvsp[(3) - (5)].string);
		}
    break;

  case 78:
#line 559 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		    (yyval.string) = NULL;
		}
    break;

  case 79:
#line 566 "yacc.y"
    {
		    (yyval.string) = (char *)controllist_controllist_ID_ID((yyvsp[(1) - (4)].string),(yyvsp[(2) - (4)].string),(yyvsp[(3) - (4)].string),1);
		}
    break;

  case 80:
#line 570 "yacc.y"
    {
		    (yyval.string) = (char *)control_list_ID_features((yyvsp[(1) - (6)].string), (yyvsp[(2) - (6)].string), (yyvsp[(4) - (6)].string), 1);
		}
    break;

  case 81:
#line 574 "yacc.y"
    {   /* $2 may be wrong for this type of call */
		    /* I confused this rule for the next one */
		    (yyval.string) = (char *)control_list_ID_features((yyvsp[(1) - (7)].string), (yyvsp[(2) - (7)].string), (yyvsp[(5) - (7)].string), 0);
		}
    break;

  case 82:
#line 579 "yacc.y"
    {
		    (yyval.string) = (char *)control_list_ID_features((yyvsp[(1) - (7)].string),(yyvsp[(3) - (7)].string), (yyvsp[(5) - (7)].string), 0);
		}
    break;

  case 83:
#line 583 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		}
    break;

  case 84:
#line 587 "yacc.y"
    {
		    (yyval.string) = (char *)controllist_controllist_ID_ID((yyvsp[(1) - (5)].string),(yyvsp[(3) - (5)].string),(yyvsp[(4) - (5)].string),0);
		}
    break;

  case 85:
#line 591 "yacc.y"
    {
		    (yyval.string) = InheritControls((yyvsp[(1) - (4)].string), (yyvsp[(3) - (4)].string));
		}
    break;

  case 86:
#line 595 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		}
    break;

  case 87:
#line 599 "yacc.y"
    {
			(yyval.string) = NULL;
		}
    break;

  case 88:
#line 605 "yacc.y"
    {
		    (yyval.string) = (yyvsp[(3) - (5)].string);
		}
    break;

  case 89:
#line 611 "yacc.y"
    {
		    (yyval.string) = arglist_arglist_ID_addexpr((yyvsp[(1) - (5)].string),(yyvsp[(2) - (5)].string),(yyvsp[(4) - (5)].string));
		}
    break;

  case 90:
#line 615 "yacc.y"
    {
		    (yyval.string) = WidgetArgument((yyvsp[(1) - (6)].string), (yyvsp[(2) - (6)].string), (yyvsp[(5) - (6)].string));
	     	}
    break;

  case 91:
#line 619 "yacc.y"
    {
		    (yyval.string) = InheritArgument((yyvsp[(1) - (4)].string), (yyvsp[(3) - (4)].string));
		}
    break;

  case 92:
#line 623 "yacc.y"
    {
		    (yyval.string) = NULL;
		}
    break;

  case 93:
#line 629 "yacc.y"
    {
		    (yyval.string) = (yyvsp[(1) - (1)].string);
		}
    break;

  case 94:
#line 633 "yacc.y"
    {
		    (yyval.string) = Subtract((yyvsp[(1) - (3)].string),(yyvsp[(3) - (3)].string));
		}
    break;

  case 95:
#line 637 "yacc.y"
    {
		    (yyval.string) = Add((yyvsp[(1) - (3)].string),(yyvsp[(3) - (3)].string));
		}
    break;

  case 96:
#line 641 "yacc.y"
    {
		    (yyval.string) = AppendStrings((yyvsp[(1) - (3)].string), (yyvsp[(3) - (3)].string));
		}
    break;

  case 97:
#line 647 "yacc.y"
    {
		    (yyval.string) = (yyvsp[(1) - (1)].string);
		}
    break;

  case 98:
#line 651 "yacc.y"
    {
		    (yyval.string) = Multiply((yyvsp[(1) - (3)].string),(yyvsp[(3) - (3)].string));
		}
    break;

  case 99:
#line 655 "yacc.y"
    {
		    (yyval.string) = Divide((yyvsp[(1) - (3)].string),(yyvsp[(3) - (3)].string));
		}
    break;

  case 100:
#line 661 "yacc.y"
    {
		    (yyval.string) = (char *) expr_ID((yyvsp[(1) - (1)].string));
		}
    break;

  case 101:
#line 665 "yacc.y"
    {
		    __MrmWarn(LOC, "Font Unit not implemented yet\n");
		    (yyval.string) = (char *) prim_exp((yyvsp[(1) - (2)].string));
		}
    break;

  case 102:
#line 670 "yacc.y"
    {
		    (yyval.string) = (char *)prim_exp((yyvsp[(1) - (1)].string));
		}
    break;

  case 103:
#line 674 "yacc.y"
    {
		    fprintf(stderr,"BLAB BLAB ID = %s\n", (yyvsp[(3) - (3)].string));
		    (yyval.string) = (char *) expr_STRING((yyvsp[(3) - (3)].string), (yyvsp[(2) - (3)].string),/*Compound?*/ False);
		}
    break;

  case 104:
#line 679 "yacc.y"
    {
		    (yyval.string) = (char *) expr_STRING((yyvsp[(1) - (1)].string), NULL, /*Compound?*/ False);
		}
    break;

  case 105:
#line 683 "yacc.y"
    {
		    (yyval.string) = (char *) expr_BOOL((yyvsp[(1) - (1)].string));
		}
    break;

  case 106:
#line 687 "yacc.y"
    {
		    (yyval.string) = (char *) keysym((yyvsp[(3) - (4)].string));
		}
    break;

  case 107:
#line 691 "yacc.y"
    {
		    (yyval.string) = (char *) font((yyvsp[(3) - (4)].string));
		}
    break;

  case 108:
#line 695 "yacc.y"
    {
		    (yyval.string) = (char *) keysym((yyvsp[(3) - (4)].string));
		}
    break;

  case 109:
#line 699 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP\n");
		    (yyval.string) = NULL;
		}
    break;

  case 110:
#line 704 "yacc.y"
    {	
		    (yyval.string) = (char *) bitmap((yyvsp[(3) - (4)].string));
		}
    break;

  case 111:
#line 708 "yacc.y"
    {
		    (yyval.string) = (char *) pixmap((yyvsp[(5) - (8)].string), (yyvsp[(7) - (8)].string));
		}
    break;

  case 112:
#line 712 "yacc.y"
    {
		    (yyval.string) = pixmap(NULL,(yyvsp[(3) - (4)].string));
		}
    break;

  case 113:
#line 716 "yacc.y"
    {
		    (yyval.string) = color(NULL, (yyvsp[(3) - (8)].string), (yyvsp[(5) - (8)].string), (yyvsp[(7) - (8)].string));
		}
    break;

  case 114:
#line 720 "yacc.y"
    {
		    (yyval.string) = color((yyvsp[(3) - (4)].string), 0, 0, 0);
		}
    break;

  case 115:
#line 724 "yacc.y"
    {
		    (yyval.string) = color((yyvsp[(3) - (6)].string), 0, 0, 0); /* For Now  FIX ME */
		}
    break;

  case 116:
#line 728 "yacc.y"
    {
		    (yyval.string) = (yyvsp[(1) - (1)].string);
		}
    break;

  case 117:
#line 732 "yacc.y"
    {	
		    (yyval.string) = (yyvsp[(2) - (3)].string);
		}
    break;

  case 118:
#line 738 "yacc.y"
    {
		    /* string, separate, IsAddress */
		    (yyval.string) = (char *) expr_STRING_Compound((yyvsp[(3) - (8)].string),(int)(long)(yyvsp[(7) - (8)].string),False);
		}
    break;

  case 119:
#line 743 "yacc.y"
    {
		    (yyval.string) = (char *) expr_STRING_Compound((yyvsp[(3) - (8)].string),(int)(long)(yyvsp[(7) - (8)].string),True);
		}
    break;

  case 120:
#line 747 "yacc.y"
    {
		    (yyval.string) = (char *) expr_STRING_Compound((yyvsp[(3) - (4)].string), False, True);
		}
    break;

  case 121:
#line 751 "yacc.y"
    {
		    (yyval.string) = (char *) expr_STRING_Compound((yyvsp[(3) - (4)].string), False, False);
		}
    break;

  case 122:
#line 757 "yacc.y"
    {
		    (yyval.string) = (yyvsp[(3) - (5)].string);
		}
    break;

  case 123:
#line 761 "yacc.y"
    {
		    (yyval.string) = InheritCallback((yyvsp[(2) - (3)].string));
		}
    break;

  case 124:
#line 767 "yacc.y"
    {
		    (yyval.string) = (char *)callbacklist_callbacklist_PROCID_arglist((yyvsp[(1) - (9)].string),
									  (yyvsp[(2) - (9)].string),
									  (yyvsp[(5) - (9)].string),
									  (yyvsp[(7) - (9)].string));
		}
    break;

  case 125:
#line 774 "yacc.y"
    {
		    __MrmWarn(LOC,"NO OP************\n");
		}
    break;

  case 126:
#line 778 "yacc.y"
    {
		    (yyval.string) = NULL;
		}
    break;

  case 127:
#line 784 "yacc.y"
    {
		    __MrmWarn(LOC, "NO OP\n");
		}
    break;

  case 128:
#line 788 "yacc.y"
    {
		}
    break;

  case 129:
#line 793 "yacc.y"
    {
		    (yyval.string) = Parameter((yyvsp[(1) - (3)].string), prim_exp((yyvsp[(3) - (3)].string)));
		}
    break;

  case 130:
#line 797 "yacc.y"
    {
		    (yyval.string) = Parameter((yyvsp[(1) - (3)].string), expr_ID((yyvsp[(3) - (3)].string)));
		}
    break;

  case 131:
#line 801 "yacc.y"
    {
		    (yyval.string) = Parameter((yyvsp[(1) - (3)].string), expr_STRING((yyvsp[(3) - (3)].string), NULL, 0));
		}
    break;

  case 132:
#line 805 "yacc.y"
    {
		    (yyval.string) = Parameter(NULL, prim_exp((yyvsp[(1) - (1)].string)));
		}
    break;

  case 133:
#line 809 "yacc.y"
    {
		    (yyval.string) = Parameter(NULL, expr_ID((yyvsp[(1) - (1)].string)));
		}
    break;

  case 134:
#line 813 "yacc.y"
    {
		    (yyval.string) = Parameter(NULL, expr_STRING((yyvsp[(1) - (1)].string), NULL, 0));
		}
    break;

  case 135:
#line 817 "yacc.y"
    {
		    (yyval.string) = Parameter(NULL, expr_STRING((yyvsp[(1) - (1)].string), NULL, 1));
		}
    break;

  case 136:
#line 821 "yacc.y"
    {
		    (yyval.string) = Parameter(NULL, NULL);
		}
    break;


/* Line 1267 of yacc.c.  */
#line 2777 "yacc.c"
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


#line 827 "yacc.y"



void
yyerror(char *s)
{
    fprintf(stderr,"%s:%d: %s\n", FileName, LineNumber, s);
    fprintf(stderr," current token \'%s\'\n", yytext);
}


void
yyExit(int line, char *file, char *fmt, ...)
{
    va_list ap;

    yyerror("");

    va_start (ap, fmt);

    fprintf(stderr,"%s:%d:", file, line);
    vfprintf(stderr, fmt, ap);

    va_end(ap);

    exit(1);
}

