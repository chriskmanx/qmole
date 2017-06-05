
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C
   
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
#define YYBISON_VERSION "2.4.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 1

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Using locations.  */
#define YYLSP_NEEDED 1

/* Substitute the variable and function names.  */
#define yyparse         webidl_parse
#define yylex           webidl_lex
#define yyerror         webidl_error
#define yylval          webidl_lval
#define yychar          webidl_char
#define yydebug         webidl_debug
#define yynerrs         webidl_nerrs
#define yylloc          webidl_lloc

/* Copy the first part of user declarations.  */

/* Line 189 of yacc.c  */
#line 1 "src/webidl-parser.y"


/* This is a bison parser for Web IDL
 *
 * This file is part of nsgenbind.
 * Licensed under the MIT License,
 *                http://www.opensource.org/licenses/mit-license.php
 * Copyright 2012 Vincent Sanders <vince@netsurf-browser.org>
 *
 * Derived from the the grammar in apendix A of W3C WEB IDL
 *   http://www.w3.org/TR/WebIDL/
 */

#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include <math.h>

#include "webidl-ast.h"

#include "webidl-parser.h"
#include "webidl-lexer.h"

char *errtxt;

static void
webidl_error(YYLTYPE *locp, struct webidl_node **winbind_ast, const char *str)
{
  locp = locp;
  winbind_ast = winbind_ast;
    errtxt = strdup(str);
}



/* Line 189 of yacc.c  */
#line 118 "build-arm-apple-darwin9-arm-apple-darwin9-release-binary/webidl-parser.c"

/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 1
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif


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

/* Line 214 of yacc.c  */
#line 46 "src/webidl-parser.y"

    int attr;
    long value;
    bool isit;
    char* text;
    struct webidl_node *node;



/* Line 214 of yacc.c  */
#line 223 "build-arm-apple-darwin9-arm-apple-darwin9-release-binary/webidl-parser.c"
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


/* Copy the second part of user declarations.  */


/* Line 264 of yacc.c  */
#line 248 "build-arm-apple-darwin9-arm-apple-darwin9-release-binary/webidl-parser.c"

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
YYID (int yyi)
#else
static int
YYID (yyi)
    int yyi;
#endif
{
  return yyi;
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
	 || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
	     && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE) + sizeof (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

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
# define YYSTACK_RELOCATE(Stack_alloc, Stack)				\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack_alloc, Stack, yysize);			\
	Stack = &yyptr->Stack_alloc;					\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   660

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  75
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  74
/* YYNRULES -- Number of rules.  */
#define YYNRULES  208
/* YYNRULES -- Number of states.  */
#define YYNSTATES  336

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   314

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      66,    67,     2,     2,    65,    68,    71,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    64,    62,
      72,    63,    73,    74,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    69,     2,    70,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    60,     2,    61,     2,     2,     2,     2,
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
      55,    56,    57,    58,    59
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     4,     8,    10,    12,    14,    16,    18,
      20,    22,    24,    27,    29,    31,    33,    41,    44,    46,
      48,    55,    56,    60,    62,    64,    72,    73,    77,    82,
      89,    90,    93,    95,    97,   105,   106,   110,   111,   114,
     121,   124,   125,   129,   137,   143,   148,   155,   157,   159,
     161,   163,   165,   167,   169,   172,   174,   176,   179,   181,
     183,   185,   187,   189,   196,   197,   199,   200,   202,   205,
     207,   209,   210,   213,   215,   217,   219,   221,   223,   230,
     231,   233,   234,   237,   238,   242,   245,   250,   254,   256,
     258,   259,   261,   263,   265,   269,   270,   275,   276,   280,
     285,   290,   295,   298,   299,   301,   302,   307,   312,   317,
     320,   322,   324,   326,   328,   330,   332,   334,   336,   338,
     340,   342,   344,   346,   348,   350,   352,   354,   356,   358,
     360,   362,   364,   366,   368,   370,   372,   374,   376,   378,
     380,   382,   384,   386,   388,   390,   392,   394,   396,   398,
     400,   402,   404,   406,   408,   410,   412,   414,   416,   418,
     420,   422,   424,   426,   428,   430,   432,   434,   436,   439,
     441,   444,   451,   453,   456,   461,   462,   466,   469,   472,
     475,   481,   484,   487,   490,   493,   495,   497,   499,   501,
     503,   506,   508,   510,   512,   515,   517,   519,   522,   523,
     525,   526,   530,   533,   534,   538,   539,   541,   543
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
      76,     0,    -1,    -1,    76,   124,    77,    -1,     1,    -1,
      78,    -1,    81,    -1,    86,    -1,    92,    -1,    95,    -1,
      99,    -1,   100,    -1,     7,    79,    -1,    80,    -1,    98,
      -1,    80,    -1,    27,    54,    94,    60,    84,    61,    62,
      -1,    38,    82,    -1,    83,    -1,    89,    -1,    27,    54,
      60,    84,    61,    62,    -1,    -1,    84,   124,    85,    -1,
     101,    -1,   105,    -1,    13,    54,    94,    60,    87,    61,
      62,    -1,    -1,   124,    88,    87,    -1,   132,    54,    90,
      62,    -1,    13,    54,    60,    87,    61,    62,    -1,    -1,
      63,    91,    -1,   102,    -1,    57,    -1,    18,    54,    94,
      60,    93,    61,    62,    -1,    -1,   124,   122,    93,    -1,
      -1,    64,    54,    -1,    16,    54,    60,    96,    61,    62,
      -1,    57,    97,    -1,    -1,    65,    57,    97,    -1,    54,
      63,   148,    66,   116,    67,    62,    -1,    49,   124,   132,
      54,    62,    -1,    54,    23,    54,    62,    -1,     9,   138,
      54,    63,   102,    62,    -1,   103,    -1,   104,    -1,    55,
      -1,    32,    -1,    48,    -1,    19,    -1,    56,    -1,    68,
      25,    -1,    25,    -1,    30,    -1,    47,   106,    -1,   107,
      -1,   110,    -1,   107,    -1,   114,    -1,    62,    -1,   108,
     109,     4,   132,    54,    62,    -1,    -1,    26,    -1,    -1,
      40,    -1,   111,   114,    -1,    45,    -1,   112,    -1,    -1,
     113,   112,    -1,    22,    -1,    42,    -1,    10,    -1,    12,
      -1,     8,    -1,   148,   115,    66,   116,    67,    62,    -1,
      -1,    54,    -1,    -1,   118,   117,    -1,    -1,    65,   118,
     117,    -1,   124,   119,    -1,    36,   132,   120,    90,    -1,
     132,   121,   120,    -1,   130,    -1,    54,    -1,    -1,    15,
      -1,   101,    -1,   123,    -1,   132,    54,    62,    -1,    -1,
      69,   126,   125,    70,    -1,    -1,    65,   126,   125,    -1,
      66,   128,    67,   127,    -1,    69,   128,    70,   127,    -1,
      60,   128,    61,   127,    -1,   129,   127,    -1,    -1,   126,
      -1,    -1,    66,   128,    67,   128,    -1,    69,   128,    70,
     128,    -1,    60,   128,    61,   128,    -1,   131,   128,    -1,
      55,    -1,    56,    -1,    54,    -1,    57,    -1,    58,    -1,
      68,    -1,    71,    -1,    15,    -1,    64,    -1,    62,    -1,
      72,    -1,    63,    -1,    73,    -1,    74,    -1,    11,    -1,
      46,    -1,    25,    -1,    30,    -1,     3,    -1,     5,    -1,
       6,    -1,    14,    -1,    19,    -1,    20,    -1,    28,    -1,
      32,    -1,    33,    -1,    34,    -1,    37,    -1,    36,    -1,
      43,    -1,    44,    -1,    48,    -1,    51,    -1,    52,    -1,
     130,    -1,     4,    -1,     7,    -1,     9,    -1,    10,    -1,
      12,    -1,    13,    -1,    16,    -1,    18,    -1,    22,    -1,
      23,    -1,    26,    -1,    27,    -1,     8,    -1,    38,    -1,
      42,    -1,    45,    -1,    47,    -1,    49,    -1,    50,    -1,
     129,    -1,    65,    -1,   133,    -1,   134,   145,    -1,   137,
      -1,     3,   146,    -1,    66,   135,    37,   135,   136,    67,
      -1,   137,    -1,   134,   145,    -1,     3,    69,    70,   145,
      -1,    -1,    37,   135,   136,    -1,   139,   145,    -1,    46,
     145,    -1,    54,   145,    -1,    43,    72,   132,    73,   147,
      -1,    33,   145,    -1,    11,   145,    -1,   139,   147,    -1,
      54,   147,    -1,   142,    -1,   140,    -1,     5,    -1,     6,
      -1,    34,    -1,    50,   141,    -1,   141,    -1,    20,    -1,
      14,    -1,    51,   143,    -1,   143,    -1,    44,    -1,    28,
     144,    -1,    -1,    28,    -1,    -1,    69,    70,   145,    -1,
      74,   146,    -1,    -1,    69,    70,   145,    -1,    -1,    74,
      -1,   132,    -1,    52,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   195,   195,   199,   205,   215,   217,   219,   221,   223,
     225,   227,   233,   238,   243,   245,   250,   285,   293,   295,
     300,   332,   336,   394,   396,   401,   408,   411,   416,   421,
     427,   430,   436,   438,   443,   450,   453,   459,   463,   471,
     479,   483,   486,   491,   499,   507,   539,   557,   559,   561,
     566,   574,   579,   587,   595,   603,   611,   622,   627,   629,
     634,   636,   642,   650,   672,   676,   685,   689,   697,   706,
     708,   712,   715,   720,   722,   724,   726,   728,   733,   750,
     754,   766,   770,   779,   783,   792,   800,   808,   820,   822,
     828,   832,   840,   842,   847,   853,   857,   866,   870,   880,
     885,   891,   897,   907,   911,   920,   924,   933,   943,   953,
     962,   969,   974,   979,   984,   989,   994,   999,  1004,  1009,
    1014,  1019,  1024,  1029,  1034,  1039,  1044,  1049,  1054,  1059,
    1064,  1069,  1074,  1079,  1084,  1089,  1094,  1099,  1104,  1109,
    1114,  1119,  1124,  1129,  1134,  1139,  1147,  1152,  1157,  1162,
    1167,  1172,  1177,  1182,  1187,  1192,  1197,  1202,  1207,  1212,
    1217,  1222,  1227,  1232,  1237,  1245,  1250,  1258,  1263,  1272,
    1274,  1282,  1290,  1292,  1294,  1298,  1301,  1306,  1311,  1316,
    1323,  1328,  1333,  1341,  1346,  1358,  1360,  1362,  1367,  1372,
    1380,  1387,  1392,  1397,  1405,  1412,  1417,  1422,  1435,  1439,
    1448,  1452,  1457,  1466,  1470,  1477,  1480,  1485,  1487
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "TOK_ANY", "TOK_ATTRIBUTE",
  "TOK_BOOLEAN", "TOK_BYTE", "TOK_CALLBACK", "TOK_LEGACYCALLER",
  "TOK_CONST", "TOK_CREATOR", "TOK_DATE", "TOK_DELETER", "TOK_DICTIONARY",
  "TOK_DOUBLE", "TOK_ELLIPSIS", "TOK_ENUM", "TOK_EOL", "TOK_EXCEPTION",
  "TOK_FALSE", "TOK_FLOAT", "TOK_GETRAISES", "TOK_GETTER",
  "TOK_IMPLEMENTS", "TOK_IN", "TOK_INFINITY", "TOK_INHERIT",
  "TOK_INTERFACE", "TOK_LONG", "TOK_MODULE", "TOK_NAN", "TOK_NATIVE",
  "TOK_NULL_LITERAL", "TOK_OBJECT", "TOK_OCTET", "TOK_OMITTABLE",
  "TOK_OPTIONAL", "TOK_OR", "TOK_PARTIAL", "TOK_RAISES", "TOK_READONLY",
  "TOK_SETRAISES", "TOK_SETTER", "TOK_SEQUENCE", "TOK_SHORT", "TOK_STATIC",
  "TOK_STRING", "TOK_STRINGIFIER", "TOK_TRUE", "TOK_TYPEDEF",
  "TOK_UNRESTRICTED", "TOK_UNSIGNED", "TOK_VOID", "TOK_POUND_SIGN",
  "TOK_IDENTIFIER", "TOK_INT_LITERAL", "TOK_FLOAT_LITERAL",
  "TOK_STRING_LITERAL", "TOK_OTHER_LITERAL", "TOK_JAVADOC", "'{'", "'}'",
  "';'", "'='", "':'", "','", "'('", "')'", "'-'", "'['", "']'", "'.'",
  "'<'", "'>'", "'?'", "$accept", "Definitions", "Definition",
  "CallbackOrInterface", "CallbackRestOrInterface", "Interface", "Partial",
  "PartialDefinition", "PartialInterface", "InterfaceMembers",
  "InterfaceMember", "Dictionary", "DictionaryMembers", "DictionaryMember",
  "PartialDictionary", "Default", "DefaultValue", "Exception",
  "ExceptionMembers", "Inheritance", "Enum", "EnumValueList", "EnumValues",
  "CallbackRest", "Typedef", "ImplementsStatement", "Const", "ConstValue",
  "BooleanLiteral", "FloatLiteral", "AttributeOrOperation",
  "StringifierAttributeOrOperation", "Attribute", "Inherit", "ReadOnly",
  "Operation", "Qualifiers", "Specials", "Special", "OperationRest",
  "OptionalIdentifier", "ArgumentList", "Arguments", "Argument",
  "OptionalOrRequiredArgument", "ArgumentName", "Ellipsis",
  "ExceptionMember", "ExceptionField", "ExtendedAttributeList",
  "ExtendedAttributes", "ExtendedAttribute", "ExtendedAttributeRest",
  "ExtendedAttributeInner", "Other", "ArgumentNameKeyword", "OtherOrComma",
  "Type", "SingleType", "UnionType", "UnionMemberType", "UnionMemberTypes",
  "NonAnyType", "ConstType", "PrimitiveType", "UnrestrictedFloatType",
  "FloatType", "UnsignedIntegerType", "IntegerType", "OptionalLong",
  "TypeSuffix", "TypeSuffixStartingWithArray", "Null", "ReturnType", 0
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
     123,   125,    59,    61,    58,    44,    40,    41,    45,    91,
      93,    46,    60,    62,    63
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    75,    76,    76,    76,    77,    77,    77,    77,    77,
      77,    77,    78,    78,    79,    79,    80,    81,    82,    82,
      83,    84,    84,    85,    85,    86,    87,    87,    88,    89,
      90,    90,    91,    91,    92,    93,    93,    94,    94,    95,
      96,    97,    97,    98,    99,   100,   101,   102,   102,   102,
     102,   103,   103,   104,   104,   104,   104,   105,   105,   105,
     106,   106,   106,   107,   108,   108,   109,   109,   110,   111,
     111,   112,   112,   113,   113,   113,   113,   113,   114,   115,
     115,   116,   116,   117,   117,   118,   119,   119,   120,   120,
     121,   121,   122,   122,   123,   124,   124,   125,   125,   126,
     126,   126,   126,   127,   127,   128,   128,   128,   128,   128,
     129,   129,   129,   129,   129,   129,   129,   129,   129,   129,
     129,   129,   129,   129,   129,   129,   129,   129,   129,   129,
     129,   129,   129,   129,   129,   129,   129,   129,   129,   129,
     129,   129,   129,   129,   129,   129,   130,   130,   130,   130,
     130,   130,   130,   130,   130,   130,   130,   130,   130,   130,
     130,   130,   130,   130,   130,   131,   131,   132,   132,   133,
     133,   134,   135,   135,   135,   136,   136,   137,   137,   137,
     137,   137,   137,   138,   138,   139,   139,   139,   139,   139,
     140,   140,   141,   141,   142,   142,   143,   143,   144,   144,
     145,   145,   145,   146,   146,   147,   147,   148,   148
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     3,     1,     1,     1,     1,     1,     1,
       1,     1,     2,     1,     1,     1,     7,     2,     1,     1,
       6,     0,     3,     1,     1,     7,     0,     3,     4,     6,
       0,     2,     1,     1,     7,     0,     3,     0,     2,     6,
       2,     0,     3,     7,     5,     4,     6,     1,     1,     1,
       1,     1,     1,     1,     2,     1,     1,     2,     1,     1,
       1,     1,     1,     6,     0,     1,     0,     1,     2,     1,
       1,     0,     2,     1,     1,     1,     1,     1,     6,     0,
       1,     0,     2,     0,     3,     2,     4,     3,     1,     1,
       0,     1,     1,     1,     3,     0,     4,     0,     3,     4,
       4,     4,     2,     0,     1,     0,     4,     4,     4,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     2,     1,
       2,     6,     1,     2,     4,     0,     3,     2,     2,     2,
       5,     2,     2,     2,     2,     1,     1,     1,     1,     1,
       2,     1,     1,     1,     2,     1,     1,     2,     0,     1,
       0,     3,     2,     0,     3,     0,     1,     1,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     4,    95,     1,     0,     0,   128,   146,   129,   130,
     147,   158,   148,   149,   124,   150,   151,   131,   117,   152,
     153,   132,   133,   154,   155,   126,   156,   157,   134,   127,
     135,   136,   137,   139,   138,   159,   160,   140,   141,   161,
     125,   162,   142,   163,   164,   143,   144,   112,   110,   111,
     113,   114,   105,   119,   121,   118,   105,   115,   105,   116,
     120,   122,   123,    97,   103,   145,     0,     0,     0,     0,
       0,     0,    95,     0,     3,     5,    13,     6,     7,     8,
       9,    10,    11,   105,   166,   105,   105,     0,   165,   105,
       0,     0,     0,     0,   104,   102,     0,    12,    15,    14,
      37,     0,    37,    37,     0,     0,    17,    18,    19,     0,
       0,     0,     0,     0,   103,   109,   103,   103,    97,    96,
       0,     0,     0,     0,     0,     0,     0,     0,   203,   187,
     188,   200,   193,   192,   198,   200,   189,     0,   196,   200,
       0,     0,   200,     0,     0,   167,   200,   169,   200,   186,
     191,   185,   195,     0,   105,   105,   105,   101,    99,   100,
      98,   208,   207,     0,    38,    95,    41,     0,    95,    21,
      95,    21,     0,   170,     0,   203,   182,   199,   197,   181,
       0,   178,   190,   194,   179,     0,   200,     0,   172,     0,
     168,   177,    45,   108,   106,   107,    95,     0,     0,     0,
      40,     0,     0,     0,    95,     0,    95,   200,   200,   202,
       0,     0,   173,     0,    44,     0,    83,     0,     0,    95,
       0,    41,    39,     0,     0,    92,    95,    93,     0,     0,
      71,     0,     0,   204,   201,   205,   200,   175,     0,    95,
      82,     0,    85,    90,    25,    27,    30,    42,    34,   205,
       0,   205,    36,     0,    16,    77,    75,    76,    73,    65,
      74,    69,    64,    22,    23,    24,    58,    66,    59,     0,
      70,    71,    29,    20,   206,   180,   174,     0,     0,    43,
      83,     0,    91,     0,     0,     0,   184,     0,   183,    94,
      62,    57,    60,    61,    79,    67,     0,    68,    72,   175,
     171,    84,    89,    30,    88,    87,    52,    55,    56,    50,
      51,    49,    53,    33,     0,    31,    32,    47,    48,    28,
       0,    80,     0,     0,   176,    86,    54,     0,    95,     0,
      46,     0,     0,     0,    63,    78
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     2,    74,    75,    97,    76,    77,   106,   107,   204,
     263,    78,   197,   219,   108,   285,   315,    79,   202,   122,
      80,   167,   200,    99,    81,    82,   225,   316,   317,   318,
     265,   291,   266,   267,   296,   268,   269,   270,   271,   293,
     322,   215,   240,   216,   242,   303,   283,   226,   227,   198,
      93,    94,    95,    87,    88,    65,    89,   162,   145,   146,
     187,   278,   147,   250,   148,   149,   150,   151,   152,   178,
     176,   173,   275,   294
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -212
static const yytype_int16 yypact[] =
{
      22,  -212,    11,  -212,   309,   163,  -212,  -212,  -212,  -212,
    -212,  -212,  -212,  -212,  -212,  -212,  -212,  -212,  -212,  -212,
    -212,  -212,  -212,  -212,  -212,  -212,  -212,  -212,  -212,  -212,
    -212,  -212,  -212,  -212,  -212,  -212,  -212,  -212,  -212,  -212,
    -212,  -212,  -212,  -212,  -212,  -212,  -212,  -212,  -212,  -212,
    -212,  -212,   235,  -212,  -212,  -212,   235,  -212,   235,  -212,
    -212,  -212,  -212,   -41,   309,  -212,   -18,   -20,    28,    33,
      45,    19,    -7,    60,  -212,  -212,  -212,  -212,  -212,  -212,
    -212,  -212,  -212,   235,  -212,   235,   235,    42,  -212,   235,
      38,    36,   309,    46,  -212,  -212,    56,  -212,  -212,  -212,
      44,    61,    44,    44,    66,    69,  -212,  -212,  -212,   515,
      71,    68,    63,    64,   309,  -212,   309,   309,   -41,  -212,
     381,    77,    79,    83,    84,    85,    86,    88,    74,  -212,
    -212,   -11,  -212,  -212,   122,   -11,  -212,    81,  -212,   -11,
      17,    -1,   -11,   557,    97,  -212,   -11,  -212,   -11,  -212,
    -212,  -212,  -212,    94,   235,   235,   235,  -212,  -212,  -212,
    -212,  -212,  -212,    91,  -212,   -36,    95,   100,   -14,  -212,
     -36,  -212,    93,  -212,    98,    74,  -212,  -212,  -212,  -212,
     515,  -212,  -212,  -212,  -212,   105,   -11,   132,  -212,   115,
    -212,  -212,  -212,  -212,  -212,  -212,    25,   117,   515,   123,
    -212,   120,   124,   431,     6,   125,    20,   -11,   -11,  -212,
     111,   118,  -212,   557,  -212,   126,   127,   473,   136,   -36,
     135,    95,  -212,   138,   108,  -212,   -14,  -212,   149,   143,
     187,   144,   145,  -212,  -212,   134,   -11,   173,   152,    -7,
    -212,   515,  -212,   196,  -212,  -212,   153,  -212,  -212,   134,
     164,   134,  -212,   157,  -212,  -212,  -212,  -212,  -212,  -212,
    -212,  -212,   121,  -212,  -212,  -212,  -212,   180,  -212,   381,
    -212,     8,  -212,  -212,  -212,  -212,  -212,   557,   154,  -212,
     127,   606,  -212,   606,    -4,   160,  -212,   162,  -212,  -212,
    -212,  -212,  -212,  -212,   169,  -212,   222,  -212,  -212,   173,
    -212,  -212,  -212,   153,  -212,  -212,  -212,  -212,  -212,  -212,
    -212,  -212,  -212,  -212,   203,  -212,  -212,  -212,  -212,  -212,
      29,  -212,   165,   515,  -212,  -212,  -212,   168,    25,   179,
    -212,   185,   174,   194,  -212,  -212
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -212,  -212,  -212,  -212,  -212,   193,  -212,  -212,  -212,    99,
    -212,  -212,  -163,  -212,  -212,   -68,  -212,  -212,    40,   -34,
    -212,  -212,    43,  -212,  -212,  -212,    58,   -46,  -212,  -212,
    -212,  -212,    13,  -212,  -212,  -212,  -212,     5,  -212,    27,
    -212,   -26,    14,    72,  -212,    47,  -212,  -212,  -212,    -2,
     192,     4,   -16,   -44,     1,  -195,  -212,  -108,  -212,  -140,
    -211,    34,  -139,  -212,   114,  -212,   200,  -212,   207,  -212,
    -129,   130,  -147,   224
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -82
static const yytype_int16 yytable[] =
{
       5,   144,   237,   186,   188,    64,   179,   205,    63,    70,
     181,     3,    90,   184,    91,   306,   255,   190,   256,   191,
     257,   307,    -2,     1,    92,   -26,   308,   134,   309,    -2,
     258,   132,   104,     4,   100,    -2,    96,   133,    -2,   111,
      -2,   112,   113,   138,   310,   115,   105,   -35,   306,    -2,
     260,   311,   312,   313,   307,     4,   245,   212,   174,   308,
      -2,   309,     4,   175,   314,    64,   299,   229,   124,   125,
     109,    -2,   210,   186,   188,     4,    -2,   310,   233,   234,
       4,   232,   101,   110,   311,   312,   304,   102,   304,     4,
     220,    -2,   -81,    64,     4,   228,   118,   314,   157,   103,
     158,   159,   286,   114,   288,   116,   117,   276,   121,   243,
     193,   194,   195,   129,   130,    64,   119,    64,    64,   120,
     126,   123,   132,   127,   128,   153,   129,   130,   133,   154,
     155,   164,   131,   281,   156,   132,   134,   186,   188,   165,
     166,   133,   136,   172,   168,   169,   170,   259,   171,   134,
     177,   189,   138,   180,   135,   136,   192,   196,   140,   141,
     199,   201,   249,   207,   137,   138,   203,   139,   208,   213,
      66,   140,   141,   161,   211,   142,    67,   214,   218,    68,
     221,    69,   222,   290,   235,   223,   231,   143,   236,   246,
      70,   -64,   239,   238,   217,   255,   224,   256,   244,   257,
     248,    71,   230,   253,   230,   254,   272,   273,   274,   258,
     277,   282,    72,   259,   279,   329,   284,    73,   287,   289,
     295,   300,   319,   321,   203,   320,   323,   -64,   326,   260,
     330,   328,   261,   332,   262,   325,   334,   217,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,   333,    20,    21,    22,   335,    23,    24,    98,
      25,    26,    27,    28,   247,    29,   252,    30,    31,    32,
     206,    33,    34,    35,   327,   292,   298,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,   264,    47,
      48,    49,    50,    51,   301,    83,   297,    53,    54,    55,
      84,    85,   331,    57,    86,   209,    59,    60,    61,    62,
     160,   280,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,   217,    20,    21,    22,
     305,    23,    24,   324,    25,    26,    27,    28,   251,    29,
     182,    30,    31,    32,   163,    33,    34,    35,   183,     0,
       0,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,     0,    47,    48,    49,    50,    51,     0,    52,
       0,    53,    54,    55,     0,    56,     0,    57,    58,     0,
      59,    60,    61,    62,   128,     0,   129,   130,     0,     0,
       0,     0,   131,     0,     0,   132,     0,     0,     0,     0,
       0,   133,     0,     0,     0,     0,     0,     0,     0,   134,
       0,     0,     0,     0,   135,   136,     0,     0,     0,     0,
       0,     0,     0,     0,   137,   138,     0,   139,     0,     0,
       0,   140,   141,   161,   128,   142,   129,   130,     0,     0,
     224,     0,   131,     0,     0,   132,     0,   143,     0,     0,
       0,   133,     0,     0,     0,     0,     0,     0,     0,   134,
       0,     0,     0,     0,   135,   136,     0,     0,     0,     0,
       0,     0,     0,     0,   137,   138,   128,   139,   129,   130,
       0,   140,   141,     0,   131,   142,     0,   132,     0,     0,
       0,     0,     0,   133,     0,     0,     0,   143,     0,     0,
       0,   134,     0,     0,     0,     0,   135,   136,     0,   241,
       0,     0,     0,     0,     0,     0,   137,   138,   128,   139,
     129,   130,     0,   140,   141,     0,   131,   142,     0,   132,
       0,     0,     0,     0,     0,   133,     0,     0,     0,   143,
       0,     0,     0,   134,     0,     0,     0,     0,   135,   136,
       0,     0,     0,     0,     0,     0,     0,     0,   137,   138,
     185,   139,   129,   130,     0,   140,   141,     0,   131,   142,
       0,   132,     0,     0,     0,     0,     0,   133,     0,     0,
       0,   143,     0,     0,     0,   134,     0,     0,     0,     0,
     135,   136,     0,     0,     0,     0,     0,     0,     0,     0,
     137,   138,     0,   139,     0,     0,     0,   140,   141,     0,
       7,   142,     0,    10,    11,    12,    13,     0,    15,    16,
       0,     0,    19,   143,    20,     0,     0,     0,    23,    24,
       0,     0,    26,    27,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    35,     0,     0,     0,    36,     0,
       0,    39,     0,    41,     0,    43,    44,     0,     0,     0,
     302
};

static const yytype_int16 yycheck[] =
{
       2,   109,   213,   143,   143,     4,   135,   170,     4,    27,
     139,     0,    56,   142,    58,    19,     8,   146,    10,   148,
      12,    25,     0,     1,    65,    61,    30,    28,    32,     7,
      22,    14,    13,    69,    54,    13,    54,    20,    16,    83,
      18,    85,    86,    44,    48,    89,    27,    61,    19,    27,
      42,    55,    56,    57,    25,    69,   219,   186,    69,    30,
      38,    32,    69,    74,    68,    64,   277,    61,   102,   103,
      72,    49,   180,   213,   213,    69,    54,    48,   207,   208,
      69,    61,    54,    23,    55,    56,   281,    54,   283,    69,
     198,    69,    67,    92,    69,   203,    92,    68,   114,    54,
     116,   117,   249,    61,   251,    67,    70,   236,    64,   217,
     154,   155,   156,     5,     6,   114,    70,   116,   117,    63,
      54,    60,    14,    54,     3,    54,     5,     6,    20,    61,
      67,    54,    11,   241,    70,    14,    28,   277,   277,    60,
      57,    20,    34,    69,    60,    60,    60,    26,    60,    28,
      28,    54,    44,    72,    33,    34,    62,    66,    50,    51,
      65,    61,    54,    70,    43,    44,   168,    46,    70,    37,
       7,    50,    51,    52,    69,    54,    13,    62,    61,    16,
      57,    18,    62,    62,    73,    61,    61,    66,    70,    54,
      27,     4,    65,    67,   196,     8,     9,    10,    62,    12,
      62,    38,   204,    54,   206,    62,    62,    62,    74,    22,
      37,    15,    49,    26,    62,   323,    63,    54,    54,    62,
      40,    67,    62,    54,   226,    63,     4,    40,    25,    42,
      62,    66,    45,    54,    47,   303,    62,   239,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    67,    18,    19,    20,    62,    22,    23,    66,
      25,    26,    27,    28,   221,    30,   226,    32,    33,    34,
     171,    36,    37,    38,   320,   262,   271,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,   230,    54,
      55,    56,    57,    58,   280,    60,   269,    62,    63,    64,
      65,    66,   328,    68,    69,   175,    71,    72,    73,    74,
     118,   239,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,   328,    18,    19,    20,
     283,    22,    23,   299,    25,    26,    27,    28,   224,    30,
     140,    32,    33,    34,   120,    36,    37,    38,   141,    -1,
      -1,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    54,    55,    56,    57,    58,    -1,    60,
      -1,    62,    63,    64,    -1,    66,    -1,    68,    69,    -1,
      71,    72,    73,    74,     3,    -1,     5,     6,    -1,    -1,
      -1,    -1,    11,    -1,    -1,    14,    -1,    -1,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    28,
      -1,    -1,    -1,    -1,    33,    34,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    43,    44,    -1,    46,    -1,    -1,
      -1,    50,    51,    52,     3,    54,     5,     6,    -1,    -1,
       9,    -1,    11,    -1,    -1,    14,    -1,    66,    -1,    -1,
      -1,    20,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    28,
      -1,    -1,    -1,    -1,    33,    34,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    43,    44,     3,    46,     5,     6,
      -1,    50,    51,    -1,    11,    54,    -1,    14,    -1,    -1,
      -1,    -1,    -1,    20,    -1,    -1,    -1,    66,    -1,    -1,
      -1,    28,    -1,    -1,    -1,    -1,    33,    34,    -1,    36,
      -1,    -1,    -1,    -1,    -1,    -1,    43,    44,     3,    46,
       5,     6,    -1,    50,    51,    -1,    11,    54,    -1,    14,
      -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    66,
      -1,    -1,    -1,    28,    -1,    -1,    -1,    -1,    33,    34,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    43,    44,
       3,    46,     5,     6,    -1,    50,    51,    -1,    11,    54,
      -1,    14,    -1,    -1,    -1,    -1,    -1,    20,    -1,    -1,
      -1,    66,    -1,    -1,    -1,    28,    -1,    -1,    -1,    -1,
      33,    34,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      43,    44,    -1,    46,    -1,    -1,    -1,    50,    51,    -1,
       4,    54,    -1,     7,     8,     9,    10,    -1,    12,    13,
      -1,    -1,    16,    66,    18,    -1,    -1,    -1,    22,    23,
      -1,    -1,    26,    27,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    38,    -1,    -1,    -1,    42,    -1,
      -1,    45,    -1,    47,    -1,    49,    50,    -1,    -1,    -1,
      54
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     1,    76,     0,    69,   124,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      18,    19,    20,    22,    23,    25,    26,    27,    28,    30,
      32,    33,    34,    36,    37,    38,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    54,    55,    56,
      57,    58,    60,    62,    63,    64,    66,    68,    69,    71,
      72,    73,    74,   126,   129,   130,     7,    13,    16,    18,
      27,    38,    49,    54,    77,    78,    80,    81,    86,    92,
      95,    99,   100,    60,    65,    66,    69,   128,   129,   131,
     128,   128,    65,   125,   126,   127,    54,    79,    80,    98,
      54,    54,    54,    54,    13,    27,    82,    83,    89,   124,
      23,   128,   128,   128,    61,   128,    67,    70,   126,    70,
      63,    64,    94,    60,    94,    94,    54,    54,     3,     5,
       6,    11,    14,    20,    28,    33,    34,    43,    44,    46,
      50,    51,    54,    66,   132,   133,   134,   137,   139,   140,
     141,   142,   143,    54,    61,    67,    70,   127,   127,   127,
     125,    52,   132,   148,    54,    60,    57,    96,    60,    60,
      60,    60,    69,   146,    69,    74,   145,    28,   144,   145,
      72,   145,   141,   143,   145,     3,   134,   135,   137,    54,
     145,   145,    62,   128,   128,   128,    66,    87,   124,    65,
      97,    61,    93,   124,    84,    87,    84,    70,    70,   146,
     132,    69,   145,    37,    62,   116,   118,   124,    61,    88,
     132,    57,    62,    61,     9,   101,   122,   123,   132,    61,
     124,    61,    61,   145,   145,    73,    70,   135,    67,    65,
     117,    36,   119,   132,    62,    87,    54,    97,    62,    54,
     138,   139,    93,    54,    62,     8,    10,    12,    22,    26,
      42,    45,    47,    85,   101,   105,   107,   108,   110,   111,
     112,   113,    62,    62,    74,   147,   145,    37,   136,    62,
     118,   132,    15,   121,    63,    90,   147,    54,   147,    62,
      62,   106,   107,   114,   148,    40,   109,   114,   112,   135,
      67,   117,    54,   120,   130,   120,    19,    25,    30,    32,
      48,    55,    56,    57,    68,    91,   102,   103,   104,    62,
      63,    54,   115,     4,   136,    90,    25,   102,    66,   132,
      62,   116,    54,    67,    62,    62
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
      yyerror (&yylloc, webidl_ast, YY_("syntax error: cannot back up")); \
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
# define YYLEX yylex (&yylval, &yylloc, YYLEX_PARAM)
#else
# define YYLEX yylex (&yylval, &yylloc)
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
		  Type, Value, Location, webidl_ast); \
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
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, struct webidl_node **webidl_ast)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep, yylocationp, webidl_ast)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
    YYLTYPE const * const yylocationp;
    struct webidl_node **webidl_ast;
#endif
{
  if (!yyvaluep)
    return;
  YYUSE (yylocationp);
  YYUSE (webidl_ast);
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
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, struct webidl_node **webidl_ast)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep, yylocationp, webidl_ast)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
    YYLTYPE const * const yylocationp;
    struct webidl_node **webidl_ast;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  YY_LOCATION_PRINT (yyoutput, *yylocationp);
  YYFPRINTF (yyoutput, ": ");
  yy_symbol_value_print (yyoutput, yytype, yyvaluep, yylocationp, webidl_ast);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
#else
static void
yy_stack_print (yybottom, yytop)
    yytype_int16 *yybottom;
    yytype_int16 *yytop;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
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
yy_reduce_print (YYSTYPE *yyvsp, YYLTYPE *yylsp, int yyrule, struct webidl_node **webidl_ast)
#else
static void
yy_reduce_print (yyvsp, yylsp, yyrule, webidl_ast)
    YYSTYPE *yyvsp;
    YYLTYPE *yylsp;
    int yyrule;
    struct webidl_node **webidl_ast;
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
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       , &(yylsp[(yyi + 1) - (yynrhs)])		       , webidl_ast);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, yylsp, Rule, webidl_ast); \
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
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, YYLTYPE *yylocationp, struct webidl_node **webidl_ast)
#else
static void
yydestruct (yymsg, yytype, yyvaluep, yylocationp, webidl_ast)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
    YYLTYPE *yylocationp;
    struct webidl_node **webidl_ast;
#endif
{
  YYUSE (yyvaluep);
  YYUSE (yylocationp);
  YYUSE (webidl_ast);

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
int yyparse (struct webidl_node **webidl_ast);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */





/*-------------------------.
| yyparse or yypush_parse.  |
`-------------------------*/

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
yyparse (struct webidl_node **webidl_ast)
#else
int
yyparse (webidl_ast)
    struct webidl_node **webidl_ast;
#endif
#endif
{
/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Location data for the lookahead symbol.  */
YYLTYPE yylloc;

    /* Number of syntax errors so far.  */
    int yynerrs;

    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.
       `yyls': related to locations.

       Refer to the stacks thru separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    /* The location stack.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls;
    YYLTYPE *yylsp;

    /* The locations where the error started and ended.  */
    YYLTYPE yyerror_range[2];

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yytoken = 0;
  yyss = yyssa;
  yyvs = yyvsa;
  yyls = yylsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */
  yyssp = yyss;
  yyvsp = yyvs;
  yylsp = yyls;

#if YYLTYPE_IS_TRIVIAL
  /* Initialize the default location before parsing starts.  */
  yylloc.first_line   = yylloc.last_line   = 1;
  yylloc.first_column = yylloc.last_column = 1;
#endif

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
	YYLTYPE *yyls1 = yyls;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yyls1, yysize * sizeof (*yylsp),
		    &yystacksize);

	yyls = yyls1;
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
	YYSTACK_RELOCATE (yyss_alloc, yyss);
	YYSTACK_RELOCATE (yyvs_alloc, yyvs);
	YYSTACK_RELOCATE (yyls_alloc, yyls);
#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
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

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;
  *++yylsp = yylloc;
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

  /* Default location.  */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:

/* Line 1455 of yacc.c  */
#line 195 "src/webidl-parser.y"
    {
          (yyval.node) = NULL;
        ;}
    break;

  case 3:

/* Line 1455 of yacc.c  */
#line 200 "src/webidl-parser.y"
    {
            webidl_node_add((yyvsp[(3) - (3)].node), (yyvsp[(2) - (3)].node));
            (yyval.node) = *webidl_ast = webidl_node_prepend(*webidl_ast, (yyvsp[(3) - (3)].node));
        ;}
    break;

  case 4:

/* Line 1455 of yacc.c  */
#line 206 "src/webidl-parser.y"
    {
            fprintf(stderr, "%d: %s\n", yylloc.first_line, errtxt);
            free(errtxt);
            YYABORT ;
        ;}
    break;

  case 12:

/* Line 1455 of yacc.c  */
#line 234 "src/webidl-parser.y"
    {
            (yyval.node) = (yyvsp[(2) - (2)].node);
        ;}
    break;

  case 16:

/* Line 1455 of yacc.c  */
#line 251 "src/webidl-parser.y"
    {
            /* extend interface with additional members */
            struct webidl_node *interface_node;
            struct webidl_node *members = NULL;

            if ((yyvsp[(3) - (7)].text) != NULL) {
                members = webidl_node_new(WEBIDL_NODE_TYPE_INTERFACE_INHERITANCE, members, (yyvsp[(3) - (7)].text));
            }

            members = webidl_node_new(WEBIDL_NODE_TYPE_LIST, members, (yyvsp[(5) - (7)].node));


            interface_node = webidl_node_find_type_ident(*webidl_ast,
                                                     WEBIDL_NODE_TYPE_INTERFACE,
                                                     (yyvsp[(2) - (7)].text));

            if (interface_node == NULL) {
                /* no existing interface - create one with ident */
                members = webidl_node_new(WEBIDL_NODE_TYPE_IDENT, members, (yyvsp[(2) - (7)].text));

                (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_INTERFACE, NULL, members);
            } else {
                /* update the existing interface */

                /* link member node into interfaces_node */
                webidl_node_add(interface_node, members);

                (yyval.node) = NULL; /* updating so no need to add a new node */
            }
        ;}
    break;

  case 17:

/* Line 1455 of yacc.c  */
#line 286 "src/webidl-parser.y"
    {
            (yyval.node) = (yyvsp[(2) - (2)].node);
        ;}
    break;

  case 20:

/* Line 1455 of yacc.c  */
#line 301 "src/webidl-parser.y"
    {
            /* extend interface with additional members */
            struct webidl_node *members;
            struct webidl_node *interface_node;

            interface_node = webidl_node_find_type_ident(*webidl_ast,
                                                     WEBIDL_NODE_TYPE_INTERFACE,
                                                     (yyvsp[(2) - (6)].text));

            members = webidl_node_new(WEBIDL_NODE_TYPE_LIST, NULL, (yyvsp[(4) - (6)].node));

            if (interface_node == NULL) {
                /* doesnt already exist so create it */

                members = webidl_node_new(WEBIDL_NODE_TYPE_IDENT, members, (yyvsp[(2) - (6)].text));

                (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_INTERFACE, NULL, members);
            } else {
                /* update the existing interface */

                /* link member node into interfaces_node */
                webidl_node_add(interface_node, members);

                (yyval.node) = NULL; /* updating so no need to add a new node */
            }
        ;}
    break;

  case 21:

/* Line 1455 of yacc.c  */
#line 332 "src/webidl-parser.y"
    {
          (yyval.node) = NULL;
        ;}
    break;

  case 22:

/* Line 1455 of yacc.c  */
#line 344 "src/webidl-parser.y"
    {
            struct webidl_node *member_node;
            struct webidl_node *ident_node;
            struct webidl_node *list_node;

            ident_node = webidl_node_find_type(webidl_node_getnode((yyvsp[(3) - (3)].node)),
                                               NULL,
                                               WEBIDL_NODE_TYPE_IDENT);

            list_node = webidl_node_find_type(webidl_node_getnode((yyvsp[(3) - (3)].node)),
                                              NULL,
                                              WEBIDL_NODE_TYPE_LIST);

            if (ident_node == NULL) {
                /* something with no ident - possibly constructors? */
                /* @todo understand this abtter */

                (yyval.node) = webidl_node_prepend((yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node));

            } else if (list_node == NULL) {
                /* member with no argument list, usually an attribute, cannot
                 * be polymorphic
                 */

                /* add extended attributes to parameter list */
                webidl_node_add((yyvsp[(3) - (3)].node), (yyvsp[(2) - (3)].node));

                (yyval.node) = webidl_node_prepend((yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node));

            } else {
                /* add extended attributes to parameter list */
                webidl_node_add(list_node, (yyvsp[(2) - (3)].node));

                /* has an arguemnt list so can be polymorphic */
                member_node = webidl_node_find_type_ident((yyvsp[(1) - (3)].node),
                                             webidl_node_gettype((yyvsp[(3) - (3)].node)),
                                             webidl_node_gettext(ident_node));
                if (member_node == NULL) {
                    /* not a member with that ident already present */
                    (yyval.node) = webidl_node_prepend((yyvsp[(1) - (3)].node), (yyvsp[(3) - (3)].node));
                } else {
                    webidl_node_add(member_node, list_node);
                    (yyval.node) = (yyvsp[(1) - (3)].node); /* updated existing node do not add new one */
                }
            }
        ;}
    break;

  case 25:

/* Line 1455 of yacc.c  */
#line 402 "src/webidl-parser.y"
    {
            (yyval.node) = NULL;
        ;}
    break;

  case 29:

/* Line 1455 of yacc.c  */
#line 422 "src/webidl-parser.y"
    {
            (yyval.node) = NULL;
        ;}
    break;

  case 34:

/* Line 1455 of yacc.c  */
#line 444 "src/webidl-parser.y"
    {
            (yyval.node) = NULL;
        ;}
    break;

  case 37:

/* Line 1455 of yacc.c  */
#line 459 "src/webidl-parser.y"
    {
          (yyval.text) = NULL;
        ;}
    break;

  case 38:

/* Line 1455 of yacc.c  */
#line 464 "src/webidl-parser.y"
    {
          (yyval.text) = (yyvsp[(2) - (2)].text);
        ;}
    break;

  case 39:

/* Line 1455 of yacc.c  */
#line 472 "src/webidl-parser.y"
    {
            (yyval.node) = NULL;
        ;}
    break;

  case 43:

/* Line 1455 of yacc.c  */
#line 492 "src/webidl-parser.y"
    {
            (yyval.node) = NULL;
        ;}
    break;

  case 44:

/* Line 1455 of yacc.c  */
#line 500 "src/webidl-parser.y"
    {
            (yyval.node) = NULL;
        ;}
    break;

  case 45:

/* Line 1455 of yacc.c  */
#line 508 "src/webidl-parser.y"
    {
            /* extend interface with implements members */
            struct webidl_node *implements;
            struct webidl_node *interface_node;


            interface_node = webidl_node_find_type_ident(*webidl_ast,
                                                     WEBIDL_NODE_TYPE_INTERFACE,
                                                     (yyvsp[(1) - (4)].text));

            implements = webidl_node_new(WEBIDL_NODE_TYPE_INTERFACE_IMPLEMENTS, NULL, (yyvsp[(3) - (4)].text));

            if (interface_node == NULL) {
                /* interface doesnt already exist so create it */

                implements = webidl_node_new(WEBIDL_NODE_TYPE_IDENT, implements, (yyvsp[(1) - (4)].text));

                (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_INTERFACE, NULL, implements);
            } else {
                /* update the existing interface */

                /* link implements node into interfaces_node */
                webidl_node_add(interface_node, implements);

                (yyval.node) = NULL; /* updating so no need to add a new node */
            }
        ;}
    break;

  case 46:

/* Line 1455 of yacc.c  */
#line 540 "src/webidl-parser.y"
    {
            struct webidl_node *constant;

            constant = webidl_node_new(WEBIDL_NODE_TYPE_IDENT, NULL, (yyvsp[(3) - (6)].text));

            /* add constant type */
            constant = webidl_node_prepend(constant, (yyvsp[(2) - (6)].node));

            /* add constant value */
            constant = webidl_node_prepend(constant, (yyvsp[(5) - (6)].node));

            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_CONST, NULL, constant);
        ;}
    break;

  case 49:

/* Line 1455 of yacc.c  */
#line 562 "src/webidl-parser.y"
    {
          (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_LITERAL_INT, NULL, (void *)(yyvsp[(1) - (1)].value));
        ;}
    break;

  case 50:

/* Line 1455 of yacc.c  */
#line 567 "src/webidl-parser.y"
    {
          (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_LITERAL_NULL, NULL, NULL);
        ;}
    break;

  case 51:

/* Line 1455 of yacc.c  */
#line 575 "src/webidl-parser.y"
    {
          (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_LITERAL_BOOL, NULL, (void *)true);
        ;}
    break;

  case 52:

/* Line 1455 of yacc.c  */
#line 580 "src/webidl-parser.y"
    {
          (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_LITERAL_BOOL, NULL, (void *)false);
        ;}
    break;

  case 53:

/* Line 1455 of yacc.c  */
#line 588 "src/webidl-parser.y"
    {
          float *value;
          value = malloc(sizeof(float));
          *value = strtof((yyvsp[(1) - (1)].text), NULL);
          (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_LITERAL_FLOAT, NULL, value);
        ;}
    break;

  case 54:

/* Line 1455 of yacc.c  */
#line 596 "src/webidl-parser.y"
    {
          float *value;
          value = malloc(sizeof(float));
          *value = -INFINITY;
          (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_LITERAL_FLOAT, NULL, value);
        ;}
    break;

  case 55:

/* Line 1455 of yacc.c  */
#line 604 "src/webidl-parser.y"
    {
          float *value;
          value = malloc(sizeof(float));
          *value = INFINITY;
          (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_LITERAL_FLOAT, NULL, value);
        ;}
    break;

  case 56:

/* Line 1455 of yacc.c  */
#line 612 "src/webidl-parser.y"
    {
          float *value;
          value = malloc(sizeof(float));
          *value = NAN;
          (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_LITERAL_FLOAT, NULL, value);
        ;}
    break;

  case 57:

/* Line 1455 of yacc.c  */
#line 623 "src/webidl-parser.y"
    {
            (yyval.node) = (yyvsp[(2) - (2)].node);
        ;}
    break;

  case 61:

/* Line 1455 of yacc.c  */
#line 637 "src/webidl-parser.y"
    {
            /* @todo deal with stringifier */
            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_OPERATION, NULL, (yyvsp[(1) - (1)].node));
        ;}
    break;

  case 62:

/* Line 1455 of yacc.c  */
#line 643 "src/webidl-parser.y"
    {
          (yyval.node)=NULL;
        ;}
    break;

  case 63:

/* Line 1455 of yacc.c  */
#line 651 "src/webidl-parser.y"
    {
            struct webidl_node *attribute;

            attribute = webidl_node_new(WEBIDL_NODE_TYPE_IDENT, NULL, (yyvsp[(5) - (6)].text));

            /* add attributes type */
            attribute = webidl_node_prepend(attribute, (yyvsp[(4) - (6)].node));

            /* deal with readonly modifier */
            if ((yyvsp[(2) - (6)].isit)) {
                attribute = webidl_node_new(WEBIDL_NODE_TYPE_MODIFIER, attribute, (void *)WEBIDL_TYPE_READONLY);
            }

            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_ATTRIBUTE, NULL, attribute);

        ;}
    break;

  case 64:

/* Line 1455 of yacc.c  */
#line 672 "src/webidl-parser.y"
    {
            (yyval.isit) = false;
        ;}
    break;

  case 65:

/* Line 1455 of yacc.c  */
#line 677 "src/webidl-parser.y"
    {
            (yyval.isit) = true;
        ;}
    break;

  case 66:

/* Line 1455 of yacc.c  */
#line 685 "src/webidl-parser.y"
    {
            (yyval.isit) = false;
        ;}
    break;

  case 67:

/* Line 1455 of yacc.c  */
#line 690 "src/webidl-parser.y"
    {
            (yyval.isit) = true;
        ;}
    break;

  case 68:

/* Line 1455 of yacc.c  */
#line 698 "src/webidl-parser.y"
    {
            /* @todo fix qualifiers */
            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_OPERATION, NULL, (yyvsp[(2) - (2)].node));
        ;}
    break;

  case 78:

/* Line 1455 of yacc.c  */
#line 734 "src/webidl-parser.y"
    {
            struct webidl_node *arglist;

            /* put return type in argument list */
            arglist = webidl_node_prepend((yyvsp[(4) - (6)].node), (yyvsp[(1) - (6)].node));

            /* argument list */
            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_LIST, NULL, arglist);

            (yyval.node) = webidl_node_prepend((yyval.node), (yyvsp[(2) - (6)].node)); /* identifier */
        ;}
    break;

  case 79:

/* Line 1455 of yacc.c  */
#line 750 "src/webidl-parser.y"
    {
            (yyval.node) = NULL;
        ;}
    break;

  case 80:

/* Line 1455 of yacc.c  */
#line 755 "src/webidl-parser.y"
    {
            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_IDENT, NULL, (yyvsp[(1) - (1)].text));
        ;}
    break;

  case 81:

/* Line 1455 of yacc.c  */
#line 766 "src/webidl-parser.y"
    {
            (yyval.node) = NULL;
        ;}
    break;

  case 82:

/* Line 1455 of yacc.c  */
#line 771 "src/webidl-parser.y"
    {
            (yyval.node) = webidl_node_append((yyvsp[(2) - (2)].node), (yyvsp[(1) - (2)].node));
        ;}
    break;

  case 83:

/* Line 1455 of yacc.c  */
#line 779 "src/webidl-parser.y"
    {
            (yyval.node) = NULL;
        ;}
    break;

  case 84:

/* Line 1455 of yacc.c  */
#line 784 "src/webidl-parser.y"
    {
            (yyval.node) = webidl_node_append((yyvsp[(3) - (3)].node), (yyvsp[(2) - (3)].node));
        ;}
    break;

  case 85:

/* Line 1455 of yacc.c  */
#line 793 "src/webidl-parser.y"
    {
            (yyval.node) = (yyvsp[(2) - (2)].node);
        ;}
    break;

  case 86:

/* Line 1455 of yacc.c  */
#line 801 "src/webidl-parser.y"
    {
            struct webidl_node *argument;
            argument = webidl_node_new(WEBIDL_NODE_TYPE_IDENT, NULL, (yyvsp[(3) - (4)].text));
            argument = webidl_node_prepend(argument, (yyvsp[(2) - (4)].node)); /* add type node */
            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_OPTIONAL_ARGUMENT, NULL, argument);
        ;}
    break;

  case 87:

/* Line 1455 of yacc.c  */
#line 809 "src/webidl-parser.y"
    {
            struct webidl_node *argument;
            argument = webidl_node_new(WEBIDL_NODE_TYPE_IDENT, NULL, (yyvsp[(3) - (3)].text));
            argument = webidl_node_prepend(argument, (yyvsp[(2) - (3)].node)); /* ellipsis node */
            argument = webidl_node_prepend(argument, (yyvsp[(1) - (3)].node)); /* add type node */
            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_ARGUMENT, NULL, argument);
        ;}
    break;

  case 90:

/* Line 1455 of yacc.c  */
#line 828 "src/webidl-parser.y"
    {
            (yyval.node) = NULL;
        ;}
    break;

  case 91:

/* Line 1455 of yacc.c  */
#line 833 "src/webidl-parser.y"
    {
            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_ELLIPSIS, NULL, NULL);
        ;}
    break;

  case 95:

/* Line 1455 of yacc.c  */
#line 853 "src/webidl-parser.y"
    {
            (yyval.node) = NULL;
        ;}
    break;

  case 96:

/* Line 1455 of yacc.c  */
#line 858 "src/webidl-parser.y"
    {
            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_EXTENDED_ATTRIBUTE, (yyvsp[(3) - (4)].node), (yyvsp[(2) - (4)].node));
        ;}
    break;

  case 97:

/* Line 1455 of yacc.c  */
#line 866 "src/webidl-parser.y"
    {
            (yyval.node) = NULL;
        ;}
    break;

  case 98:

/* Line 1455 of yacc.c  */
#line 871 "src/webidl-parser.y"
    {
            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_EXTENDED_ATTRIBUTE, (yyvsp[(3) - (3)].node), (yyvsp[(2) - (3)].node));
        ;}
    break;

  case 99:

/* Line 1455 of yacc.c  */
#line 881 "src/webidl-parser.y"
    {
            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_EXTENDED_ATTRIBUTE, (yyvsp[(4) - (4)].node), (yyvsp[(2) - (4)].node));
        ;}
    break;

  case 100:

/* Line 1455 of yacc.c  */
#line 886 "src/webidl-parser.y"
    {
            /* @todo should be a WEBIDL_NODE_TYPE_EXTENDED_ATTRIBUTE_SQUARE */
            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_EXTENDED_ATTRIBUTE, (yyvsp[(4) - (4)].node), (yyvsp[(2) - (4)].node));
        ;}
    break;

  case 101:

/* Line 1455 of yacc.c  */
#line 892 "src/webidl-parser.y"
    {
            /* @todo should be a WEBIDL_NODE_TYPE_EXTENDED_ATTRIBUTE_CURLY */
            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_EXTENDED_ATTRIBUTE, (yyvsp[(4) - (4)].node), (yyvsp[(2) - (4)].node));
        ;}
    break;

  case 102:

/* Line 1455 of yacc.c  */
#line 898 "src/webidl-parser.y"
    {
            (yyval.node) = webidl_node_append((yyvsp[(2) - (2)].node),
                             webidl_node_new(WEBIDL_NODE_TYPE_IDENT, NULL, (yyvsp[(1) - (2)].text)));
        ;}
    break;

  case 103:

/* Line 1455 of yacc.c  */
#line 907 "src/webidl-parser.y"
    {
            (yyval.node) = NULL;
        ;}
    break;

  case 104:

/* Line 1455 of yacc.c  */
#line 912 "src/webidl-parser.y"
    {
            (yyval.node) = (yyvsp[(1) - (1)].node);
        ;}
    break;

  case 105:

/* Line 1455 of yacc.c  */
#line 920 "src/webidl-parser.y"
    {
            (yyval.node) = NULL;
        ;}
    break;

  case 106:

/* Line 1455 of yacc.c  */
#line 925 "src/webidl-parser.y"
    {
            (yyval.node) = webidl_node_prepend(
                     webidl_node_new(WEBIDL_NODE_TYPE_EXTENDED_ATTRIBUTE,
                                     NULL,
                                     (yyvsp[(2) - (4)].node)),
                     (yyvsp[(4) - (4)].node));
        ;}
    break;

  case 107:

/* Line 1455 of yacc.c  */
#line 934 "src/webidl-parser.y"
    {
            /* @todo should be a WEBIDL_NODE_TYPE_EXTENDED_ATTRIBUTE_SQUARE */
            (yyval.node) = webidl_node_prepend(
                     webidl_node_new(WEBIDL_NODE_TYPE_EXTENDED_ATTRIBUTE,
                                     NULL,
                                     (yyvsp[(2) - (4)].node)),
                     (yyvsp[(4) - (4)].node));
        ;}
    break;

  case 108:

/* Line 1455 of yacc.c  */
#line 944 "src/webidl-parser.y"
    {
            /* @todo should be a WEBIDL_NODE_TYPE_EXTENDED_ATTRIBUTE_CURLY */
            (yyval.node) = webidl_node_prepend(
                     webidl_node_new(WEBIDL_NODE_TYPE_EXTENDED_ATTRIBUTE,
                                     NULL,
                                     (yyvsp[(2) - (4)].node)),
                     (yyvsp[(4) - (4)].node));
        ;}
    break;

  case 109:

/* Line 1455 of yacc.c  */
#line 954 "src/webidl-parser.y"
    {
            (yyval.node) = webidl_node_append((yyvsp[(2) - (2)].node),
                             webidl_node_new(WEBIDL_NODE_TYPE_IDENT, NULL, (yyvsp[(1) - (2)].text)));
        ;}
    break;

  case 110:

/* Line 1455 of yacc.c  */
#line 963 "src/webidl-parser.y"
    {
            /* @todo loosing base info here might break the attribute */
            (yyval.text) = calloc(1, 32);
            snprintf((yyval.text), 32, "%ld", (yyvsp[(1) - (1)].value));
        ;}
    break;

  case 111:

/* Line 1455 of yacc.c  */
#line 970 "src/webidl-parser.y"
    {
            (yyval.text) = (yyvsp[(1) - (1)].text);
        ;}
    break;

  case 112:

/* Line 1455 of yacc.c  */
#line 975 "src/webidl-parser.y"
    {
            (yyval.text) = (yyvsp[(1) - (1)].text);
        ;}
    break;

  case 113:

/* Line 1455 of yacc.c  */
#line 980 "src/webidl-parser.y"
    {
            (yyval.text) = (yyvsp[(1) - (1)].text);
        ;}
    break;

  case 114:

/* Line 1455 of yacc.c  */
#line 985 "src/webidl-parser.y"
    {
            (yyval.text) = (yyvsp[(1) - (1)].text);
        ;}
    break;

  case 115:

/* Line 1455 of yacc.c  */
#line 990 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("-");
        ;}
    break;

  case 116:

/* Line 1455 of yacc.c  */
#line 995 "src/webidl-parser.y"
    {
            (yyval.text) = strdup(".");
        ;}
    break;

  case 117:

/* Line 1455 of yacc.c  */
#line 1000 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("...");
        ;}
    break;

  case 118:

/* Line 1455 of yacc.c  */
#line 1005 "src/webidl-parser.y"
    {
            (yyval.text) = strdup(":");
        ;}
    break;

  case 119:

/* Line 1455 of yacc.c  */
#line 1010 "src/webidl-parser.y"
    {
            (yyval.text) = strdup(";");
        ;}
    break;

  case 120:

/* Line 1455 of yacc.c  */
#line 1015 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("<");
        ;}
    break;

  case 121:

/* Line 1455 of yacc.c  */
#line 1020 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("=");
        ;}
    break;

  case 122:

/* Line 1455 of yacc.c  */
#line 1025 "src/webidl-parser.y"
    {
            (yyval.text) = strdup(">");
        ;}
    break;

  case 123:

/* Line 1455 of yacc.c  */
#line 1030 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("?");
        ;}
    break;

  case 124:

/* Line 1455 of yacc.c  */
#line 1035 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("Date");
        ;}
    break;

  case 125:

/* Line 1455 of yacc.c  */
#line 1040 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("DOMString");
        ;}
    break;

  case 126:

/* Line 1455 of yacc.c  */
#line 1045 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("Infinity");
        ;}
    break;

  case 127:

/* Line 1455 of yacc.c  */
#line 1050 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("NaN");
        ;}
    break;

  case 128:

/* Line 1455 of yacc.c  */
#line 1055 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("any");
        ;}
    break;

  case 129:

/* Line 1455 of yacc.c  */
#line 1060 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("boolean");
        ;}
    break;

  case 130:

/* Line 1455 of yacc.c  */
#line 1065 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("byte");
        ;}
    break;

  case 131:

/* Line 1455 of yacc.c  */
#line 1070 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("double");
        ;}
    break;

  case 132:

/* Line 1455 of yacc.c  */
#line 1075 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("false");
        ;}
    break;

  case 133:

/* Line 1455 of yacc.c  */
#line 1080 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("float");
        ;}
    break;

  case 134:

/* Line 1455 of yacc.c  */
#line 1085 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("long");
        ;}
    break;

  case 135:

/* Line 1455 of yacc.c  */
#line 1090 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("null");
        ;}
    break;

  case 136:

/* Line 1455 of yacc.c  */
#line 1095 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("object");
        ;}
    break;

  case 137:

/* Line 1455 of yacc.c  */
#line 1100 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("octet");
        ;}
    break;

  case 138:

/* Line 1455 of yacc.c  */
#line 1105 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("or");
        ;}
    break;

  case 139:

/* Line 1455 of yacc.c  */
#line 1110 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("optional");
        ;}
    break;

  case 140:

/* Line 1455 of yacc.c  */
#line 1115 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("sequence");
        ;}
    break;

  case 141:

/* Line 1455 of yacc.c  */
#line 1120 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("short");
        ;}
    break;

  case 142:

/* Line 1455 of yacc.c  */
#line 1125 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("true");
        ;}
    break;

  case 143:

/* Line 1455 of yacc.c  */
#line 1130 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("unsigned");
        ;}
    break;

  case 144:

/* Line 1455 of yacc.c  */
#line 1135 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("void");
        ;}
    break;

  case 145:

/* Line 1455 of yacc.c  */
#line 1140 "src/webidl-parser.y"
    {
            (yyval.text) = (yyvsp[(1) - (1)].text);
        ;}
    break;

  case 146:

/* Line 1455 of yacc.c  */
#line 1148 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("attribute");
        ;}
    break;

  case 147:

/* Line 1455 of yacc.c  */
#line 1153 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("callback");
        ;}
    break;

  case 148:

/* Line 1455 of yacc.c  */
#line 1158 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("const");
        ;}
    break;

  case 149:

/* Line 1455 of yacc.c  */
#line 1163 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("creator");
        ;}
    break;

  case 150:

/* Line 1455 of yacc.c  */
#line 1168 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("deleter");
        ;}
    break;

  case 151:

/* Line 1455 of yacc.c  */
#line 1173 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("dictionary");
        ;}
    break;

  case 152:

/* Line 1455 of yacc.c  */
#line 1178 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("enum");
        ;}
    break;

  case 153:

/* Line 1455 of yacc.c  */
#line 1183 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("exception");
        ;}
    break;

  case 154:

/* Line 1455 of yacc.c  */
#line 1188 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("getter");
        ;}
    break;

  case 155:

/* Line 1455 of yacc.c  */
#line 1193 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("implements");
        ;}
    break;

  case 156:

/* Line 1455 of yacc.c  */
#line 1198 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("inherit");
        ;}
    break;

  case 157:

/* Line 1455 of yacc.c  */
#line 1203 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("interface");
        ;}
    break;

  case 158:

/* Line 1455 of yacc.c  */
#line 1208 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("legacycaller");
        ;}
    break;

  case 159:

/* Line 1455 of yacc.c  */
#line 1213 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("partial");
        ;}
    break;

  case 160:

/* Line 1455 of yacc.c  */
#line 1218 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("setter");
        ;}
    break;

  case 161:

/* Line 1455 of yacc.c  */
#line 1223 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("static");
        ;}
    break;

  case 162:

/* Line 1455 of yacc.c  */
#line 1228 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("stringifier");
        ;}
    break;

  case 163:

/* Line 1455 of yacc.c  */
#line 1233 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("typedef");
        ;}
    break;

  case 164:

/* Line 1455 of yacc.c  */
#line 1238 "src/webidl-parser.y"
    {
            (yyval.text) = strdup("unrestricted");
        ;}
    break;

  case 165:

/* Line 1455 of yacc.c  */
#line 1246 "src/webidl-parser.y"
    {
            (yyval.text) = (yyvsp[(1) - (1)].text);
        ;}
    break;

  case 166:

/* Line 1455 of yacc.c  */
#line 1251 "src/webidl-parser.y"
    {
            (yyval.text) = strdup(",");
        ;}
    break;

  case 167:

/* Line 1455 of yacc.c  */
#line 1259 "src/webidl-parser.y"
    {
            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_TYPE, NULL, (yyvsp[(1) - (1)].node));
        ;}
    break;

  case 168:

/* Line 1455 of yacc.c  */
#line 1264 "src/webidl-parser.y"
    {
            /* todo handle suffix */
            (yyval.node) = (yyvsp[(1) - (2)].node);
        ;}
    break;

  case 170:

/* Line 1455 of yacc.c  */
#line 1275 "src/webidl-parser.y"
    {
            (yyval.node) = NULL; /* todo implement */
        ;}
    break;

  case 171:

/* Line 1455 of yacc.c  */
#line 1283 "src/webidl-parser.y"
    {
            (yyval.node) = NULL;
        ;}
    break;

  case 177:

/* Line 1455 of yacc.c  */
#line 1307 "src/webidl-parser.y"
    {
            (yyval.node) = webidl_node_prepend((yyvsp[(1) - (2)].node), (yyvsp[(2) - (2)].node));
        ;}
    break;

  case 178:

/* Line 1455 of yacc.c  */
#line 1312 "src/webidl-parser.y"
    {
            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_TYPE_BASE, (yyvsp[(2) - (2)].node), (void *)WEBIDL_TYPE_STRING);
        ;}
    break;

  case 179:

/* Line 1455 of yacc.c  */
#line 1317 "src/webidl-parser.y"
    {
            struct webidl_node *type;
            type = webidl_node_new(WEBIDL_NODE_TYPE_TYPE_BASE, (yyvsp[(2) - (2)].node), (void *)WEBIDL_TYPE_USER);
            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_IDENT, type, (yyvsp[(1) - (2)].text));
        ;}
    break;

  case 180:

/* Line 1455 of yacc.c  */
#line 1324 "src/webidl-parser.y"
    {
            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_TYPE_BASE, (yyvsp[(3) - (5)].node), (void *)WEBIDL_TYPE_SEQUENCE);
        ;}
    break;

  case 181:

/* Line 1455 of yacc.c  */
#line 1329 "src/webidl-parser.y"
    {
            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_TYPE_BASE, (yyvsp[(2) - (2)].node), (void *)WEBIDL_TYPE_OBJECT);
        ;}
    break;

  case 182:

/* Line 1455 of yacc.c  */
#line 1334 "src/webidl-parser.y"
    {
            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_TYPE_BASE, (yyvsp[(2) - (2)].node), (void *)WEBIDL_TYPE_DATE);
        ;}
    break;

  case 183:

/* Line 1455 of yacc.c  */
#line 1342 "src/webidl-parser.y"
    {
            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_TYPE, NULL, (yyvsp[(1) - (2)].node));
        ;}
    break;

  case 184:

/* Line 1455 of yacc.c  */
#line 1347 "src/webidl-parser.y"
    {
            struct webidl_node *type;
            type = webidl_node_new(WEBIDL_NODE_TYPE_TYPE_BASE, NULL, (void *)WEBIDL_TYPE_USER);
            type = webidl_node_new(WEBIDL_NODE_TYPE_IDENT, type, (yyvsp[(1) - (2)].text));
            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_TYPE, NULL, type);
        ;}
    break;

  case 187:

/* Line 1455 of yacc.c  */
#line 1363 "src/webidl-parser.y"
    {
            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_TYPE_BASE, NULL, (void *)WEBIDL_TYPE_BOOL);
        ;}
    break;

  case 188:

/* Line 1455 of yacc.c  */
#line 1368 "src/webidl-parser.y"
    {
            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_TYPE_BASE, NULL, (void *)WEBIDL_TYPE_BYTE);
        ;}
    break;

  case 189:

/* Line 1455 of yacc.c  */
#line 1373 "src/webidl-parser.y"
    {
            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_TYPE_BASE, NULL, (void *)WEBIDL_TYPE_OCTET);
        ;}
    break;

  case 190:

/* Line 1455 of yacc.c  */
#line 1381 "src/webidl-parser.y"
    {
            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_MODIFIER,
                                 (yyvsp[(2) - (2)].node),
                                 (void *)WEBIDL_TYPE_MODIFIER_UNRESTRICTED);
        ;}
    break;

  case 192:

/* Line 1455 of yacc.c  */
#line 1393 "src/webidl-parser.y"
    {
            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_TYPE_BASE, NULL, (void *)WEBIDL_TYPE_FLOAT);
        ;}
    break;

  case 193:

/* Line 1455 of yacc.c  */
#line 1398 "src/webidl-parser.y"
    {
            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_TYPE_BASE, NULL, (void *)WEBIDL_TYPE_DOUBLE);
        ;}
    break;

  case 194:

/* Line 1455 of yacc.c  */
#line 1406 "src/webidl-parser.y"
    {
            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_MODIFIER,
                                 (yyvsp[(2) - (2)].node),
                                 (void *)WEBIDL_TYPE_MODIFIER_UNSIGNED);
        ;}
    break;

  case 196:

/* Line 1455 of yacc.c  */
#line 1418 "src/webidl-parser.y"
    {
            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_TYPE_BASE, NULL, (void *)WEBIDL_TYPE_SHORT);
        ;}
    break;

  case 197:

/* Line 1455 of yacc.c  */
#line 1423 "src/webidl-parser.y"
    {
            if ((yyvsp[(2) - (2)].isit)) {
                (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_TYPE_BASE, NULL, (void *)WEBIDL_TYPE_LONGLONG);
            } else {
                (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_TYPE_BASE, NULL, (void *)WEBIDL_TYPE_LONG);
            }
        ;}
    break;

  case 198:

/* Line 1455 of yacc.c  */
#line 1435 "src/webidl-parser.y"
    {
            (yyval.isit) = false;
        ;}
    break;

  case 199:

/* Line 1455 of yacc.c  */
#line 1440 "src/webidl-parser.y"
    {
            (yyval.isit) = true;
        ;}
    break;

  case 200:

/* Line 1455 of yacc.c  */
#line 1448 "src/webidl-parser.y"
    {
            (yyval.node) = NULL;
        ;}
    break;

  case 201:

/* Line 1455 of yacc.c  */
#line 1453 "src/webidl-parser.y"
    {
            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_TYPE_ARRAY, (yyvsp[(3) - (3)].node), NULL);
        ;}
    break;

  case 202:

/* Line 1455 of yacc.c  */
#line 1458 "src/webidl-parser.y"
    {
            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_TYPE_NULLABLE, (yyvsp[(2) - (2)].node), NULL);
        ;}
    break;

  case 203:

/* Line 1455 of yacc.c  */
#line 1466 "src/webidl-parser.y"
    {
            (yyval.node) = NULL;
        ;}
    break;

  case 204:

/* Line 1455 of yacc.c  */
#line 1471 "src/webidl-parser.y"
    {
            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_TYPE_ARRAY, (yyvsp[(3) - (3)].node), NULL);
        ;}
    break;

  case 208:

/* Line 1455 of yacc.c  */
#line 1488 "src/webidl-parser.y"
    {
            struct webidl_node *type;
            type = webidl_node_new(WEBIDL_NODE_TYPE_TYPE_BASE, NULL, (void *)WEBIDL_TYPE_VOID);
            (yyval.node) = webidl_node_new(WEBIDL_NODE_TYPE_TYPE, NULL, type);
        ;}
    break;



/* Line 1455 of yacc.c  */
#line 3440 "build-arm-apple-darwin9-arm-apple-darwin9-release-binary/webidl-parser.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;
  *++yylsp = yyloc;

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
      yyerror (&yylloc, webidl_ast, YY_("syntax error"));
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
	    yyerror (&yylloc, webidl_ast, yymsg);
	  }
	else
	  {
	    yyerror (&yylloc, webidl_ast, YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }

  yyerror_range[0] = yylloc;

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
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
		      yytoken, &yylval, &yylloc, webidl_ast);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse lookahead token after shifting the error
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

  yyerror_range[0] = yylsp[1-yylen];
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

      yyerror_range[0] = *yylsp;
      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp, yylsp, webidl_ast);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  *++yyvsp = yylval;

  yyerror_range[1] = yylloc;
  /* Using YYLLOC is tempting, but would change the location of
     the lookahead.  YYLOC is available though.  */
  YYLLOC_DEFAULT (yyloc, (yyerror_range - 1), 2);
  *++yylsp = yyloc;

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

#if !defined(yyoverflow) || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (&yylloc, webidl_ast, YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval, &yylloc, webidl_ast);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp, yylsp, webidl_ast);
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



/* Line 1675 of yacc.c  */
#line 1496 "src/webidl-parser.y"


