
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
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Using locations.  */
#define YYLSP_NEEDED 0

/* Substitute the variable and function names.  */
#define yyparse         _asn1_yyparse
#define yylex           _asn1_yylex
#define yyerror         _asn1_yyerror
#define yylval          _asn1_yylval
#define yychar          _asn1_yychar
#define yydebug         _asn1_yydebug
#define yynerrs         _asn1_yynerrs


/* Copy the first part of user declarations.  */

/* Line 189 of yacc.c  */
#line 1 "ASN1.y"

/*
 * Copyright (C) 2001, 2002, 2004, 2005, 2006, 2008, 2009, 2010, 2011
 * Free Software Foundation, Inc.
 *
 * This file is part of LIBTASN1.
 *
 * The LIBTASN1 library is free software; you can redistribute it
 * and/or modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301, USA
 */

/*****************************************************/
/* File: x509_ASN.y                                  */
/* Description: input file for 'bison' program.      */
/*   The output file is a parser (in C language) for */
/*   ASN.1 syntax                                    */
/*****************************************************/

#include <int.h>
#include <parser_aux.h>
#include <structure.h>

static FILE *file_asn1;			/* Pointer to file to parse */
static asn1_retCode result_parse;	/* result of the parser
					   algorithm */
static ASN1_TYPE p_tree;		/* pointer to the root of the
					   structure created by the
					   parser*/
static unsigned long lineNumber;	/* line number describing the
					   parser position inside the
					   file */
static char lastToken[ASN1_MAX_NAME_SIZE+1];	/* last token find in the file
					   to parse before the 'parse
					   error' */
extern char _asn1_identifierMissing[];
static const char *fileName;		/* file to parse */

static int _asn1_yyerror (const char *);
static int _asn1_yylex(void);



/* Line 189 of yacc.c  */
#line 136 "ASN1.c"

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


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     ASSIG = 258,
     NUM = 259,
     IDENTIFIER = 260,
     OPTIONAL = 261,
     INTEGER = 262,
     SIZE = 263,
     OCTET = 264,
     STRING = 265,
     SEQUENCE = 266,
     BIT = 267,
     UNIVERSAL = 268,
     PRIVATE = 269,
     APPLICATION = 270,
     DEFAULT = 271,
     CHOICE = 272,
     OF = 273,
     OBJECT = 274,
     STR_IDENTIFIER = 275,
     BOOLEAN = 276,
     ASN1_TRUE = 277,
     ASN1_FALSE = 278,
     TOKEN_NULL = 279,
     ANY = 280,
     DEFINED = 281,
     BY = 282,
     SET = 283,
     EXPLICIT = 284,
     IMPLICIT = 285,
     DEFINITIONS = 286,
     TAGS = 287,
     BEGIN = 288,
     END = 289,
     UTCTime = 290,
     GeneralizedTime = 291,
     GeneralString = 292,
     FROM = 293,
     IMPORTS = 294,
     ENUMERATED = 295
   };
#endif
/* Tokens.  */
#define ASSIG 258
#define NUM 259
#define IDENTIFIER 260
#define OPTIONAL 261
#define INTEGER 262
#define SIZE 263
#define OCTET 264
#define STRING 265
#define SEQUENCE 266
#define BIT 267
#define UNIVERSAL 268
#define PRIVATE 269
#define APPLICATION 270
#define DEFAULT 271
#define CHOICE 272
#define OF 273
#define OBJECT 274
#define STR_IDENTIFIER 275
#define BOOLEAN 276
#define ASN1_TRUE 277
#define ASN1_FALSE 278
#define TOKEN_NULL 279
#define ANY 280
#define DEFINED 281
#define BY 282
#define SET 283
#define EXPLICIT 284
#define IMPLICIT 285
#define DEFINITIONS 286
#define TAGS 287
#define BEGIN 288
#define END 289
#define UTCTime 290
#define GeneralizedTime 291
#define GeneralString 292
#define FROM 293
#define IMPORTS 294
#define ENUMERATED 295




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 214 of yacc.c  */
#line 58 "ASN1.y"

  unsigned int constant;
  char str[ASN1_MAX_NAME_SIZE+1];
  ASN1_TYPE node;



/* Line 214 of yacc.c  */
#line 260 "ASN1.c"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */


/* Line 264 of yacc.c  */
#line 272 "ASN1.c"

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
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
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
#define YYFINAL  5
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   192

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  52
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  43
/* YYNRULES -- Number of rules.  */
#define YYNRULES  99
/* YYNRULES -- Number of states.  */
#define YYNSTATES  190

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   295

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      43,    44,     2,    41,    45,    42,    51,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    46,     2,    47,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    49,    48,    50,     2,     2,     2,     2,
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
      35,    36,    37,    38,    39,    40
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,    12,    14,    17,    20,    22,    24,    26,
      28,    30,    32,    36,    41,    43,    47,    49,    54,    56,
      59,    61,    63,    65,    69,    74,    76,    79,    82,    85,
      88,    91,    93,    97,    99,   104,   109,   117,   119,   121,
     123,   128,   136,   138,   142,   144,   147,   150,   154,   159,
     161,   165,   168,   172,   178,   183,   186,   188,   191,   193,
     195,   197,   199,   201,   203,   205,   207,   209,   211,   213,
     215,   217,   219,   222,   224,   227,   230,   233,   235,   239,
     244,   248,   253,   258,   262,   267,   272,   274,   279,   283,
     291,   298,   303,   305,   307,   309,   312,   317,   321,   323
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      53,     0,    -1,    93,    31,    94,    32,     3,    33,    92,
      34,    -1,     4,    -1,    41,     4,    -1,    42,     4,    -1,
      54,    -1,    55,    -1,     4,    -1,     5,    -1,    56,    -1,
       5,    -1,    43,    56,    44,    -1,     5,    43,    56,    44,
      -1,    59,    -1,    60,    45,    59,    -1,    57,    -1,     5,
      43,     4,    44,    -1,    61,    -1,    62,    61,    -1,    13,
      -1,    14,    -1,    15,    -1,    46,     4,    47,    -1,    46,
      63,     4,    47,    -1,    64,    -1,    64,    29,    -1,    64,
      30,    -1,    16,    58,    -1,    16,    22,    -1,    16,    23,
      -1,    56,    -1,    67,    48,    56,    -1,     7,    -1,     7,
      49,    60,    50,    -1,    68,    43,    67,    44,    -1,    68,
      43,    57,    51,    51,    57,    44,    -1,    21,    -1,    35,
      -1,    36,    -1,     8,    43,    57,    44,    -1,     8,    43,
      57,    51,    51,    57,    44,    -1,    71,    -1,    43,    71,
      44,    -1,    37,    -1,    37,    72,    -1,     9,    10,    -1,
       9,    10,    72,    -1,     5,    43,     4,    44,    -1,    75,
      -1,    76,    45,    75,    -1,    12,    10,    -1,    12,    10,
      72,    -1,    12,    10,    49,    76,    50,    -1,    40,    49,
      76,    50,    -1,    19,    20,    -1,     5,    -1,     5,    72,
      -1,    68,    -1,    78,    -1,    69,    -1,    70,    -1,    74,
      -1,    77,    -1,    73,    -1,    85,    -1,    79,    -1,    87,
      -1,    88,    -1,    86,    -1,    24,    -1,    80,    -1,    65,
      80,    -1,    81,    -1,    81,    66,    -1,    81,     6,    -1,
       5,    82,    -1,    83,    -1,    84,    45,    83,    -1,    11,
      49,    84,    50,    -1,    11,    18,    80,    -1,    11,    72,
      18,    80,    -1,    28,    49,    84,    50,    -1,    28,    18,
      80,    -1,    28,    72,    18,    80,    -1,    17,    49,    84,
      50,    -1,    25,    -1,    25,    26,    27,     5,    -1,     5,
       3,    81,    -1,     5,    19,    20,     3,    49,    62,    50,
      -1,     5,     5,     3,    49,    62,    50,    -1,     5,     7,
       3,    56,    -1,    89,    -1,    90,    -1,    91,    -1,    92,
      91,    -1,     5,    49,    62,    50,    -1,     5,    49,    50,
      -1,    29,    -1,    30,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   119,   119,   132,   133,   136,   140,   141,   144,   145,
     148,   149,   152,   154,   159,   160,   164,   166,   171,   172,
     176,   177,   178,   181,   183,   187,   188,   189,   192,   194,
     195,   199,   200,   204,   205,   207,   208,   215,   218,   219,
     222,   224,   230,   231,   234,   235,   239,   240,   244,   249,
     250,   254,   255,   256,   261,   267,   270,   272,   275,   276,
     277,   278,   279,   280,   281,   282,   283,   284,   285,   286,
     287,   290,   291,   296,   297,   300,   303,   306,   307,   311,
     313,   315,   320,   322,   324,   329,   333,   334,   339,   342,
     346,   351,   357,   358,   361,   362,   366,   369,   393,   394
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "\"::=\"", "NUM", "IDENTIFIER",
  "OPTIONAL", "INTEGER", "SIZE", "OCTET", "STRING", "SEQUENCE", "BIT",
  "UNIVERSAL", "PRIVATE", "APPLICATION", "DEFAULT", "CHOICE", "OF",
  "OBJECT", "STR_IDENTIFIER", "BOOLEAN", "ASN1_TRUE", "ASN1_FALSE",
  "TOKEN_NULL", "ANY", "DEFINED", "BY", "SET", "EXPLICIT", "IMPLICIT",
  "DEFINITIONS", "TAGS", "BEGIN", "END", "UTCTime", "GeneralizedTime",
  "GeneralString", "FROM", "IMPORTS", "ENUMERATED", "'+'", "'-'", "'('",
  "')'", "','", "'['", "']'", "'|'", "'{'", "'}'", "'.'", "$accept",
  "definitions", "pos_num", "neg_num", "pos_neg_num", "num_identifier",
  "pos_neg_identifier", "constant", "constant_list", "obj_constant",
  "obj_constant_list", "class", "tag_type", "tag", "default",
  "pos_neg_list", "integer_def", "boolean_def", "Time", "size_def2",
  "size_def", "generalstring_def", "octet_string_def", "bit_element",
  "bit_element_list", "bit_string_def", "enumerated_def", "object_def",
  "type_assig_right", "type_assig_right_tag",
  "type_assig_right_tag_default", "type_assig", "type_assig_list",
  "sequence_def", "set_def", "choise_def", "any_def", "type_def",
  "constant_def", "type_constant", "type_constant_list", "definitions_id",
  "explicit_implicit", 0
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
     295,    43,    45,    40,    41,    44,    91,    93,   124,   123,
     125,    46
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    52,    53,    54,    54,    55,    56,    56,    57,    57,
      58,    58,    59,    59,    60,    60,    61,    61,    62,    62,
      63,    63,    63,    64,    64,    65,    65,    65,    66,    66,
      66,    67,    67,    68,    68,    68,    68,    69,    70,    70,
      71,    71,    72,    72,    73,    73,    74,    74,    75,    76,
      76,    77,    77,    77,    78,    79,    80,    80,    80,    80,
      80,    80,    80,    80,    80,    80,    80,    80,    80,    80,
      80,    81,    81,    82,    82,    82,    83,    84,    84,    85,
      85,    85,    86,    86,    86,    87,    88,    88,    89,    90,
      90,    90,    91,    91,    92,    92,    93,    93,    94,    94
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     8,     1,     2,     2,     1,     1,     1,     1,
       1,     1,     3,     4,     1,     3,     1,     4,     1,     2,
       1,     1,     1,     3,     4,     1,     2,     2,     2,     2,
       2,     1,     3,     1,     4,     4,     7,     1,     1,     1,
       4,     7,     1,     3,     1,     2,     2,     3,     4,     1,
       3,     2,     3,     5,     4,     2,     1,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     2,     1,     2,     2,     2,     1,     3,     4,
       3,     4,     4,     3,     4,     4,     1,     4,     3,     7,
       6,     4,     1,     1,     1,     2,     4,     3,     1,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     0,     0,     0,     0,     1,     0,     8,     9,    97,
      16,    18,     0,    98,    99,     0,     0,    96,    19,     0,
       0,     0,    17,     0,     0,    92,    93,    94,     0,     0,
       0,     0,     0,     2,    95,    56,    33,     0,     0,     0,
       0,     0,    37,    70,    86,     0,    38,    39,    44,     0,
       0,    25,     0,    58,    60,    61,    64,    62,    63,    59,
      66,    71,    88,    65,    69,    67,    68,     0,     0,     0,
       0,     0,    42,    57,     0,    46,     0,     0,     0,    51,
       0,    55,     0,     0,     0,     0,    45,     0,     0,    20,
      21,    22,     0,    26,    27,    72,     0,     0,     3,     0,
       0,     6,     7,    91,     0,     0,     0,     0,     0,    14,
       0,    47,    80,     0,    77,     0,     0,     0,    52,     0,
       0,    83,     0,     0,     0,    49,     0,    23,     0,     3,
       9,    31,     0,     0,     0,     4,     5,     0,     0,    43,
       0,     0,     0,    34,    73,    76,     0,    79,    81,     0,
      85,    87,    82,    84,     0,     0,    54,    24,     0,    35,
       0,    90,     0,    40,     0,     0,    12,    15,    75,     0,
      74,    78,    53,     0,    50,     0,    32,    89,     0,    13,
      11,    29,    30,    10,    28,    48,     0,     0,    36,    41
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     2,   101,   102,   103,    10,   184,   109,   110,    11,
      12,    92,    51,    52,   170,   133,    53,    54,    55,    72,
      73,    56,    57,   125,   126,    58,    59,    60,    61,    62,
     145,   114,   115,    63,    64,    65,    66,    25,    26,    27,
      28,     3,    15
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -96
static const yytype_int16 yypact[] =
{
      24,   -14,    48,    56,     0,   -96,   -10,   -96,    -2,   -96,
     -96,   -96,     2,   -96,   -96,    71,   111,   -96,   -96,   117,
      83,    96,   -96,   127,    42,   -96,   -96,   -96,    10,    77,
     130,   131,   115,   -96,   -96,     8,    90,   132,    13,   135,
      92,   126,   -96,   -96,   122,    15,   -96,   -96,     8,   100,
      93,    49,   119,    94,   -96,   -96,   -96,   -96,   -96,   -96,
     -96,   -96,   -96,   -96,   -96,   -96,   -96,   102,    28,   149,
     110,   150,   -96,   -96,    17,     8,   119,   152,   142,    22,
     152,   -96,   134,   119,   152,   144,   -96,   158,   118,   -96,
     -96,   -96,   160,   -96,   -96,   -96,    35,    88,   -96,   162,
     163,   -96,   -96,   -96,   120,   114,   124,   128,    28,   -96,
      -8,   -96,   -96,    77,   -96,    -7,   119,   158,   -96,    18,
     165,   -96,    50,   119,   129,   -96,    54,   -96,   133,   123,
     -96,   -96,   125,    37,     4,   -96,   -96,    88,   -27,   -96,
      28,   137,    17,   -96,    20,   -96,   152,   -96,   -96,    66,
     -96,   -96,   -96,   -96,   169,   158,   -96,   -96,   136,   -96,
      28,   -96,     7,   -96,   138,   139,   -96,   -96,   -96,    68,
     -96,   -96,   -96,   140,   -96,   114,   -96,   -96,   114,   -96,
     -96,   -96,   -96,   -96,   -96,   -96,   141,   146,   -96,   -96
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -96,   -96,   -96,   -96,   -94,   -95,   -96,    33,   -96,   -12,
     -84,   -96,   -96,   -96,   -96,   -96,   -96,   -96,   -96,   106,
     -20,   -96,   -96,    23,    62,   -96,   -96,   -96,   -49,    69,
     -96,    40,    41,   -96,   -96,   -96,   -96,   -96,   -96,   164,
     -96,   -96,   -96
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -9
static const yytype_int16 yytable[] =
{
      18,   132,   131,    95,     7,     8,     7,     8,     7,     8,
     138,     7,     8,   134,   141,    24,    70,   163,    78,    13,
      14,    70,   107,    70,   164,    85,   168,   112,    86,     1,
      70,    76,    98,    83,   121,     4,   169,   142,   146,   129,
     130,    16,   143,   147,    33,    29,   165,    30,     5,    31,
       9,    71,    17,   162,   161,   111,    71,   177,    71,   118,
     108,    32,    77,   146,    84,    71,   176,   148,   150,    99,
     100,   117,    98,   180,   153,   183,    99,   100,    93,    94,
     186,   159,    35,   187,    36,   160,    37,     6,    38,    39,
     181,   182,     7,     8,    40,   146,    41,    88,    42,   155,
     152,    43,    44,    19,   156,    45,    89,    90,    91,    99,
     100,   155,    46,    47,    48,    20,   172,    49,     7,   130,
      21,   119,    18,    50,    35,   122,    36,    22,    37,    23,
      38,    39,    24,    67,    68,    69,    40,    96,    41,    74,
      42,    80,    75,    43,    44,    79,    81,    45,    82,    87,
      18,    97,   104,   105,    46,    47,    48,   113,    70,    49,
     116,   120,   123,   124,   128,   127,   135,   136,   139,   137,
     151,   140,   154,   173,    -8,   167,   158,   106,   174,   149,
     157,   166,   144,   179,   185,   188,   171,   175,     0,   178,
     189,     0,    34
};

static const yytype_int16 yycheck[] =
{
      12,    96,    96,    52,     4,     5,     4,     5,     4,     5,
     105,     4,     5,    97,   108,     5,     8,    44,    38,    29,
      30,     8,     5,     8,    51,    45,     6,    76,    48,     5,
       8,    18,     4,    18,    83,    49,    16,    45,    45,     4,
       5,    43,    50,    50,    34,     3,   140,     5,     0,     7,
      50,    43,    50,   137,    50,    75,    43,    50,    43,    79,
      43,    19,    49,    45,    49,    43,   160,   116,    50,    41,
      42,    49,     4,     5,   123,   169,    41,    42,    29,    30,
     175,    44,     5,   178,     7,    48,     9,    31,    11,    12,
      22,    23,     4,     5,    17,    45,    19,     4,    21,    45,
      50,    24,    25,    32,    50,    28,    13,    14,    15,    41,
      42,    45,    35,    36,    37,     4,    50,    40,     4,     5,
       3,    80,   134,    46,     5,    84,     7,    44,     9,    33,
      11,    12,     5,     3,     3,    20,    17,    43,    19,    49,
      21,    49,    10,    24,    25,    10,    20,    28,    26,    49,
     162,    49,     3,    43,    35,    36,    37,     5,     8,    40,
      18,    27,    18,     5,     4,    47,     4,     4,    44,    49,
       5,    43,    43,     4,    51,   142,    51,    71,   155,   117,
      47,    44,   113,    44,    44,    44,   146,    51,    -1,    51,
      44,    -1,    28
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     5,    53,    93,    49,     0,    31,     4,     5,    50,
      57,    61,    62,    29,    30,    94,    43,    50,    61,    32,
       4,     3,    44,    33,     5,    89,    90,    91,    92,     3,
       5,     7,    19,    34,    91,     5,     7,     9,    11,    12,
      17,    19,    21,    24,    25,    28,    35,    36,    37,    40,
      46,    64,    65,    68,    69,    70,    73,    74,    77,    78,
      79,    80,    81,    85,    86,    87,    88,     3,     3,    20,
       8,    43,    71,    72,    49,    10,    18,    49,    72,    10,
      49,    20,    26,    18,    49,    72,    72,    49,     4,    13,
      14,    15,    63,    29,    30,    80,    43,    49,     4,    41,
      42,    54,    55,    56,     3,    43,    71,     5,    43,    59,
      60,    72,    80,     5,    83,    84,    18,    49,    72,    84,
      27,    80,    84,    18,     5,    75,    76,    47,     4,     4,
       5,    56,    57,    67,    62,     4,     4,    49,    57,    44,
      43,    56,    45,    50,    81,    82,    45,    50,    80,    76,
      50,     5,    50,    80,    43,    45,    50,    47,    51,    44,
      48,    50,    62,    44,    51,    56,    44,    59,     6,    16,
      66,    83,    50,     4,    75,    51,    56,    50,    51,    44,
       5,    22,    23,    56,    58,    44,    57,    57,    44,    44
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
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      YYFPRINTF (stderr, "\n");
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


/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



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
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{


    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.

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

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yytoken = 0;
  yyss = yyssa;
  yyvs = yyvsa;
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
	YYSTACK_RELOCATE (yyss_alloc, yyss);
	YYSTACK_RELOCATE (yyvs_alloc, yyvs);
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

/* Line 1455 of yacc.c  */
#line 122 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_DEFINITIONS|(yyvsp[(3) - (8)].constant));
                    _asn1_set_name((yyval.node),_asn1_get_name((yyvsp[(1) - (8)].node)));
                    _asn1_set_name((yyvsp[(1) - (8)].node),"");
                    _asn1_set_right((yyvsp[(1) - (8)].node),(yyvsp[(7) - (8)].node));
                    _asn1_set_down((yyval.node),(yyvsp[(1) - (8)].node));

		    p_tree=(yyval.node);
		    }
    break;

  case 3:

/* Line 1455 of yacc.c  */
#line 132 "ASN1.y"
    {strcpy((yyval.str),(yyvsp[(1) - (1)].str));}
    break;

  case 4:

/* Line 1455 of yacc.c  */
#line 133 "ASN1.y"
    {strcpy((yyval.str),(yyvsp[(2) - (2)].str));}
    break;

  case 5:

/* Line 1455 of yacc.c  */
#line 136 "ASN1.y"
    {strcpy((yyval.str),"-");
                       strcat((yyval.str),(yyvsp[(2) - (2)].str));}
    break;

  case 6:

/* Line 1455 of yacc.c  */
#line 140 "ASN1.y"
    {strcpy((yyval.str),(yyvsp[(1) - (1)].str));}
    break;

  case 7:

/* Line 1455 of yacc.c  */
#line 141 "ASN1.y"
    {strcpy((yyval.str),(yyvsp[(1) - (1)].str));}
    break;

  case 8:

/* Line 1455 of yacc.c  */
#line 144 "ASN1.y"
    {strcpy((yyval.str),(yyvsp[(1) - (1)].str));}
    break;

  case 9:

/* Line 1455 of yacc.c  */
#line 145 "ASN1.y"
    {strcpy((yyval.str),(yyvsp[(1) - (1)].str));}
    break;

  case 10:

/* Line 1455 of yacc.c  */
#line 148 "ASN1.y"
    {strcpy((yyval.str),(yyvsp[(1) - (1)].str));}
    break;

  case 11:

/* Line 1455 of yacc.c  */
#line 149 "ASN1.y"
    {strcpy((yyval.str),(yyvsp[(1) - (1)].str));}
    break;

  case 12:

/* Line 1455 of yacc.c  */
#line 152 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_CONSTANT);
                                       _asn1_set_value((yyval.node),(yyvsp[(2) - (3)].str),strlen((yyvsp[(2) - (3)].str))+1);}
    break;

  case 13:

/* Line 1455 of yacc.c  */
#line 154 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_CONSTANT);
	                               _asn1_set_name((yyval.node),(yyvsp[(1) - (4)].str));
                                       _asn1_set_value((yyval.node),(yyvsp[(3) - (4)].str),strlen((yyvsp[(3) - (4)].str))+1);}
    break;

  case 14:

/* Line 1455 of yacc.c  */
#line 159 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 15:

/* Line 1455 of yacc.c  */
#line 160 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (3)].node);
                                            _asn1_set_right(_asn1_get_last_right((yyvsp[(1) - (3)].node)),(yyvsp[(3) - (3)].node));}
    break;

  case 16:

/* Line 1455 of yacc.c  */
#line 164 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_CONSTANT);
                                   _asn1_set_value((yyval.node),(yyvsp[(1) - (1)].str),strlen((yyvsp[(1) - (1)].str))+1);}
    break;

  case 17:

/* Line 1455 of yacc.c  */
#line 166 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_CONSTANT);
	                            _asn1_set_name((yyval.node),(yyvsp[(1) - (4)].str));
                                    _asn1_set_value((yyval.node),(yyvsp[(3) - (4)].str),strlen((yyvsp[(3) - (4)].str))+1);}
    break;

  case 18:

/* Line 1455 of yacc.c  */
#line 171 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 19:

/* Line 1455 of yacc.c  */
#line 172 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (2)].node);
                                                    _asn1_set_right(_asn1_get_last_right((yyvsp[(1) - (2)].node)),(yyvsp[(2) - (2)].node));}
    break;

  case 20:

/* Line 1455 of yacc.c  */
#line 176 "ASN1.y"
    {(yyval.constant)=CONST_UNIVERSAL;}
    break;

  case 21:

/* Line 1455 of yacc.c  */
#line 177 "ASN1.y"
    {(yyval.constant)=CONST_PRIVATE;}
    break;

  case 22:

/* Line 1455 of yacc.c  */
#line 178 "ASN1.y"
    {(yyval.constant)=CONST_APPLICATION;}
    break;

  case 23:

/* Line 1455 of yacc.c  */
#line 181 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_TAG);
                            _asn1_set_value((yyval.node),(yyvsp[(2) - (3)].str),strlen((yyvsp[(2) - (3)].str))+1);}
    break;

  case 24:

/* Line 1455 of yacc.c  */
#line 183 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_TAG | (yyvsp[(2) - (4)].constant));
                                _asn1_set_value((yyval.node),(yyvsp[(3) - (4)].str),strlen((yyvsp[(3) - (4)].str))+1);}
    break;

  case 25:

/* Line 1455 of yacc.c  */
#line 187 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 26:

/* Line 1455 of yacc.c  */
#line 188 "ASN1.y"
    {(yyval.node)=_asn1_mod_type((yyvsp[(1) - (2)].node),CONST_EXPLICIT);}
    break;

  case 27:

/* Line 1455 of yacc.c  */
#line 189 "ASN1.y"
    {(yyval.node)=_asn1_mod_type((yyvsp[(1) - (2)].node),CONST_IMPLICIT);}
    break;

  case 28:

/* Line 1455 of yacc.c  */
#line 192 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_DEFAULT);
                                       _asn1_set_value((yyval.node),(yyvsp[(2) - (2)].str),strlen((yyvsp[(2) - (2)].str))+1);}
    break;

  case 29:

/* Line 1455 of yacc.c  */
#line 194 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_DEFAULT|CONST_TRUE);}
    break;

  case 30:

/* Line 1455 of yacc.c  */
#line 195 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_DEFAULT|CONST_FALSE);}
    break;

  case 33:

/* Line 1455 of yacc.c  */
#line 204 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_INTEGER);}
    break;

  case 34:

/* Line 1455 of yacc.c  */
#line 205 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_INTEGER|CONST_LIST);
	                                 _asn1_set_down((yyval.node),(yyvsp[(3) - (4)].node));}
    break;

  case 35:

/* Line 1455 of yacc.c  */
#line 207 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_INTEGER);}
    break;

  case 36:

/* Line 1455 of yacc.c  */
#line 209 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_INTEGER|CONST_MIN_MAX);
                                         _asn1_set_down((yyval.node),_asn1_add_node(TYPE_SIZE));
                                         _asn1_set_value(_asn1_get_down((yyval.node)),(yyvsp[(6) - (7)].str),strlen((yyvsp[(6) - (7)].str))+1);
                                         _asn1_set_name(_asn1_get_down((yyval.node)),(yyvsp[(3) - (7)].str));}
    break;

  case 37:

/* Line 1455 of yacc.c  */
#line 215 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_BOOLEAN);}
    break;

  case 38:

/* Line 1455 of yacc.c  */
#line 218 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_TIME|CONST_UTC);}
    break;

  case 39:

/* Line 1455 of yacc.c  */
#line 219 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_TIME|CONST_GENERALIZED);}
    break;

  case 40:

/* Line 1455 of yacc.c  */
#line 222 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_SIZE|CONST_1_PARAM);
	                              _asn1_set_value((yyval.node),(yyvsp[(3) - (4)].str),strlen((yyvsp[(3) - (4)].str))+1);}
    break;

  case 41:

/* Line 1455 of yacc.c  */
#line 225 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_SIZE|CONST_MIN_MAX);
	                              _asn1_set_value((yyval.node),(yyvsp[(3) - (7)].str),strlen((yyvsp[(3) - (7)].str))+1);
                                      _asn1_set_name((yyval.node),(yyvsp[(6) - (7)].str));}
    break;

  case 42:

/* Line 1455 of yacc.c  */
#line 230 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 43:

/* Line 1455 of yacc.c  */
#line 231 "ASN1.y"
    {(yyval.node)=(yyvsp[(2) - (3)].node);}
    break;

  case 44:

/* Line 1455 of yacc.c  */
#line 234 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_GENERALSTRING);}
    break;

  case 45:

/* Line 1455 of yacc.c  */
#line 235 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_GENERALSTRING|CONST_SIZE);
					  _asn1_set_down((yyval.node),(yyvsp[(2) - (2)].node));}
    break;

  case 46:

/* Line 1455 of yacc.c  */
#line 239 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_OCTET_STRING);}
    break;

  case 47:

/* Line 1455 of yacc.c  */
#line 240 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_OCTET_STRING|CONST_SIZE);
                                           _asn1_set_down((yyval.node),(yyvsp[(3) - (3)].node));}
    break;

  case 48:

/* Line 1455 of yacc.c  */
#line 244 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_CONSTANT);
	                           _asn1_set_name((yyval.node),(yyvsp[(1) - (4)].str));
                                    _asn1_set_value((yyval.node),(yyvsp[(3) - (4)].str),strlen((yyvsp[(3) - (4)].str))+1);}
    break;

  case 49:

/* Line 1455 of yacc.c  */
#line 249 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 50:

/* Line 1455 of yacc.c  */
#line 250 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (3)].node);
                                                       _asn1_set_right(_asn1_get_last_right((yyvsp[(1) - (3)].node)),(yyvsp[(3) - (3)].node));}
    break;

  case 51:

/* Line 1455 of yacc.c  */
#line 254 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_BIT_STRING);}
    break;

  case 52:

/* Line 1455 of yacc.c  */
#line 255 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_BIT_STRING|CONST_SIZE);}
    break;

  case 53:

/* Line 1455 of yacc.c  */
#line 257 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_BIT_STRING|CONST_LIST);
                                _asn1_set_down((yyval.node),(yyvsp[(4) - (5)].node));}
    break;

  case 54:

/* Line 1455 of yacc.c  */
#line 262 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_ENUMERATED|CONST_LIST);
                                _asn1_set_down((yyval.node),(yyvsp[(3) - (4)].node));}
    break;

  case 55:

/* Line 1455 of yacc.c  */
#line 267 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_OBJECT_ID);}
    break;

  case 56:

/* Line 1455 of yacc.c  */
#line 270 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_IDENTIFIER);
                                       _asn1_set_value((yyval.node),(yyvsp[(1) - (1)].str),strlen((yyvsp[(1) - (1)].str))+1);}
    break;

  case 57:

/* Line 1455 of yacc.c  */
#line 272 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_IDENTIFIER|CONST_SIZE);
                                       _asn1_set_value((yyval.node),(yyvsp[(1) - (2)].str),strlen((yyvsp[(1) - (2)].str))+1);
                                       _asn1_set_down((yyval.node),(yyvsp[(2) - (2)].node));}
    break;

  case 58:

/* Line 1455 of yacc.c  */
#line 275 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 59:

/* Line 1455 of yacc.c  */
#line 276 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 60:

/* Line 1455 of yacc.c  */
#line 277 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 62:

/* Line 1455 of yacc.c  */
#line 279 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 63:

/* Line 1455 of yacc.c  */
#line 280 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 64:

/* Line 1455 of yacc.c  */
#line 281 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 65:

/* Line 1455 of yacc.c  */
#line 282 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 66:

/* Line 1455 of yacc.c  */
#line 283 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 67:

/* Line 1455 of yacc.c  */
#line 284 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 68:

/* Line 1455 of yacc.c  */
#line 285 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 69:

/* Line 1455 of yacc.c  */
#line 286 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 70:

/* Line 1455 of yacc.c  */
#line 287 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_NULL);}
    break;

  case 71:

/* Line 1455 of yacc.c  */
#line 290 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 72:

/* Line 1455 of yacc.c  */
#line 291 "ASN1.y"
    {(yyval.node)=_asn1_mod_type((yyvsp[(2) - (2)].node),CONST_TAG);
                                               _asn1_set_right((yyvsp[(1) - (2)].node),_asn1_get_down((yyval.node)));
                                               _asn1_set_down((yyval.node),(yyvsp[(1) - (2)].node));}
    break;

  case 73:

/* Line 1455 of yacc.c  */
#line 296 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 74:

/* Line 1455 of yacc.c  */
#line 297 "ASN1.y"
    {(yyval.node)=_asn1_mod_type((yyvsp[(1) - (2)].node),CONST_DEFAULT);
                                                       _asn1_set_right((yyvsp[(2) - (2)].node),_asn1_get_down((yyval.node)));
						       _asn1_set_down((yyval.node),(yyvsp[(2) - (2)].node));}
    break;

  case 75:

/* Line 1455 of yacc.c  */
#line 300 "ASN1.y"
    {(yyval.node)=_asn1_mod_type((yyvsp[(1) - (2)].node),CONST_OPTION);}
    break;

  case 76:

/* Line 1455 of yacc.c  */
#line 303 "ASN1.y"
    {(yyval.node)=_asn1_set_name((yyvsp[(2) - (2)].node),(yyvsp[(1) - (2)].str));}
    break;

  case 77:

/* Line 1455 of yacc.c  */
#line 306 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 78:

/* Line 1455 of yacc.c  */
#line 307 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (3)].node);
                                                _asn1_set_right(_asn1_get_last_right((yyvsp[(1) - (3)].node)),(yyvsp[(3) - (3)].node));}
    break;

  case 79:

/* Line 1455 of yacc.c  */
#line 311 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_SEQUENCE);
                                              _asn1_set_down((yyval.node),(yyvsp[(3) - (4)].node));}
    break;

  case 80:

/* Line 1455 of yacc.c  */
#line 313 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_SEQUENCE_OF);
                                              _asn1_set_down((yyval.node),(yyvsp[(3) - (3)].node));}
    break;

  case 81:

/* Line 1455 of yacc.c  */
#line 315 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_SEQUENCE_OF|CONST_SIZE);
                                            _asn1_set_right((yyvsp[(2) - (4)].node),(yyvsp[(4) - (4)].node));
                                            _asn1_set_down((yyval.node),(yyvsp[(2) - (4)].node));}
    break;

  case 82:

/* Line 1455 of yacc.c  */
#line 320 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_SET);
                                     _asn1_set_down((yyval.node),(yyvsp[(3) - (4)].node));}
    break;

  case 83:

/* Line 1455 of yacc.c  */
#line 322 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_SET_OF);
                                     _asn1_set_down((yyval.node),(yyvsp[(3) - (3)].node));}
    break;

  case 84:

/* Line 1455 of yacc.c  */
#line 324 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_SET_OF|CONST_SIZE);
                                       _asn1_set_right((yyvsp[(2) - (4)].node),(yyvsp[(4) - (4)].node));
                                       _asn1_set_down((yyval.node),(yyvsp[(2) - (4)].node));}
    break;

  case 85:

/* Line 1455 of yacc.c  */
#line 329 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_CHOICE);
                                             _asn1_set_down((yyval.node),(yyvsp[(3) - (4)].node));}
    break;

  case 86:

/* Line 1455 of yacc.c  */
#line 333 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_ANY);}
    break;

  case 87:

/* Line 1455 of yacc.c  */
#line 334 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_ANY|CONST_DEFINED_BY);
                                        _asn1_set_down((yyval.node),_asn1_add_node(TYPE_CONSTANT));
	                                _asn1_set_name(_asn1_get_down((yyval.node)),(yyvsp[(4) - (4)].str));}
    break;

  case 88:

/* Line 1455 of yacc.c  */
#line 339 "ASN1.y"
    {(yyval.node)=_asn1_set_name((yyvsp[(3) - (3)].node),(yyvsp[(1) - (3)].str));}
    break;

  case 89:

/* Line 1455 of yacc.c  */
#line 343 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_OBJECT_ID|CONST_ASSIGN);
                         _asn1_set_name((yyval.node),(yyvsp[(1) - (7)].str));
                         _asn1_set_down((yyval.node),(yyvsp[(6) - (7)].node));}
    break;

  case 90:

/* Line 1455 of yacc.c  */
#line 347 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_OBJECT_ID|CONST_ASSIGN|CONST_1_PARAM);
                         _asn1_set_name((yyval.node),(yyvsp[(1) - (6)].str));
                         _asn1_set_value((yyval.node),(yyvsp[(2) - (6)].str),strlen((yyvsp[(2) - (6)].str))+1);
                         _asn1_set_down((yyval.node),(yyvsp[(5) - (6)].node));}
    break;

  case 91:

/* Line 1455 of yacc.c  */
#line 352 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_INTEGER|CONST_ASSIGN);
                         _asn1_set_name((yyval.node),(yyvsp[(1) - (4)].str));
                         _asn1_set_value((yyval.node),(yyvsp[(4) - (4)].str),strlen((yyvsp[(4) - (4)].str))+1);}
    break;

  case 92:

/* Line 1455 of yacc.c  */
#line 357 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 93:

/* Line 1455 of yacc.c  */
#line 358 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 94:

/* Line 1455 of yacc.c  */
#line 361 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 95:

/* Line 1455 of yacc.c  */
#line 362 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (2)].node);
                                                          _asn1_set_right(_asn1_get_last_right((yyvsp[(1) - (2)].node)),(yyvsp[(2) - (2)].node));}
    break;

  case 96:

/* Line 1455 of yacc.c  */
#line 366 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_OBJECT_ID);
                                                          _asn1_set_down((yyval.node),(yyvsp[(3) - (4)].node));
                                                          _asn1_set_name((yyval.node),(yyvsp[(1) - (4)].str));}
    break;

  case 97:

/* Line 1455 of yacc.c  */
#line 369 "ASN1.y"
    {(yyval.node)=_asn1_add_node(TYPE_OBJECT_ID);
                                                          _asn1_set_name((yyval.node),(yyvsp[(1) - (3)].str));}
    break;

  case 98:

/* Line 1455 of yacc.c  */
#line 393 "ASN1.y"
    {(yyval.constant)=CONST_EXPLICIT;}
    break;

  case 99:

/* Line 1455 of yacc.c  */
#line 394 "ASN1.y"
    {(yyval.constant)=CONST_IMPLICIT;}
    break;



/* Line 1455 of yacc.c  */
#line 2371 "ASN1.c"
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
		      yytoken, &yylval);
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

#if !defined(yyoverflow) || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
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



/* Line 1675 of yacc.c  */
#line 398 "ASN1.y"




static const char *key_word[] = {
  "::=","OPTIONAL","INTEGER","SIZE","OCTET","STRING"
  ,"SEQUENCE","BIT","UNIVERSAL","PRIVATE","OPTIONAL"
  ,"DEFAULT","CHOICE","OF","OBJECT","IDENTIFIER"
  ,"BOOLEAN","TRUE","FALSE","APPLICATION","ANY","DEFINED"
  ,"SET","BY","EXPLICIT","IMPLICIT","DEFINITIONS","TAGS"
  ,"BEGIN","END","UTCTime","GeneralizedTime"
  ,"GeneralString","FROM","IMPORTS","NULL","ENUMERATED"};
static const int key_word_token[] = {
  ASSIG,OPTIONAL,INTEGER,SIZE,OCTET,STRING
  ,SEQUENCE,BIT,UNIVERSAL,PRIVATE,OPTIONAL
  ,DEFAULT,CHOICE,OF,OBJECT,STR_IDENTIFIER
  ,BOOLEAN,ASN1_TRUE,ASN1_FALSE,APPLICATION,ANY,DEFINED
  ,SET,BY,EXPLICIT,IMPLICIT,DEFINITIONS,TAGS
  ,BEGIN,END,UTCTime,GeneralizedTime
  ,GeneralString,FROM,IMPORTS,TOKEN_NULL,ENUMERATED};

/*************************************************************/
/*  Function: _asn1_yylex                                    */
/*  Description: looks for tokens in file_asn1 pointer file. */
/*  Return: int                                              */
/*    Token identifier or ASCII code or 0(zero: End Of File) */
/*************************************************************/
static int
_asn1_yylex()
{
  int c,counter=0,k,lastc;
  char string[ASN1_MAX_NAME_SIZE+1]; /* will contain the next token */
  size_t i;

  while(1)
    {
    while((c=fgetc(file_asn1))==' ' || c=='\t' || c=='\n')
      if(c=='\n') lineNumber++;

    if(c==EOF){
      strcpy(lastToken,"End Of File");
      return 0;
    }

    if(c=='(' || c==')' || c=='[' || c==']' ||
       c=='{' || c=='}' || c==',' || c=='.' ||
       c=='+' || c=='|'){
      lastToken[0]=c;lastToken[1]=0;
      return c;
    }
    if(c=='-'){  /* Maybe the first '-' of a comment */
      if((c=fgetc(file_asn1))!='-'){
	ungetc(c,file_asn1);
	lastToken[0]='-';lastToken[1]=0;
	return '-';
      }
      else{ /* Comments */
	lastc=0;
	counter=0;
	/* A comment finishes at the next double hypen or the end of line */
	while((c=fgetc(file_asn1))!=EOF && c!='\n' &&
	      (lastc!='-' || (lastc=='-' && c!='-')))
	  lastc=c;
	if(c==EOF){
	  strcpy(lastToken,"End Of File");
	  return 0;
	}
	else{
	  if(c=='\n') lineNumber++;
	  continue; /* next char, please! (repeat the search) */
	}
      }
    }
    string[counter++]=c;
    /* Till the end of the token */
    while(!((c=fgetc(file_asn1))==EOF || c==' '|| c=='\t' || c=='\n' ||
	     c=='(' || c==')' || c=='[' || c==']' ||
	     c=='{' || c=='}' || c==',' || c=='.'))
      {
	if(counter>=ASN1_MAX_NAME_SIZE){
	  result_parse=ASN1_NAME_TOO_LONG;
	  return 0;
	}
	string[counter++]=c;
      }
    ungetc(c,file_asn1);
    string[counter]=0;
    strcpy(lastToken,string);

    /* Is STRING a number? */
    for(k=0;k<counter;k++)
      if(!isdigit(string[k])) break;
    if(k>=counter)
      {
      strcpy(yylval.str,string);
      return NUM; /* return the number */
      }

    /* Is STRING a keyword? */
    for(i=0;i<(sizeof(key_word)/sizeof(char*));i++)
      if(!strcmp(string,key_word[i])) return key_word_token[i];

    /* STRING is an IDENTIFIER */
    strcpy(yylval.str,string);
    return IDENTIFIER;
    }
}

/*************************************************************/
/*  Function: _asn1_create_errorDescription                  */
/*  Description: creates a string with the description of the*/
/*    error.                                                 */
/*  Parameters:                                              */
/*    error : error to describe.                             */
/*    errorDescription: string that will contain the         */
/*                      description.                         */
/*************************************************************/
static void
_asn1_create_errorDescription(int error,char *errorDescription)
{
  switch(error){
  case ASN1_SUCCESS: case ASN1_FILE_NOT_FOUND:
    if (errorDescription!=NULL) errorDescription[0]=0;
    break;
  case ASN1_SYNTAX_ERROR:
    if (errorDescription!=NULL) {
	strcpy(errorDescription,fileName);
	strcat(errorDescription,":");
	_asn1_ltostr(lineNumber,errorDescription+strlen(fileName)+1);
	strcat(errorDescription,": parse error near '");
	strcat(errorDescription,lastToken);
	strcat(errorDescription,"'");
    }
    break;
  case ASN1_NAME_TOO_LONG:
    if (errorDescription!=NULL) {
       strcpy(errorDescription,fileName);
       strcat(errorDescription,":");
       _asn1_ltostr(lineNumber,errorDescription+strlen(fileName)+1);
       strcat(errorDescription,": name too long (more than ");
       _asn1_ltostr(ASN1_MAX_NAME_SIZE,errorDescription+strlen(errorDescription));
       strcat(errorDescription," characters)");
    }
    break;
  case ASN1_IDENTIFIER_NOT_FOUND:
    if (errorDescription!=NULL) {
       strcpy(errorDescription,fileName);
       strcat(errorDescription,":");
       strcat(errorDescription,": identifier '");
       strcat(errorDescription,_asn1_identifierMissing);
       strcat(errorDescription,"' not found");
    }
    break;
  default:
    if (errorDescription!=NULL) errorDescription[0]=0;
    break;
  }

}

/**
 * asn1_parser2tree:
 * @file_name: specify the path and the name of file that contains
 *   ASN.1 declarations.
 * @definitions: return the pointer to the structure created from
 *   "file_name" ASN.1 declarations.
 * @errorDescription: return the error description or an empty
 * string if success.
 *
 * Function used to start the parse algorithm.  Creates the structures
 * needed to manage the definitions included in @file_name file.
 *
 * Returns: %ASN1_SUCCESS if the file has a correct syntax and every
 *   identifier is known, %ASN1_ELEMENT_NOT_EMPTY if @definitions not
 *   %ASN1_TYPE_EMPTY, %ASN1_FILE_NOT_FOUND if an error occured while
 *   opening @file_name, %ASN1_SYNTAX_ERROR if the syntax is not
 *   correct, %ASN1_IDENTIFIER_NOT_FOUND if in the file there is an
 *   identifier that is not defined, %ASN1_NAME_TOO_LONG if in the
 *   file there is an identifier whith more than %ASN1_MAX_NAME_SIZE
 *   characters.
 **/
asn1_retCode
asn1_parser2tree(const char *file_name, ASN1_TYPE *definitions,
		 char *errorDescription){

  p_tree=ASN1_TYPE_EMPTY;

  if(*definitions != ASN1_TYPE_EMPTY)
    return ASN1_ELEMENT_NOT_EMPTY;

  *definitions=ASN1_TYPE_EMPTY;

  fileName = file_name;

  /* open the file to parse */
  file_asn1=fopen(file_name,"r");

  if(file_asn1==NULL){
    result_parse=ASN1_FILE_NOT_FOUND;
  }
  else{
    result_parse=ASN1_SUCCESS;

    lineNumber=1;
    yyparse();

    fclose(file_asn1);

    if(result_parse==ASN1_SUCCESS){ /* syntax OK */
      /* set IMPLICIT or EXPLICIT property */
      _asn1_set_default_tag(p_tree);
      /* set CONST_SET and CONST_NOT_USED */
      _asn1_type_set_config(p_tree);
      /* check the identifier definitions */
      result_parse=_asn1_check_identifier(p_tree);
      if(result_parse==ASN1_SUCCESS){ /* all identifier defined */
	/* Delete the list and keep the ASN1 structure */
	_asn1_delete_list();
	/* Convert into DER coding the value assign to INTEGER constants */
	_asn1_change_integer_value(p_tree);
	/* Expand the IDs of OBJECT IDENTIFIER constants */
	_asn1_expand_object_id(p_tree);

	*definitions=p_tree;
      }
      else /* some identifiers not defined */
	/* Delete the list and the ASN1 structure */
	_asn1_delete_list_and_nodes();
    }
    else  /* syntax error */
      /* Delete the list and the ASN1 structure */
      _asn1_delete_list_and_nodes();
  }

  if (errorDescription!=NULL)
	_asn1_create_errorDescription(result_parse,errorDescription);

  return result_parse;
}

/**
 * asn1_parser2array:
 * @inputFileName: specify the path and the name of file that
 *   contains ASN.1 declarations.
 * @outputFileName: specify the path and the name of file that will
 *   contain the C vector definition.
 * @vectorName: specify the name of the C vector.
 * @errorDescription : return the error description or an empty
 *   string if success.
 *
 * Function that generates a C structure from an ASN1 file.  Creates a
 * file containing a C vector to use to manage the definitions
 * included in @inputFileName file. If @inputFileName is
 * "/aa/bb/xx.yy" and @outputFileName is %NULL, the file created is
 * "/aa/bb/xx_asn1_tab.c".  If @vectorName is %NULL the vector name
 * will be "xx_asn1_tab".
 *
 * Returns: %ASN1_SUCCESS if the file has a correct syntax and every
 *   identifier is known, %ASN1_FILE_NOT_FOUND if an error occured
 *   while opening @inputFileName, %ASN1_SYNTAX_ERROR if the syntax is
 *   not correct, %ASN1_IDENTIFIER_NOT_FOUND if in the file there is
 *   an identifier that is not defined, %ASN1_NAME_TOO_LONG if in the
 *   file there is an identifier whith more than %ASN1_MAX_NAME_SIZE
 *   characters.
 **/
int asn1_parser2array(const char *inputFileName,const char *outputFileName,
		      const char *vectorName,char *errorDescription){
  char *file_out_name=NULL;
  char *vector_name=NULL;
  const char *char_p,*slash_p,*dot_p;

  p_tree=NULL;

  fileName = inputFileName;

  /* open the file to parse */
  file_asn1=fopen(inputFileName,"r");

  if(file_asn1==NULL)
    result_parse=ASN1_FILE_NOT_FOUND;
  else{
    result_parse=ASN1_SUCCESS;

    lineNumber=1;
    yyparse();

    fclose(file_asn1);

    if(result_parse==ASN1_SUCCESS){ /* syntax OK */
      /* set IMPLICIT or EXPLICIT property */
      _asn1_set_default_tag(p_tree);
      /* set CONST_SET and CONST_NOT_USED */
      _asn1_type_set_config(p_tree);
      /* check the identifier definitions */
      result_parse=_asn1_check_identifier(p_tree);

      if(result_parse==ASN1_SUCCESS){ /* all identifier defined */

	/* searching the last '/' and '.' in inputFileName */
	char_p=inputFileName;
	slash_p=inputFileName;
	while((char_p=strchr(char_p,'/'))){
	  char_p++;
	  slash_p=char_p;
	}

	char_p=slash_p;
	dot_p=inputFileName+strlen(inputFileName);

	while((char_p=strchr(char_p,'.'))){
	  dot_p=char_p;
	  char_p++;
	}

	if(outputFileName == NULL){
	  /* file_out_name = inputFileName + _asn1_tab.c */
	  file_out_name=(char *)malloc(dot_p-inputFileName+1+
				       strlen("_asn1_tab.c"));
	  memcpy(file_out_name,inputFileName,dot_p-inputFileName);
	  file_out_name[dot_p-inputFileName]=0;
	  strcat(file_out_name,"_asn1_tab.c");
	}
	else{
	  /* file_out_name = inputFileName */
	  file_out_name=(char *)malloc(strlen(outputFileName)+1);
	  strcpy(file_out_name,outputFileName);
	}

	if(vectorName == NULL){
	  /* vector_name = file name + _asn1_tab */
	  vector_name=(char *)malloc(dot_p-slash_p+1+
				     strlen("_asn1_tab"));
	  memcpy(vector_name,slash_p,dot_p-slash_p);
	  vector_name[dot_p-slash_p]=0;
	  strcat(vector_name,"_asn1_tab");
	}
	else{
	  /* vector_name = vectorName */
	  vector_name=(char *)malloc(strlen(vectorName)+1);
	  strcpy(vector_name,vectorName);
	}

	/* Save structure in a file */
	_asn1_create_static_structure(p_tree,
				      file_out_name,vector_name);

	free(file_out_name);
	free(vector_name);
      } /* result == OK */
    }   /* result == OK */

    /* Delete the list and the ASN1 structure */
    _asn1_delete_list_and_nodes();
  } /* inputFile exist */

  if (errorDescription!=NULL)
	_asn1_create_errorDescription(result_parse,errorDescription);

  return result_parse;
}

/*************************************************************/
/*  Function: _asn1_yyerror                                  */
/*  Description: function called when there are syntax errors*/
/*  Parameters:                                              */
/*    char *s : error description                            */
/*  Return: int                                              */
/*                                                           */
/*************************************************************/
static int _asn1_yyerror (const char *s)
{
  /* Sends the error description to the std_out */

#if 0
  printf("_asn1_yyerror:%s:%ld: %s (Last Token:'%s')\n",fileName,
	 lineNumber,s,lastToken);
#endif

  if(result_parse!=ASN1_NAME_TOO_LONG)
    result_parse=ASN1_SYNTAX_ERROR;

  return 0;
}

