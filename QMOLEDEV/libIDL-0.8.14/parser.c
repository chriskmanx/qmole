
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



/* Copy the first part of user declarations.  */

/* Line 189 of yacc.c  */
#line 24 "./parser.y"

#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <errno.h>
#include "rename.h"
#include "util.h"

#define REF_IDENTS

#define do_binop(rv,op,a,b)		do {		\
	if (IDL_binop_chktypes (op, a, b))		\
		YYABORT;				\
	if (!(__IDL_flags & IDLF_NO_EVAL_CONST)) {	\
		rv = IDL_binop_eval (op, a, b);		\
		IDL_tree_free (a);			\
		IDL_tree_free (b);			\
		if (!rv) YYABORT;			\
	} else {					\
		rv = IDL_binop_new (op, a, b);		\
	}						\
} while (0)

#define do_unaryop(rv,op,a)		do {		\
	if (IDL_unaryop_chktypes (op, a))		\
		YYABORT;				\
	if (!(__IDL_flags & IDLF_NO_EVAL_CONST)) {	\
		rv = IDL_unaryop_eval (op, a);		\
		IDL_tree_free (a);			\
		if (!rv) YYABORT;			\
	} else {					\
		rv = IDL_unaryop_new (op, a);		\
	}						\
} while (0)

#define IS_INHIBIT_STATE()				\
	(__IDL_inhibits > 0 ||				\
	  ((__IDL_flags & IDLF_INHIBIT_INCLUDES) &&	\
	     (__IDL_flagsi & IDLFP_IN_INCLUDES) ) )


#define assign_declspec(tree,declspec)	do {		\
	IDL_NODE_DECLSPEC (tree) = declspec;		\
	if ( IS_INHIBIT_STATE() ) {			\
		IDL_NODE_DECLSPEC (tree) |=		\
			IDLF_DECLSPEC_EXIST |		\
			IDLF_DECLSPEC_INHIBIT;		\
	}						\
	if ( __IDL_pidl > 0 ) {				\
		IDL_NODE_DECLSPEC (tree) |=		\
			IDLF_DECLSPEC_PIDL;		\
	}						\
} while (0)

#define assign_props(tree,props)	do {		\
	if (__IDL_flags & IDLF_PROPERTIES)		\
		IDL_NODE_PROPERTIES (tree) = (props);	\
	else						\
		__IDL_free_properties (props);		\
} while (0)

extern int		yylex				(void);
static IDL_declspec_t	IDL_parse_declspec		(const char *strspec);
static int		IDL_binop_chktypes		(enum IDL_binop op,
							 IDL_tree a,
							 IDL_tree b);
static int		IDL_unaryop_chktypes		(enum IDL_unaryop op,
							 IDL_tree a);
static IDL_tree		IDL_binop_eval			(enum IDL_binop op,
							 IDL_tree a,
							 IDL_tree b);
static IDL_tree		IDL_unaryop_eval		(enum IDL_unaryop op,
							 IDL_tree a);
static IDL_tree		list_start			(IDL_tree a,
							 gboolean filter_null);
static IDL_tree		list_chain			(IDL_tree a,
							 IDL_tree b,
							 gboolean filter_null);
static IDL_tree		zlist_chain			(IDL_tree a,
							 IDL_tree b,
							 gboolean filter_null);
static int		do_token_error			(IDL_tree p,
							 const char *message,
							 gboolean prev);
static void		illegal_context_type_error	(IDL_tree p,
							 const char *what);
static void		illegal_type_error		(IDL_tree p,
							 const char *message);


/* Line 189 of yacc.c  */
#line 167 "y.tab.c"

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

/* Line 214 of yacc.c  */
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



/* Line 214 of yacc.c  */
#line 335 "y.tab.c"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */


/* Line 264 of yacc.c  */
#line 347 "y.tab.c"

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
#define YYFINAL  26
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   695

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  79
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  148
/* YYNRULES -- Number of rules.  */
#define YYNRULES  279
/* YYNRULES -- Number of states.  */
#define YYNSTATES  397

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   312

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,    73,    68,     2,
      63,    64,    71,    69,    59,    70,     2,    72,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    62,    58,
      75,    65,    76,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    77,     2,    78,    67,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    60,    66,    61,    74,     2,     2,     2,
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
      55,    56,    57
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     4,     6,     7,     9,    11,    14,    16,
      17,    19,    21,    22,    24,    27,    30,    33,    36,    39,
      41,    43,    45,    47,    50,    51,    59,    61,    63,    65,
      66,    67,    80,    86,    87,    90,    92,    96,    98,    99,
     102,   105,   108,   111,   114,   117,   119,   121,   124,   128,
     130,   132,   134,   138,   139,   146,   149,   151,   153,   155,
     157,   159,   161,   163,   165,   166,   168,   169,   171,   172,
     173,   183,   184,   185,   199,   201,   203,   205,   207,   209,
     211,   213,   216,   220,   223,   225,   228,   232,   235,   238,
     244,   247,   254,   255,   258,   259,   261,   264,   265,   272,
     274,   276,   278,   280,   282,   284,   286,   288,   290,   292,
     293,   295,   298,   307,   308,   311,   313,   314,   316,   317,
     320,   325,   329,   331,   335,   336,   342,   344,   346,   348,
     350,   351,   353,   354,   356,   361,   366,   368,   370,   372,
     374,   376,   378,   380,   382,   384,   386,   388,   390,   394,
     396,   400,   402,   406,   408,   412,   416,   418,   422,   426,
     428,   432,   436,   440,   443,   445,   447,   449,   451,   453,
     455,   459,   461,   463,   465,   467,   469,   471,   472,   480,
     482,   484,   487,   491,   493,   497,   499,   502,   506,   508,
     510,   512,   514,   516,   518,   520,   522,   524,   526,   528,
     530,   532,   539,   544,   546,   548,   551,   558,   560,   562,
     564,   566,   568,   570,   572,   574,   577,   579,   581,   583,
     586,   589,   593,   595,   597,   599,   601,   603,   605,   607,
     612,   614,   619,   621,   623,   627,   629,   631,   633,   635,
     637,   641,   644,   646,   649,   653,   656,   659,   664,   666,
     670,   672,   674,   676,   678,   679,   681,   683,   685,   687,
     689,   693,   695,   696,   698,   699,   700,   705,   707,   709,
     711,   713,   715,   717,   719,   722,   724,   726,   729,   731
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
      80,     0,    -1,    -1,    82,    -1,    -1,    82,    -1,    87,
      -1,    82,    87,    -1,    58,    -1,    -1,    58,    -1,    59,
      -1,    -1,   164,    -1,   100,    83,    -1,   122,    83,    -1,
     124,    83,    -1,    92,    83,    -1,    89,    83,    -1,   222,
      -1,   223,    -1,    86,    -1,    84,    -1,   213,    21,    -1,
      -1,    88,   205,    90,    60,    81,    61,   206,    -1,   205,
      -1,    23,    -1,    38,    -1,    -1,    -1,   213,   214,    19,
      91,    93,   206,    95,    94,    60,    97,    61,   206,    -1,
     213,   214,    19,    91,   206,    -1,    -1,    62,    96,    -1,
     164,    -1,    96,    85,   164,    -1,    98,    -1,    -1,    98,
      99,    -1,   100,    83,    -1,   124,    83,    -1,   135,    83,
      -1,   128,    83,    -1,   122,    83,    -1,   222,    -1,    84,
      -1,   213,   101,    -1,   214,    39,   103,    -1,   109,    -1,
     112,    -1,   162,    -1,   214,    22,   195,    -1,    -1,   214,
      22,   195,    63,   102,    51,    -1,   104,   193,    -1,   105,
      -1,   106,    -1,   169,    -1,   170,    -1,   164,    -1,   109,
      -1,   112,    -1,   162,    -1,    -1,   203,    -1,    -1,   204,
      -1,    -1,    -1,   214,    35,   110,   108,    60,   111,   167,
      61,   206,    -1,    -1,    -1,   214,    40,   113,   108,    36,
      63,   115,    64,    60,   114,   116,    61,   206,    -1,   175,
      -1,   184,    -1,   186,    -1,   162,    -1,   164,    -1,   117,
      -1,   118,    -1,   117,   118,    -1,   120,   119,    83,    -1,
     104,   194,    -1,   121,    -1,   120,   121,    -1,     6,   151,
      62,    -1,    10,    62,    -1,   213,   123,    -1,     8,   150,
     203,    65,   151,    -1,   213,   125,    -1,    13,   204,    60,
     126,    61,   206,    -1,    -1,   126,   168,    -1,    -1,    31,
      -1,   213,   129,    -1,    -1,   214,   127,     4,   130,   131,
     197,    -1,   133,    -1,    43,    -1,   171,    -1,   106,    -1,
     169,    -1,   191,    -1,   192,    -1,   173,    -1,   164,    -1,
     132,    -1,    -1,    25,    -1,   213,   136,    -1,   214,   134,
     137,   204,   141,   206,   146,   147,    -1,    -1,   138,   133,
      -1,    43,    -1,    -1,    42,    -1,    -1,    59,    42,    -1,
      63,   142,   140,    64,    -1,    63,   139,    64,    -1,   143,
      -1,   142,    85,   143,    -1,    -1,   214,   145,   144,   131,
     195,    -1,    17,    -1,    29,    -1,    18,    -1,   131,    -1,
      -1,   148,    -1,    -1,   149,    -1,    30,    63,    96,    64,
      -1,     9,    63,   211,    64,    -1,   175,    -1,   184,    -1,
     187,    -1,   185,    -1,   186,    -1,   172,    -1,   191,    -1,
     192,    -1,   174,    -1,   164,    -1,   152,    -1,   153,    -1,
     152,    66,   153,    -1,   154,    -1,   153,    67,   154,    -1,
     155,    -1,   154,    68,   155,    -1,   156,    -1,   155,    28,
     156,    -1,   155,    27,   156,    -1,   157,    -1,   156,    69,
     157,    -1,   156,    70,   157,    -1,   158,    -1,   157,    71,
     158,    -1,   157,    72,   158,    -1,   157,    73,   158,    -1,
     159,   160,    -1,   160,    -1,    70,    -1,    69,    -1,    74,
      -1,   164,    -1,   161,    -1,    63,   151,    64,    -1,   216,
      -1,   217,    -1,   218,    -1,   219,    -1,   220,    -1,   221,
      -1,    -1,   214,    12,   163,   107,    60,   166,    61,    -1,
     165,    -1,   208,    -1,    26,   210,    -1,   165,    26,   202,
      -1,   203,    -1,   166,    85,   203,    -1,   168,    -1,   167,
     168,    -1,   104,   193,    83,    -1,   172,    -1,   175,    -1,
     184,    -1,   185,    -1,   186,    -1,   187,    -1,   188,    -1,
     189,    -1,   190,    -1,   171,    -1,   191,    -1,   192,    -1,
     173,    -1,    32,    75,   105,    59,   212,    76,    -1,    32,
      75,   105,    76,    -1,    16,    -1,    11,    -1,    20,    11,
      -1,    15,    75,   212,    59,   216,    76,    -1,    15,    -1,
     176,    -1,   180,    -1,   177,    -1,   178,    -1,   179,    -1,
      33,    -1,    20,    -1,    20,    20,    -1,   181,    -1,   182,
      -1,   183,    -1,    41,    33,    -1,    41,    20,    -1,    41,
      20,    20,    -1,     7,    -1,    44,    -1,     5,    -1,    24,
      -1,     3,    -1,    23,    -1,    38,    -1,    34,    75,   212,
      76,    -1,    34,    -1,    45,    75,   212,    76,    -1,    45,
      -1,   194,    -1,   193,    85,   194,    -1,   195,    -1,   196,
      -1,   203,    -1,   198,    -1,   195,    -1,   197,    85,   195,
      -1,   203,   199,    -1,   200,    -1,   199,   200,    -1,    77,
     212,    78,    -1,    77,    78,    -1,    49,    50,    -1,   201,
      59,    49,    50,    -1,    49,    -1,   201,    59,    49,    -1,
      52,    -1,   207,    -1,   207,    -1,   209,    -1,    -1,   202,
      -1,   202,    -1,   202,    -1,   202,    -1,   217,    -1,   211,
      85,   217,    -1,   151,    -1,    -1,    48,    -1,    -1,    -1,
      77,   215,   201,    78,    -1,    47,    -1,   224,    -1,   226,
      -1,    55,    -1,    46,    -1,    37,    -1,    14,    -1,   213,
      56,    -1,    57,    -1,   225,    -1,   224,   225,    -1,    54,
      -1,    53,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   309,   309,   310,   313,   314,   317,   318,   321,   322,
     328,   334,   335,   341,   350,   351,   352,   353,   354,   355,
     356,   357,   358,   361,   365,   364,   402,   403,   407,   416,
     439,   413,   451,   464,   465,   511,   512,   516,   519,   520,
     523,   524,   525,   526,   527,   528,   529,   532,   538,   551,
     552,   553,   554,   560,   559,   575,   578,   579,   582,   583,
     584,   587,   588,   589,   592,   596,   599,   603,   607,   608,
     606,   622,   625,   621,   638,   639,   640,   641,   642,   645,
     648,   649,   652,   656,   670,   671,   674,   675,   678,   684,
     691,   697,   702,   703,   706,   707,   710,   719,   716,   735,
     736,   743,   744,   747,   748,   749,   750,   751,   752,   758,
     759,   762,   768,   780,   780,   782,   785,   786,   789,   790,
     793,   800,   806,   807,   813,   811,   821,   822,   823,   824,
     830,   831,   834,   835,   838,   843,   848,   849,   850,   851,
     852,   853,   854,   855,   856,   857,   860,   863,   864,   867,
     868,   871,   872,   875,   876,   877,   880,   881,   882,   885,
     886,   887,   888,   891,   892,   895,   896,   897,   900,   914,
     915,   918,   919,   920,   921,   922,   923,   927,   926,   936,
     944,   945,   946,   972,   973,   977,   978,   981,   996,   997,
     998,   999,  1000,  1001,  1002,  1003,  1004,  1007,  1008,  1009,
    1010,  1013,  1016,  1021,  1022,  1023,  1026,  1031,  1034,  1035,
    1038,  1039,  1040,  1043,  1046,  1049,  1052,  1053,  1054,  1057,
    1060,  1063,  1066,  1069,  1072,  1075,  1078,  1081,  1084,  1087,
    1090,  1093,  1096,  1099,  1100,  1104,  1105,  1108,  1111,  1114,
    1115,  1119,  1134,  1135,  1139,  1140,  1143,  1148,  1154,  1158,
    1165,  1168,  1176,  1189,  1206,  1218,  1256,  1275,  1305,  1324,
    1325,  1329,  1368,  1369,  1375,  1376,  1376,  1388,  1391,  1394,
    1397,  1400,  1403,  1404,  1407,  1413,  1418,  1419,  1427,  1434
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "TOK_ANY", "TOK_ATTRIBUTE",
  "TOK_BOOLEAN", "TOK_CASE", "TOK_CHAR", "TOK_CONST", "TOK_CONTEXT",
  "TOK_DEFAULT", "TOK_DOUBLE", "TOK_ENUM", "TOK_EXCEPTION", "TOK_FALSE",
  "TOK_FIXED", "TOK_FLOAT", "TOK_IN", "TOK_INOUT", "TOK_INTERFACE",
  "TOK_LONG", "TOK_MODULE", "TOK_NATIVE", "TOK_OBJECT", "TOK_OCTET",
  "TOK_ONEWAY", "TOK_OP_SCOPE", "TOK_OP_SHL", "TOK_OP_SHR", "TOK_OUT",
  "TOK_RAISES", "TOK_READONLY", "TOK_SEQUENCE", "TOK_SHORT", "TOK_STRING",
  "TOK_STRUCT", "TOK_SWITCH", "TOK_TRUE", "TOK_TYPECODE", "TOK_TYPEDEF",
  "TOK_UNION", "TOK_UNSIGNED", "TOK_VARARGS", "TOK_VOID", "TOK_WCHAR",
  "TOK_WSTRING", "TOK_FLOATP", "TOK_INTEGER", "TOK_DECLSPEC",
  "TOK_PROP_KEY", "TOK_PROP_VALUE", "TOK_NATIVE_TYPE", "TOK_IDENT",
  "TOK_SQSTRING", "TOK_DQSTRING", "TOK_FIXEDP", "TOK_CODEFRAG",
  "TOK_SRCFILE", "';'", "','", "'{'", "'}'", "':'", "'('", "')'", "'='",
  "'|'", "'^'", "'&'", "'+'", "'-'", "'*'", "'/'", "'%'", "'~'", "'<'",
  "'>'", "'['", "']'", "$accept", "specification", "z_definition_list",
  "definition_list", "check_semicolon", "useless_semicolon", "check_comma",
  "illegal_ident", "definition", "module_declspec", "module", "$@1",
  "interface_catch_ident", "interface", "$@2", "$@3", "z_inheritance",
  "scoped_name_list", "interface_body", "export_list", "export",
  "type_dcl", "type_dcl_def", "$@4", "type_declarator", "type_spec",
  "simple_type_spec", "constr_type_spec", "z_new_ident_catch",
  "z_new_scope_catch", "struct_type", "@5", "@6", "union_type", "@7", "@8",
  "switch_type_spec", "switch_body", "case_stmt_list", "case_stmt",
  "element_spec", "case_label_list", "case_label", "const_dcl",
  "const_dcl_def", "except_dcl", "except_dcl_def", "member_zlist",
  "is_readonly", "attr_dcl", "attr_dcl_def", "@9", "param_type_spec",
  "op_param_type_spec_illegal", "op_param_type_spec", "is_oneway",
  "op_dcl", "op_dcl_def", "op_type_spec", "@10", "is_varargs",
  "is_cvarargs", "parameter_dcls", "param_dcl_list", "param_dcl", "@11",
  "param_attribute", "is_raises_expr", "is_context_expr", "raises_expr",
  "context_expr", "const_type", "const_exp", "or_expr", "xor_expr",
  "and_expr", "shift_expr", "add_expr", "mult_expr", "unary_expr",
  "unary_op", "primary_expr", "literal", "enum_type", "@12", "scoped_name",
  "ns_scoped_name", "enumerator_list", "member_list", "member",
  "base_type_spec", "template_type_spec", "sequence_type",
  "floating_pt_type", "fixed_pt_type", "fixed_pt_const_type",
  "integer_type", "signed_int", "signed_short_int", "signed_long_int",
  "signed_longlong_int", "unsigned_int", "unsigned_short_int",
  "unsigned_long_int", "unsigned_longlong_int", "char_type",
  "wide_char_type", "boolean_type", "octet_type", "any_type",
  "object_type", "typecode_type", "string_type", "wide_string_type",
  "declarator_list", "declarator", "simple_declarator",
  "complex_declarator", "simple_declarator_list", "array_declarator",
  "fixed_array_size_list", "fixed_array_size", "prop_hash", "ident",
  "new_ident", "new_scope", "new_or_prev_scope", "pop_scope",
  "ns_new_ident", "ns_prev_ident", "cur_ns_new_or_prev_ident",
  "ns_global_ident", "string_lit_list", "positive_int_const", "z_declspec",
  "z_props", "$@13", "integer_lit", "string_lit", "char_lit",
  "fixed_pt_lit", "floating_pt_lit", "boolean_lit", "codefrag", "srcfile",
  "dqstring_cat", "dqstring", "sqstring", 0
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
     305,   306,   307,   308,   309,   310,   311,   312,    59,    44,
     123,   125,    58,    40,    41,    61,   124,    94,    38,    43,
      45,    42,    47,    37,   126,    60,    62,    91,    93
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    79,    80,    80,    81,    81,    82,    82,    83,    83,
      84,    85,    85,    86,    87,    87,    87,    87,    87,    87,
      87,    87,    87,    88,    90,    89,    91,    91,    91,    93,
      94,    92,    92,    95,    95,    96,    96,    97,    98,    98,
      99,    99,    99,    99,    99,    99,    99,   100,   101,   101,
     101,   101,   101,   102,   101,   103,   104,   104,   105,   105,
     105,   106,   106,   106,   107,   107,   108,   108,   110,   111,
     109,   113,   114,   112,   115,   115,   115,   115,   115,   116,
     117,   117,   118,   119,   120,   120,   121,   121,   122,   123,
     124,   125,   126,   126,   127,   127,   128,   130,   129,   131,
     131,   132,   132,   133,   133,   133,   133,   133,   133,   134,
     134,   135,   136,   138,   137,   137,   139,   139,   140,   140,
     141,   141,   142,   142,   144,   143,   145,   145,   145,   145,
     146,   146,   147,   147,   148,   149,   150,   150,   150,   150,
     150,   150,   150,   150,   150,   150,   151,   152,   152,   153,
     153,   154,   154,   155,   155,   155,   156,   156,   156,   157,
     157,   157,   157,   158,   158,   159,   159,   159,   160,   160,
     160,   161,   161,   161,   161,   161,   161,   163,   162,   164,
     165,   165,   165,   166,   166,   167,   167,   168,   169,   169,
     169,   169,   169,   169,   169,   169,   169,   170,   170,   170,
     170,   171,   171,   172,   172,   172,   173,   174,   175,   175,
     176,   176,   176,   177,   178,   179,   180,   180,   180,   181,
     182,   183,   184,   185,   186,   187,   188,   189,   190,   191,
     191,   192,   192,   193,   193,   194,   194,   195,   196,   197,
     197,   198,   199,   199,   200,   200,   201,   201,   201,   201,
     202,   203,   204,   205,   206,   207,   208,   209,   210,   211,
     211,   212,   213,   213,   214,   215,   214,   216,   217,   218,
     219,   220,   221,   221,   222,   223,   224,   224,   225,   226
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     1,     0,     1,     1,     2,     1,     0,
       1,     1,     0,     1,     2,     2,     2,     2,     2,     1,
       1,     1,     1,     2,     0,     7,     1,     1,     1,     0,
       0,    12,     5,     0,     2,     1,     3,     1,     0,     2,
       2,     2,     2,     2,     2,     1,     1,     2,     3,     1,
       1,     1,     3,     0,     6,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     0,     1,     0,     1,     0,     0,
       9,     0,     0,    13,     1,     1,     1,     1,     1,     1,
       1,     2,     3,     2,     1,     2,     3,     2,     2,     5,
       2,     6,     0,     2,     0,     1,     2,     0,     6,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     0,
       1,     2,     8,     0,     2,     1,     0,     1,     0,     2,
       4,     3,     1,     3,     0,     5,     1,     1,     1,     1,
       0,     1,     0,     1,     4,     4,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     3,     1,
       3,     1,     3,     1,     3,     3,     1,     3,     3,     1,
       3,     3,     3,     2,     1,     1,     1,     1,     1,     1,
       3,     1,     1,     1,     1,     1,     1,     0,     7,     1,
       1,     2,     3,     1,     3,     1,     2,     3,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     6,     4,     1,     1,     2,     6,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     1,     1,     1,     2,
       2,     3,     1,     1,     1,     1,     1,     1,     1,     4,
       1,     4,     1,     1,     3,     1,     1,     1,     1,     1,
       3,     2,     1,     2,     3,     2,     2,     4,     1,     3,
       1,     1,     1,     1,     0,     1,     1,     1,     1,     1,
       3,     1,     0,     1,     0,     0,     4,     1,     1,     1,
       1,     1,     1,     1,     2,     1,     1,     2,     1,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
     262,     0,   263,   250,   275,    10,     0,   262,    22,    21,
       6,     0,     9,     9,     9,     9,     9,    13,   179,   256,
     180,   264,    19,    20,   258,   181,     1,     7,   257,    24,
     253,     8,    18,    17,    14,    15,    16,     0,     0,     0,
      23,   274,   265,    47,    49,    50,    88,    90,    51,     0,
       0,   182,   224,   222,   204,   207,   203,   214,   225,   213,
     230,     0,   223,   232,     0,   145,   141,   144,   136,   208,
     210,   211,   212,   209,   216,   217,   218,   137,   139,   140,
     138,   142,   143,   255,     0,   252,     0,   177,     0,     0,
      68,   264,    71,   262,   205,   215,     0,   220,   219,     0,
       0,   251,    92,   248,     0,    64,    27,    28,   254,    26,
      52,   237,    66,   226,     0,   227,     0,   228,    48,     0,
      56,    57,    61,    62,    63,    60,    58,    59,   197,   188,
     200,   189,   190,   191,   192,   193,   194,   195,   196,   198,
     199,     0,    66,     0,   262,   273,   272,   271,   267,   279,
     278,   270,     0,   166,   165,   167,   261,   146,   147,   149,
     151,   153,   156,   159,     0,   164,   169,   168,     0,   171,
     172,   173,   174,   175,   176,   268,   276,   269,   221,     0,
       0,   264,   246,     0,   266,     0,    65,   254,    32,    53,
       0,    67,     0,     0,    55,   233,   235,   236,   238,   237,
       0,   254,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   163,   229,   277,   231,    89,   254,     0,
      93,   249,     0,    33,     0,    69,     0,     0,    11,     0,
       0,   241,   242,     0,    25,   170,   148,   150,   152,   155,
     154,   157,   158,   160,   161,   162,    91,     9,   247,    12,
     183,     0,    30,    54,   264,     0,     0,   202,   234,   245,
       0,   243,   264,   187,   178,     0,    12,    35,     0,   264,
     185,     0,     0,   244,   214,     0,    77,    78,    74,    75,
      76,     0,   184,     0,    38,   254,   186,   206,   201,     0,
      36,     0,   262,    70,    72,   254,    46,    39,     9,     9,
       9,     9,     9,   264,    45,     0,    31,    40,    44,    41,
      43,    42,    96,   111,   109,     0,     0,     0,    79,    80,
     264,    84,   110,    95,     0,   113,     0,    87,   254,    81,
       0,     9,    85,    97,   115,     0,   264,    86,    73,    83,
      82,   264,     0,   102,   108,   114,   107,   103,   101,   106,
     104,   105,   100,     0,    99,   264,   254,   239,    98,   117,
       0,    12,   122,   264,   130,     0,   121,    11,   264,     0,
     126,   128,   127,   129,   124,     0,   132,   131,   240,   119,
     123,   120,   264,     0,     0,   112,   133,     0,    12,     0,
     125,   134,    12,   259,   135,     0,   260
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     6,   143,     7,    32,     8,   229,     9,    10,    11,
      12,    50,   108,    13,   187,   268,   252,   266,   291,   292,
     297,    14,    43,   224,   118,   219,   120,   121,   185,   190,
     122,   112,   254,   123,   142,   305,   275,   317,   318,   319,
     331,   320,   321,    15,    46,    16,    47,   181,   324,   301,
     312,   341,   353,   344,   354,   325,   302,   313,   335,   336,
     360,   369,   356,   361,   362,   382,   374,   376,   385,   377,
     386,    64,   156,   157,   158,   159,   160,   161,   162,   163,
     164,   165,   166,   124,   105,   167,    18,   249,   269,   220,
     126,   127,   128,   129,   130,    67,   131,    69,    70,    71,
      72,    73,    74,    75,    76,   132,   133,   134,   135,   136,
     137,   138,   139,   140,   194,   195,   196,   197,   358,   198,
     231,   232,   104,    19,   111,   191,    29,   188,   101,    20,
      30,    25,   392,   168,    21,   141,    86,   169,   170,   171,
     172,   173,   174,    22,    23,   175,   176,   177
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -302
static const yytype_int16 yypact[] =
{
      57,    -7,  -302,  -302,  -302,  -302,    24,   223,  -302,  -302,
    -302,    -7,    -3,    -3,    -3,    -3,    -3,  -302,    46,  -302,
    -302,    29,  -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,
    -302,  -302,  -302,  -302,  -302,  -302,  -302,    -7,   643,    -7,
    -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,   320,
      32,  -302,  -302,  -302,  -302,  -302,  -302,   124,  -302,  -302,
      48,    69,  -302,    61,    -7,  -302,  -302,  -302,  -302,  -302,
    -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,
    -302,  -302,  -302,  -302,    47,  -302,    90,  -302,    18,    -7,
    -302,   562,  -302,   376,  -302,  -302,   183,   130,  -302,   183,
     105,  -302,  -302,   133,     6,    -7,  -302,  -302,    15,  -302,
     109,  -302,    -7,  -302,   125,  -302,   140,  -302,  -302,    -7,
    -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,
    -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,
    -302,    36,    -7,   156,   518,  -302,  -302,  -302,  -302,  -302,
    -302,  -302,   183,  -302,  -302,  -302,  -302,   153,   146,   154,
     152,   119,   123,  -302,   319,  -302,  -302,  -302,   148,  -302,
    -302,  -302,  -302,  -302,  -302,   173,  -302,  -302,  -302,   157,
     183,   420,  -302,   185,  -302,   184,  -302,  -302,  -302,  -302,
     187,  -302,   183,   608,    38,  -302,  -302,  -302,  -302,   171,
     214,  -302,   190,   183,   183,   183,   183,   183,   183,   183,
     183,   183,   183,  -302,  -302,  -302,  -302,  -302,  -302,    -7,
    -302,   201,    -7,   193,   207,  -302,   202,   -15,  -302,    -7,
     138,   171,  -302,   199,  -302,  -302,   146,   154,   152,   119,
     119,   123,   123,  -302,  -302,  -302,  -302,   147,  -302,    34,
    -302,     8,  -302,  -302,   562,   216,   183,  -302,  -302,  -302,
     191,  -302,    26,  -302,  -302,    -7,   151,  -302,   205,   468,
    -302,   192,   194,  -302,   252,   209,  -302,  -302,  -302,  -302,
    -302,   264,  -302,     8,  -302,  -302,  -302,  -302,  -302,   217,
    -302,   221,    82,  -302,  -302,  -302,  -302,  -302,    -3,    -3,
      -3,    -3,    -3,    17,  -302,   163,  -302,  -302,  -302,  -302,
    -302,  -302,  -302,  -302,   318,   183,   222,   224,   163,  -302,
     122,  -302,  -302,  -302,   283,   248,   230,  -302,  -302,  -302,
      -7,    -3,  -302,  -302,  -302,    -7,   562,  -302,  -302,  -302,
    -302,   516,   231,  -302,  -302,  -302,  -302,  -302,  -302,  -302,
    -302,  -302,  -302,    -7,  -302,    44,  -302,  -302,    38,  -302,
     229,    58,  -302,   374,   265,    -7,  -302,   254,   220,   234,
    -302,  -302,  -302,  -302,  -302,   236,   293,  -302,  -302,  -302,
    -302,  -302,   516,     8,   240,  -302,  -302,    -7,    98,   173,
    -302,  -302,   117,  -302,  -302,   173,  -302
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -302,  -302,  -302,   215,    13,    25,  -227,  -302,    14,  -302,
    -302,  -302,  -302,  -302,  -302,  -302,  -302,   -73,  -302,  -302,
    -302,    27,  -302,  -302,  -302,   -77,   127,  -301,  -302,   174,
     -14,  -302,  -302,   -13,  -302,  -302,  -302,  -302,  -302,     0,
    -302,  -302,     1,    31,  -302,    35,  -302,  -302,  -302,  -302,
    -302,  -302,  -263,  -302,   -12,  -302,  -302,  -302,  -302,  -302,
    -302,  -302,  -302,  -302,   -42,  -302,  -302,  -302,  -302,  -302,
    -302,  -302,  -133,  -302,   134,   132,   149,    19,    23,    -8,
    -302,   197,  -302,   -17,  -302,     5,  -302,  -302,  -302,  -203,
    -283,  -302,  -272,   300,  -262,  -302,   -23,  -302,  -302,  -302,
    -302,  -302,  -302,  -302,  -302,   -22,   313,   -21,   324,  -302,
    -302,  -302,   -35,   -32,   144,  -206,   -87,  -302,  -302,  -302,
    -302,   136,  -302,    -1,   -51,   -28,   281,  -169,   -30,  -302,
    -302,  -302,  -302,   -79,    78,   -20,  -302,   120,  -242,  -302,
    -302,  -302,  -302,    84,  -302,  -302,   203,  -302
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -119
static const yytype_int16 yytable[] =
{
      24,    49,   110,    81,    48,    17,    82,    44,    45,    85,
      28,    84,    17,   100,   119,    68,    77,    79,   223,   202,
     179,    27,   265,   258,    26,    38,    33,    34,    35,    36,
      39,    52,   234,    53,     1,   343,    51,    38,    83,   283,
     343,   106,    39,    65,   256,     3,   274,   217,    87,   246,
      40,   270,     1,   347,   186,    31,   107,    -2,   347,    59,
       3,   257,   343,    83,   348,   183,   286,    61,   199,   348,
       3,    90,    37,    41,   349,   -29,    92,   -29,     3,   349,
     347,   343,    85,     1,   184,    41,   359,    28,    83,    97,
     -12,   348,    93,   228,    42,   264,   125,   228,    17,   347,
     373,   349,    98,    42,    83,     2,    42,   102,  -116,     3,
     348,    83,    85,   226,     4,     5,   293,   367,    83,   387,
     349,    42,  -118,    96,   339,   113,   306,    52,   315,    53,
       2,   365,   316,    54,   368,    94,    99,   114,    56,   103,
       5,    83,    57,   -37,    95,   115,    58,   393,     1,    17,
     178,   260,   145,   396,   116,    59,    60,   228,    27,   338,
     117,   283,   391,    61,     1,   395,    62,    63,   199,   315,
     180,   250,   189,   316,     3,   146,   228,   272,   199,   206,
     207,   394,   326,   182,   147,   148,   125,   364,   208,   209,
       3,   149,   150,   151,   210,   211,   212,   145,   125,    42,
     192,   152,   243,   244,   245,    31,   228,   153,   154,     1,
     228,   -34,   155,   204,   282,   193,   259,   201,    83,   203,
     146,    83,   205,    -3,   214,   239,   240,   150,    83,   147,
     148,   241,   242,   216,   221,     3,   149,   150,   151,   278,
     279,   280,   281,   330,   222,   276,   152,   225,   230,     1,
     233,   248,   153,   154,   235,   251,   267,   155,   253,   125,
     263,   255,   262,   148,    83,   284,   357,   277,   287,   273,
     288,     2,    95,   289,   125,     3,    87,   294,   378,   199,
       4,     5,   295,   314,   327,   328,    48,   333,   290,    44,
      45,   334,   337,   366,   355,   375,   379,    42,   381,   383,
     390,   350,   384,   389,   351,    85,   350,   342,   144,   351,
     388,   307,   308,   309,   310,   311,   200,   296,   329,   298,
     227,   332,   -94,   299,   345,   125,   380,   300,   350,    83,
      87,   351,    87,   145,    83,   363,   237,   236,    66,    88,
      89,   346,    89,   322,   340,     1,   346,   350,   363,   323,
     351,    78,    83,    90,   238,    90,   146,    91,    92,    91,
      92,   213,    80,   247,    83,   147,   148,   261,   346,   109,
     303,     3,   149,   150,   151,   271,   304,   113,   215,    52,
       0,    53,   152,     0,     0,    54,    83,   346,   267,   114,
      56,   370,   371,     0,    57,     0,     0,   115,    58,     0,
       1,     0,     1,   372,     0,     0,   116,    59,    60,     0,
       0,     0,   117,     0,     0,    61,     0,   352,    62,    63,
       0,     0,     0,   113,     2,    52,     3,    53,     3,     0,
       0,    54,     0,     4,     5,   114,    56,    -4,     0,     0,
      57,     0,     0,   115,    58,     0,     1,     0,     0,     0,
       0,    42,   116,    59,    60,     0,     0,     0,   117,     0,
       0,    61,     0,     0,    62,    63,     0,     0,     0,     0,
       0,   113,     3,    52,     0,    53,     0,     0,     0,    54,
       0,   218,     0,   114,    56,     0,     0,     0,    57,     0,
       0,   115,    58,     0,     1,     0,     0,    42,     0,     0,
     116,    59,    60,     0,     0,     0,   117,     0,     0,    61,
       0,     0,    62,    63,     0,     0,     0,     0,     0,   113,
       3,    52,     0,    53,     0,     0,     0,    54,     0,   285,
       0,   114,    56,     0,     0,     0,    57,     0,     0,   115,
      58,     0,     1,     0,     1,    42,     0,     0,   116,    59,
      60,     0,     0,     0,   117,     0,     0,    61,     0,   352,
      62,    63,     0,     0,     0,   113,     2,    52,     3,    53,
       3,     0,     0,    54,     0,     4,     5,   114,    56,    -5,
       0,     0,    57,     0,     0,   115,    58,     0,     1,     0,
       0,     0,     0,    42,   116,    59,    60,     0,     0,     0,
     117,     0,     0,    61,     0,     0,    62,    63,     0,     0,
       0,   113,     0,    52,     3,    53,     0,     0,     0,    54,
       0,     0,     0,   114,    56,     0,     0,     0,    57,     0,
       0,   115,    58,     0,     1,     0,     0,     0,     0,    42,
     116,    59,    60,     0,     0,     0,   117,     0,    52,    61,
      53,     0,    62,    63,    54,     0,     0,     0,    55,    56,
       3,     0,     0,    57,     0,     0,     0,    58,     0,     1,
       0,     0,     0,     0,     0,     0,    59,    60,     0,     0,
       0,     0,     0,     0,    61,     0,     0,    62,    63,     0,
       0,     0,     0,     0,     0,     3
};

static const yytype_int16 yycheck[] =
{
       1,    21,    89,    38,    21,     0,    38,    21,    21,    39,
      11,    39,     7,    64,    91,    38,    38,    38,   187,   152,
      99,     7,   249,   229,     0,     8,    13,    14,    15,    16,
      13,     5,   201,     7,    26,   336,    37,     8,    39,   266,
     341,    23,    13,    38,    59,    52,    20,   180,    12,   218,
      21,   254,    26,   336,   105,    58,    38,     0,   341,    33,
      52,    76,   363,    64,   336,    59,   269,    41,   119,   341,
      52,    35,    26,    56,   336,    60,    40,    62,    52,   341,
     363,   382,   112,    26,    78,    56,    42,    88,    89,    20,
      52,   363,    60,    59,    77,    61,    91,    59,    93,   382,
     363,   363,    33,    77,   105,    48,    77,    60,    64,    52,
     382,   112,   142,   192,    57,    58,   285,    59,   119,   382,
     382,    77,    64,    75,   330,     3,   295,     5,     6,     7,
      48,   358,    10,    11,   361,    11,    75,    15,    16,    49,
      58,   142,    20,    61,    20,    23,    24,   389,    26,   144,
      20,   230,    14,   395,    32,    33,    34,    59,   144,   328,
      38,   388,    64,    41,    26,   392,    44,    45,   219,     6,
      65,   222,    63,    10,    52,    37,    59,   256,   229,    27,
      28,    64,   315,    50,    46,    47,   181,   356,    69,    70,
      52,    53,    54,    55,    71,    72,    73,    14,   193,    77,
      75,    63,   210,   211,   212,    58,    59,    69,    70,    26,
      59,    60,    74,    67,   265,    75,    78,    61,   219,    66,
      37,   222,    68,     0,    76,   206,   207,    54,   229,    46,
      47,   208,   209,    76,    49,    52,    53,    54,    55,   262,
     262,   262,   262,   320,    60,   262,    63,    60,    77,    26,
      36,    50,    69,    70,    64,    62,   251,    74,    51,   254,
     247,    59,    63,    47,   265,    60,   353,   262,    76,    78,
      76,    48,    20,    64,   269,    52,    12,    60,   365,   330,
      57,    58,    61,   303,    62,    61,   303,     4,   283,   303,
     303,    43,    62,    64,    63,    30,    42,    77,    64,    63,
     387,   336,     9,    63,   336,   335,   341,   335,    93,   341,
     383,   298,   299,   300,   301,   302,   142,   292,   318,   292,
     193,   320,     4,   292,   336,   320,   368,   292,   363,   330,
      12,   363,    12,    14,   335,   355,   204,   203,    38,    19,
      22,   336,    22,    25,   331,    26,   341,   382,   368,    31,
     382,    38,   353,    35,   205,    35,    37,    39,    40,    39,
      40,   164,    38,   219,   365,    46,    47,   231,   363,    88,
     292,    52,    53,    54,    55,   255,   292,     3,   175,     5,
      -1,     7,    63,    -1,    -1,    11,   387,   382,   383,    15,
      16,    17,    18,    -1,    20,    -1,    -1,    23,    24,    -1,
      26,    -1,    26,    29,    -1,    -1,    32,    33,    34,    -1,
      -1,    -1,    38,    -1,    -1,    41,    -1,    43,    44,    45,
      -1,    -1,    -1,     3,    48,     5,    52,     7,    52,    -1,
      -1,    11,    -1,    57,    58,    15,    16,    61,    -1,    -1,
      20,    -1,    -1,    23,    24,    -1,    26,    -1,    -1,    -1,
      -1,    77,    32,    33,    34,    -1,    -1,    -1,    38,    -1,
      -1,    41,    -1,    -1,    44,    45,    -1,    -1,    -1,    -1,
      -1,     3,    52,     5,    -1,     7,    -1,    -1,    -1,    11,
      -1,    61,    -1,    15,    16,    -1,    -1,    -1,    20,    -1,
      -1,    23,    24,    -1,    26,    -1,    -1,    77,    -1,    -1,
      32,    33,    34,    -1,    -1,    -1,    38,    -1,    -1,    41,
      -1,    -1,    44,    45,    -1,    -1,    -1,    -1,    -1,     3,
      52,     5,    -1,     7,    -1,    -1,    -1,    11,    -1,    61,
      -1,    15,    16,    -1,    -1,    -1,    20,    -1,    -1,    23,
      24,    -1,    26,    -1,    26,    77,    -1,    -1,    32,    33,
      34,    -1,    -1,    -1,    38,    -1,    -1,    41,    -1,    43,
      44,    45,    -1,    -1,    -1,     3,    48,     5,    52,     7,
      52,    -1,    -1,    11,    -1,    57,    58,    15,    16,    61,
      -1,    -1,    20,    -1,    -1,    23,    24,    -1,    26,    -1,
      -1,    -1,    -1,    77,    32,    33,    34,    -1,    -1,    -1,
      38,    -1,    -1,    41,    -1,    -1,    44,    45,    -1,    -1,
      -1,     3,    -1,     5,    52,     7,    -1,    -1,    -1,    11,
      -1,    -1,    -1,    15,    16,    -1,    -1,    -1,    20,    -1,
      -1,    23,    24,    -1,    26,    -1,    -1,    -1,    -1,    77,
      32,    33,    34,    -1,    -1,    -1,    38,    -1,     5,    41,
       7,    -1,    44,    45,    11,    -1,    -1,    -1,    15,    16,
      52,    -1,    -1,    20,    -1,    -1,    -1,    24,    -1,    26,
      -1,    -1,    -1,    -1,    -1,    -1,    33,    34,    -1,    -1,
      -1,    -1,    -1,    -1,    41,    -1,    -1,    44,    45,    -1,
      -1,    -1,    -1,    -1,    -1,    52
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    26,    48,    52,    57,    58,    80,    82,    84,    86,
      87,    88,    89,    92,   100,   122,   124,   164,   165,   202,
     208,   213,   222,   223,   202,   210,     0,    87,   202,   205,
     209,    58,    83,    83,    83,    83,    83,    26,     8,    13,
      21,    56,    77,   101,   109,   112,   123,   125,   162,   214,
      90,   202,     5,     7,    11,    15,    16,    20,    24,    33,
      34,    41,    44,    45,   150,   164,   172,   174,   175,   176,
     177,   178,   179,   180,   181,   182,   183,   184,   185,   186,
     187,   191,   192,   202,   204,   207,   215,    12,    19,    22,
      35,    39,    40,    60,    11,    20,    75,    20,    33,    75,
     203,   207,    60,    49,   201,   163,    23,    38,    91,   205,
     195,   203,   110,     3,    15,    23,    32,    38,   103,   104,
     105,   106,   109,   112,   162,   164,   169,   170,   171,   172,
     173,   175,   184,   185,   186,   187,   188,   189,   190,   191,
     192,   214,   113,    81,    82,    14,    37,    46,    47,    53,
      54,    55,    63,    69,    70,    74,   151,   152,   153,   154,
     155,   156,   157,   158,   159,   160,   161,   164,   212,   216,
     217,   218,   219,   220,   221,   224,   225,   226,    20,   212,
      65,   126,    50,    59,    78,   107,   203,    93,   206,    63,
     108,   204,    75,    75,   193,   194,   195,   196,   198,   203,
     108,    61,   151,    66,    67,    68,    27,    28,    69,    70,
      71,    72,    73,   160,    76,   225,    76,   151,    61,   104,
     168,    49,    60,   206,   102,    60,   212,   105,    59,    85,
      77,   199,   200,    36,   206,    64,   153,   154,   155,   156,
     156,   157,   157,   158,   158,   158,   206,   193,    50,   166,
     203,    62,    95,    51,   111,    59,    59,    76,   194,    78,
     212,   200,    63,    83,    61,    85,    96,   164,    94,   167,
     168,   216,   212,    78,    20,   115,   162,   164,   175,   184,
     186,   214,   203,    85,    60,    61,   168,    76,    76,    64,
     164,    97,    98,   206,    60,    61,    84,    99,   100,   122,
     124,   128,   135,   213,   222,   114,   206,    83,    83,    83,
      83,    83,   129,   136,   214,     6,    10,   116,   117,   118,
     120,   121,    25,    31,   127,   134,   151,    62,    61,   118,
     104,   119,   121,     4,    43,   137,   138,    62,   206,   194,
      83,   130,   204,   106,   132,   133,   164,   169,   171,   173,
     191,   192,    43,   131,   133,    63,   141,   195,   197,    42,
     139,   142,   143,   214,   206,    85,    64,    59,    85,   140,
      17,    18,    29,   131,   145,    30,   146,   148,   195,    42,
     143,    64,   144,    63,     9,   147,   149,   131,    96,    63,
     195,    64,   211,   217,    64,    85,   217
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
#line 309 "./parser.y"
    { yyerror ("Empty file"); YYABORT; }
    break;

  case 3:

/* Line 1455 of yacc.c  */
#line 310 "./parser.y"
    { __IDL_root = (yyvsp[(1) - (1)].tree); }
    break;

  case 4:

/* Line 1455 of yacc.c  */
#line 313 "./parser.y"
    { (yyval.tree) = NULL; }
    break;

  case 6:

/* Line 1455 of yacc.c  */
#line 317 "./parser.y"
    { (yyval.tree) = list_start ((yyvsp[(1) - (1)].tree), TRUE); }
    break;

  case 7:

/* Line 1455 of yacc.c  */
#line 318 "./parser.y"
    { (yyval.tree) = list_chain ((yyvsp[(1) - (2)].tree), (yyvsp[(2) - (2)].tree), TRUE); }
    break;

  case 9:

/* Line 1455 of yacc.c  */
#line 322 "./parser.y"
    {
	if (do_token_error ((yyvsp[(0) - (0)].tree), "Missing semicolon after", TRUE))
		YYABORT;
}
    break;

  case 10:

/* Line 1455 of yacc.c  */
#line 328 "./parser.y"
    {
	yyerror ("Dangling semicolon has no effect");
	(yyval.tree) = NULL;
}
    break;

  case 12:

/* Line 1455 of yacc.c  */
#line 335 "./parser.y"
    {
	if (do_token_error ((yyvsp[(0) - (0)].tree), "Missing comma after", TRUE))
		YYABORT;
}
    break;

  case 13:

/* Line 1455 of yacc.c  */
#line 341 "./parser.y"
    {
	if (IDL_NODE_UP ((yyvsp[(1) - (1)].tree)))
		do_token_error (IDL_NODE_UP ((yyvsp[(1) - (1)].tree)), "Illegal context for", FALSE);
	else
		yyerror ("Illegal context for identifier");
	YYABORT;
}
    break;

  case 24:

/* Line 1455 of yacc.c  */
#line 365 "./parser.y"
    {
	if (IDL_NODE_UP ((yyvsp[(2) - (2)].tree)) != NULL &&
	    IDL_NODE_TYPE (IDL_NODE_UP ((yyvsp[(2) - (2)].tree))) != IDLN_MODULE) {
		yyerror ("Module definition conflicts");
		do_token_error (IDL_NODE_UP ((yyvsp[(2) - (2)].tree)), "with", FALSE);
		YYABORT;
	}
}
    break;

  case 25:

/* Line 1455 of yacc.c  */
#line 374 "./parser.y"
    {
	IDL_tree module;

	if ((yyvsp[(5) - (7)].tree) == NULL) {
		yyerrorv ("Empty module declaration `%s' is not legal IDL",
			  IDL_IDENT ((yyvsp[(2) - (7)].tree)).str);
		module = NULL;
	}

	if (__IDL_flags & IDLF_COMBINE_REOPENED_MODULES) {
		if (IDL_NODE_UP ((yyvsp[(2) - (7)].tree)) == NULL)
			module = IDL_module_new ((yyvsp[(2) - (7)].tree), (yyvsp[(5) - (7)].tree));
		else {
			module = IDL_NODE_UP ((yyvsp[(2) - (7)].tree));
			IDL_MODULE (module).definition_list =
				IDL_list_concat (IDL_MODULE (module).definition_list, (yyvsp[(5) - (7)].tree));
			module = NULL;
		}
	} else
		module = IDL_module_new ((yyvsp[(2) - (7)].tree), (yyvsp[(5) - (7)].tree));

	(yyval.tree) = module;
	if ((yyval.tree)) assign_declspec ((yyval.tree), (yyvsp[(1) - (7)].declspec));
}
    break;

  case 27:

/* Line 1455 of yacc.c  */
#line 403 "./parser.y"
    {
	yyerror ("Interfaces cannot be named `Object'");
	YYABORT;
}
    break;

  case 28:

/* Line 1455 of yacc.c  */
#line 407 "./parser.y"
    {
	yyerror ("Interfaces cannot be named `TypeCode'");
	YYABORT;
}
    break;

  case 29:

/* Line 1455 of yacc.c  */
#line 416 "./parser.y"
    {
	assert ((yyvsp[(4) - (4)].tree) != NULL);
	assert (IDL_NODE_TYPE ((yyvsp[(4) - (4)].tree)) == IDLN_IDENT);
	assert (IDL_IDENT_TO_NS ((yyvsp[(4) - (4)].tree)) != NULL);
	assert (IDL_NODE_TYPE (IDL_IDENT_TO_NS ((yyvsp[(4) - (4)].tree))) == IDLN_GENTREE);

	if (IDL_NODE_UP ((yyvsp[(4) - (4)].tree)) != NULL &&
	    IDL_NODE_TYPE (IDL_NODE_UP ((yyvsp[(4) - (4)].tree))) != IDLN_INTERFACE &&
	    IDL_NODE_TYPE (IDL_NODE_UP ((yyvsp[(4) - (4)].tree))) != IDLN_FORWARD_DCL) {
		yyerrorl ("Interface definition conflicts",
			  __IDL_prev_token_line - __IDL_cur_token_line);
		do_token_error (IDL_NODE_UP ((yyvsp[(4) - (4)].tree)), "with", FALSE);
		YYABORT;
	} else if (IDL_NODE_UP ((yyvsp[(4) - (4)].tree)) != NULL &&
		   IDL_NODE_TYPE (IDL_NODE_UP ((yyvsp[(4) - (4)].tree))) != IDLN_FORWARD_DCL) {
		yyerrorv ("Cannot redeclare interface `%s'", IDL_IDENT ((yyvsp[(4) - (4)].tree)).str);
		IDL_tree_error ((yyvsp[(4) - (4)].tree), "Previous declaration of interface `%s'", IDL_IDENT ((yyvsp[(4) - (4)].tree)).str);
		YYABORT;
	} else if (IDL_NODE_UP ((yyvsp[(4) - (4)].tree)) != NULL &&
		   IDL_NODE_TYPE (IDL_NODE_UP ((yyvsp[(4) - (4)].tree))) == IDLN_FORWARD_DCL)
		__IDL_assign_this_location ((yyvsp[(4) - (4)].tree), __IDL_cur_filename, __IDL_cur_line);
}
    break;

  case 30:

/* Line 1455 of yacc.c  */
#line 439 "./parser.y"
    {
	IDL_GENTREE (IDL_IDENT_TO_NS ((yyvsp[(4) - (7)].tree)))._import = (yyvsp[(7) - (7)].tree);
	IDL_ns_push_scope (__IDL_root_ns, IDL_IDENT_TO_NS ((yyvsp[(4) - (7)].tree)));
	if (IDL_ns_check_for_ambiguous_inheritance ((yyvsp[(4) - (7)].tree), (yyvsp[(7) - (7)].tree)))
		__IDL_is_okay = FALSE;
}
    break;

  case 31:

/* Line 1455 of yacc.c  */
#line 446 "./parser.y"
    {
 	(yyval.tree) = IDL_interface_new ((yyvsp[(4) - (12)].tree), (yyvsp[(7) - (12)].tree), (yyvsp[(10) - (12)].tree));
	assign_declspec ((yyval.tree), (yyvsp[(1) - (12)].declspec));
	assign_props (IDL_INTERFACE ((yyval.tree)).ident, (yyvsp[(2) - (12)].hash_table));
}
    break;

  case 32:

/* Line 1455 of yacc.c  */
#line 455 "./parser.y"
    {
	if ((yyvsp[(2) - (5)].hash_table)) yywarningv (IDL_WARNING1,
			    "Ignoring properties for forward declaration `%s'",
			    IDL_IDENT ((yyvsp[(4) - (5)].tree)).str);
	(yyval.tree) = IDL_forward_dcl_new ((yyvsp[(4) - (5)].tree));
	assign_declspec ((yyval.tree), (yyvsp[(1) - (5)].declspec));
}
    break;

  case 33:

/* Line 1455 of yacc.c  */
#line 464 "./parser.y"
    { (yyval.tree) = NULL; }
    break;

  case 34:

/* Line 1455 of yacc.c  */
#line 465 "./parser.y"
    {
	GHashTable *table = g_hash_table_new (g_direct_hash, g_direct_equal);
	gboolean die = FALSE;
	IDL_tree p = (yyvsp[(2) - (2)].tree);

	assert (IDL_NODE_TYPE (p) == IDLN_LIST);
	for (; p != NULL && !die; p = IDL_LIST (p).next) {
		assert (IDL_LIST (p).data != NULL);
		assert (IDL_NODE_TYPE (IDL_LIST (p).data) == IDLN_IDENT);

		if (g_hash_table_lookup_extended (table, IDL_LIST (p).data, NULL, NULL)) {
			char *s = IDL_ns_ident_to_qstring (IDL_LIST (p).data, "::", 0);
			yyerrorv ("Cannot inherit from interface `%s' more than once", s);
			g_free (s);
			die = TRUE;
			break;
		} else
			g_hash_table_insert (table, IDL_LIST (p).data, NULL);

		if (IDL_NODE_TYPE (IDL_NODE_UP (IDL_LIST (p).data)) == IDLN_FORWARD_DCL) {
			char *s = IDL_ns_ident_to_qstring (IDL_LIST (p).data, "::", 0);
			yyerrorv ("Incomplete definition of interface `%s'", s);
			IDL_tree_error (IDL_LIST (p).data,
					"Previous forward declaration of `%s'", s);
			g_free (s);
			die = TRUE;
		}
		else if (IDL_NODE_TYPE (IDL_NODE_UP (IDL_LIST (p).data)) != IDLN_INTERFACE) {
			char *s = IDL_ns_ident_to_qstring (IDL_LIST (p).data, "::", 0);
			yyerrorv ("`%s' is not an interface", s);
			IDL_tree_error (IDL_LIST (p).data,
					"Previous declaration of `%s'", s);
			g_free (s);
			die = TRUE;
		}
	}

	g_hash_table_destroy (table);

	if (die)
		YYABORT;

	(yyval.tree) = (yyvsp[(2) - (2)].tree);
}
    break;

  case 35:

/* Line 1455 of yacc.c  */
#line 511 "./parser.y"
    { (yyval.tree) = list_start ((yyvsp[(1) - (1)].tree), TRUE); }
    break;

  case 36:

/* Line 1455 of yacc.c  */
#line 513 "./parser.y"
    { (yyval.tree) = list_chain ((yyvsp[(1) - (3)].tree), (yyvsp[(3) - (3)].tree), TRUE); }
    break;

  case 38:

/* Line 1455 of yacc.c  */
#line 519 "./parser.y"
    { (yyval.tree) = NULL; }
    break;

  case 39:

/* Line 1455 of yacc.c  */
#line 520 "./parser.y"
    { (yyval.tree) = zlist_chain ((yyvsp[(1) - (2)].tree), (yyvsp[(2) - (2)].tree), TRUE); }
    break;

  case 47:

/* Line 1455 of yacc.c  */
#line 532 "./parser.y"
    {
	(yyval.tree) = (yyvsp[(2) - (2)].tree);
	assign_declspec ((yyval.tree), (yyvsp[(1) - (2)].declspec));
}
    break;

  case 48:

/* Line 1455 of yacc.c  */
#line 539 "./parser.y"
    {
	IDL_tree_node node;
	IDL_tree p, dcl;

	(yyval.tree) = (yyvsp[(3) - (3)].tree);
	node.properties = (yyvsp[(1) - (3)].hash_table);
	for (p = IDL_TYPE_DCL ((yyvsp[(3) - (3)].tree)).dcls; p; p = IDL_LIST (p).next) {
		dcl = IDL_LIST (p).data;
		IDL_tree_properties_copy (&node, dcl);
	}
	__IDL_free_properties (node.properties);
}
    break;

  case 52:

/* Line 1455 of yacc.c  */
#line 555 "./parser.y"
    {
	(yyval.tree) = IDL_native_new ((yyvsp[(3) - (3)].tree));
	assign_props (IDL_NATIVE ((yyval.tree)).ident, (yyvsp[(1) - (3)].hash_table));
}
    break;

  case 53:

/* Line 1455 of yacc.c  */
#line 560 "./parser.y"
    {
	/* Enable native type scanning */
	if (__IDL_pidl > 0)
		__IDL_flagsi |= IDLFP_NATIVE;
	else {
		yyerror ("Native syntax not enabled");
		YYABORT;
	}
}
    break;

  case 54:

/* Line 1455 of yacc.c  */
#line 568 "./parser.y"
    {
	(yyval.tree) = IDL_native_new ((yyvsp[(3) - (6)].tree));
	IDL_NATIVE ((yyval.tree)).user_type = (yyvsp[(6) - (6)].str);
	assign_props (IDL_NATIVE ((yyval.tree)).ident, (yyvsp[(1) - (6)].hash_table));
}
    break;

  case 55:

/* Line 1455 of yacc.c  */
#line 575 "./parser.y"
    { (yyval.tree) = IDL_type_dcl_new ((yyvsp[(1) - (2)].tree), (yyvsp[(2) - (2)].tree)); }
    break;

  case 64:

/* Line 1455 of yacc.c  */
#line 592 "./parser.y"
    {
	yyerrorv ("Missing identifier in %s definition", (yyvsp[(0) - (0)].str));
	YYABORT;
}
    break;

  case 66:

/* Line 1455 of yacc.c  */
#line 599 "./parser.y"
    {
	yyerrorv ("Missing identifier in %s definition", (yyvsp[(0) - (0)].str));
	YYABORT;
}
    break;

  case 68:

/* Line 1455 of yacc.c  */
#line 607 "./parser.y"
    { (yyval.str) = "struct"; }
    break;

  case 69:

/* Line 1455 of yacc.c  */
#line 608 "./parser.y"
    {
	g_hash_table_insert (__IDL_structunion_ht, (yyvsp[(4) - (5)].tree), (yyvsp[(4) - (5)].tree));
	(yyval.tree) = IDL_type_struct_new ((yyvsp[(4) - (5)].tree), NULL);
}
    break;

  case 70:

/* Line 1455 of yacc.c  */
#line 612 "./parser.y"
    {
	g_hash_table_remove (__IDL_structunion_ht, (yyvsp[(4) - (9)].tree));
	(yyval.tree) = (yyvsp[(6) - (9)].tree);
	__IDL_assign_up_node ((yyval.tree), (yyvsp[(7) - (9)].tree));
	IDL_TYPE_STRUCT ((yyval.tree)).member_list = (yyvsp[(7) - (9)].tree);
	assign_props (IDL_TYPE_STRUCT ((yyval.tree)).ident, (yyvsp[(1) - (9)].hash_table));
}
    break;

  case 71:

/* Line 1455 of yacc.c  */
#line 622 "./parser.y"
    { (yyval.str) = "union"; }
    break;

  case 72:

/* Line 1455 of yacc.c  */
#line 625 "./parser.y"
    {
	g_hash_table_insert (__IDL_structunion_ht, (yyvsp[(4) - (9)].tree), (yyvsp[(4) - (9)].tree));
	(yyval.tree) = IDL_type_union_new ((yyvsp[(4) - (9)].tree), (yyvsp[(7) - (9)].tree), NULL);
}
    break;

  case 73:

/* Line 1455 of yacc.c  */
#line 629 "./parser.y"
    {
	g_hash_table_remove (__IDL_structunion_ht, (yyvsp[(4) - (13)].tree));
	(yyval.tree) = (yyvsp[(10) - (13)].tree);
	__IDL_assign_up_node ((yyval.tree), (yyvsp[(11) - (13)].tree));
	IDL_TYPE_UNION ((yyval.tree)).switch_body = (yyvsp[(11) - (13)].tree);
	assign_props (IDL_TYPE_UNION ((yyval.tree)).ident, (yyvsp[(1) - (13)].hash_table));
}
    break;

  case 80:

/* Line 1455 of yacc.c  */
#line 648 "./parser.y"
    { (yyval.tree) = list_start ((yyvsp[(1) - (1)].tree), TRUE); }
    break;

  case 81:

/* Line 1455 of yacc.c  */
#line 649 "./parser.y"
    { (yyval.tree) = list_chain ((yyvsp[(1) - (2)].tree), (yyvsp[(2) - (2)].tree), TRUE); }
    break;

  case 82:

/* Line 1455 of yacc.c  */
#line 653 "./parser.y"
    { (yyval.tree) = IDL_case_stmt_new ((yyvsp[(1) - (3)].tree), (yyvsp[(2) - (3)].tree)); }
    break;

  case 83:

/* Line 1455 of yacc.c  */
#line 656 "./parser.y"
    {
	char *s;

	(yyval.tree) = IDL_member_new ((yyvsp[(1) - (2)].tree), list_start ((yyvsp[(2) - (2)].tree), TRUE));
	if (IDL_NODE_TYPE ((yyvsp[(1) - (2)].tree)) == IDLN_IDENT &&
	    g_hash_table_lookup (__IDL_structunion_ht, (yyvsp[(1) - (2)].tree))) {
		s = IDL_ns_ident_to_qstring ((yyvsp[(2) - (2)].tree), "::", 0);
		yyerrorv ("Member `%s'", s);
		do_token_error (IDL_NODE_UP ((yyvsp[(1) - (2)].tree)), "recurses", TRUE);
		g_free (s);
	}
}
    break;

  case 84:

/* Line 1455 of yacc.c  */
#line 670 "./parser.y"
    { (yyval.tree) = list_start ((yyvsp[(1) - (1)].tree), FALSE); }
    break;

  case 85:

/* Line 1455 of yacc.c  */
#line 671 "./parser.y"
    { (yyval.tree) = list_chain ((yyvsp[(1) - (2)].tree), (yyvsp[(2) - (2)].tree), FALSE); }
    break;

  case 86:

/* Line 1455 of yacc.c  */
#line 674 "./parser.y"
    { (yyval.tree) = (yyvsp[(2) - (3)].tree); }
    break;

  case 87:

/* Line 1455 of yacc.c  */
#line 675 "./parser.y"
    { (yyval.tree) = NULL; }
    break;

  case 88:

/* Line 1455 of yacc.c  */
#line 678 "./parser.y"
    {
	(yyval.tree) = (yyvsp[(2) - (2)].tree);
	assign_declspec ((yyval.tree), (yyvsp[(1) - (2)].declspec));
}
    break;

  case 89:

/* Line 1455 of yacc.c  */
#line 685 "./parser.y"
    {
	(yyval.tree) = IDL_const_dcl_new ((yyvsp[(2) - (5)].tree), (yyvsp[(3) - (5)].tree), (yyvsp[(5) - (5)].tree));
	/* Should probably do some type checking here... */
}
    break;

  case 90:

/* Line 1455 of yacc.c  */
#line 691 "./parser.y"
    {
	(yyval.tree) = (yyvsp[(2) - (2)].tree);
	assign_declspec ((yyval.tree), (yyvsp[(1) - (2)].declspec));
}
    break;

  case 91:

/* Line 1455 of yacc.c  */
#line 699 "./parser.y"
    { (yyval.tree) = IDL_except_dcl_new ((yyvsp[(2) - (6)].tree), (yyvsp[(4) - (6)].tree)); }
    break;

  case 92:

/* Line 1455 of yacc.c  */
#line 702 "./parser.y"
    { (yyval.tree) = NULL; }
    break;

  case 93:

/* Line 1455 of yacc.c  */
#line 703 "./parser.y"
    { (yyval.tree) = zlist_chain ((yyvsp[(1) - (2)].tree), (yyvsp[(2) - (2)].tree), TRUE); }
    break;

  case 94:

/* Line 1455 of yacc.c  */
#line 706 "./parser.y"
    { (yyval.boolean) = FALSE; }
    break;

  case 95:

/* Line 1455 of yacc.c  */
#line 707 "./parser.y"
    { (yyval.boolean) = TRUE; }
    break;

  case 96:

/* Line 1455 of yacc.c  */
#line 710 "./parser.y"
    {
	(yyval.tree) = (yyvsp[(2) - (2)].tree);
	assign_declspec ((yyval.tree), (yyvsp[(1) - (2)].declspec));
}
    break;

  case 97:

/* Line 1455 of yacc.c  */
#line 719 "./parser.y"
    { (yyval.str) = "attribute"; }
    break;

  case 98:

/* Line 1455 of yacc.c  */
#line 721 "./parser.y"
    {
	IDL_tree_node node;
	IDL_tree p, dcl;

	(yyval.tree) = IDL_attr_dcl_new ((yyvsp[(2) - (6)].boolean), (yyvsp[(5) - (6)].tree), (yyvsp[(6) - (6)].tree));
	node.properties = (yyvsp[(1) - (6)].hash_table);
	for (p = (yyvsp[(6) - (6)].tree); p; p = IDL_LIST (p).next) {
		dcl = IDL_LIST (p).data;
		IDL_tree_properties_copy (&node, dcl);
	}
	__IDL_free_properties (node.properties);
}
    break;

  case 100:

/* Line 1455 of yacc.c  */
#line 736 "./parser.y"
    {
	yyerrorv ("Illegal type `void' for %s", (yyvsp[(0) - (1)].str));
	(yyval.tree) = NULL;
}
    break;

  case 108:

/* Line 1455 of yacc.c  */
#line 752 "./parser.y"
    {
	illegal_context_type_error ((yyvsp[(1) - (1)].tree), (yyvsp[(0) - (1)].str));
	(yyval.tree) = (yyvsp[(1) - (1)].tree);
}
    break;

  case 109:

/* Line 1455 of yacc.c  */
#line 758 "./parser.y"
    { (yyval.boolean) = FALSE; }
    break;

  case 110:

/* Line 1455 of yacc.c  */
#line 759 "./parser.y"
    { (yyval.boolean) = TRUE; }
    break;

  case 111:

/* Line 1455 of yacc.c  */
#line 762 "./parser.y"
    {
	(yyval.tree) = (yyvsp[(2) - (2)].tree);
	assign_declspec ((yyval.tree), (yyvsp[(1) - (2)].declspec));
}
    break;

  case 112:

/* Line 1455 of yacc.c  */
#line 773 "./parser.y"
    {
	(yyval.tree) = IDL_op_dcl_new ((yyvsp[(2) - (8)].boolean), (yyvsp[(3) - (8)].tree), (yyvsp[(4) - (8)].tree), (yyvsp[(5) - (8)].treedata).tree, (yyvsp[(7) - (8)].tree), (yyvsp[(8) - (8)].tree));
	IDL_OP_DCL ((yyval.tree)).f_varargs = GPOINTER_TO_INT ((yyvsp[(5) - (8)].treedata).data);
	assign_props (IDL_OP_DCL ((yyval.tree)).ident, (yyvsp[(1) - (8)].hash_table));
}
    break;

  case 113:

/* Line 1455 of yacc.c  */
#line 780 "./parser.y"
    { (yyval.str) = "operation return value"; }
    break;

  case 114:

/* Line 1455 of yacc.c  */
#line 781 "./parser.y"
    { (yyval.tree) = (yyvsp[(2) - (2)].tree); }
    break;

  case 115:

/* Line 1455 of yacc.c  */
#line 782 "./parser.y"
    { (yyval.tree) = NULL; }
    break;

  case 116:

/* Line 1455 of yacc.c  */
#line 785 "./parser.y"
    { (yyval.boolean) = FALSE; }
    break;

  case 117:

/* Line 1455 of yacc.c  */
#line 786 "./parser.y"
    { (yyval.boolean) = TRUE; }
    break;

  case 118:

/* Line 1455 of yacc.c  */
#line 789 "./parser.y"
    { (yyval.boolean) = FALSE; }
    break;

  case 119:

/* Line 1455 of yacc.c  */
#line 790 "./parser.y"
    { (yyval.boolean) = TRUE; }
    break;

  case 120:

/* Line 1455 of yacc.c  */
#line 796 "./parser.y"
    {
	(yyval.treedata).tree = (yyvsp[(2) - (4)].tree);
	(yyval.treedata).data = GINT_TO_POINTER ((yyvsp[(3) - (4)].boolean));
}
    break;

  case 121:

/* Line 1455 of yacc.c  */
#line 800 "./parser.y"
    {
	(yyval.treedata).tree = NULL;
	(yyval.treedata).data = GINT_TO_POINTER ((yyvsp[(2) - (3)].boolean));
}
    break;

  case 122:

/* Line 1455 of yacc.c  */
#line 806 "./parser.y"
    { (yyval.tree) = list_start ((yyvsp[(1) - (1)].tree), TRUE); }
    break;

  case 123:

/* Line 1455 of yacc.c  */
#line 808 "./parser.y"
    { (yyval.tree) = list_chain ((yyvsp[(1) - (3)].tree), (yyvsp[(3) - (3)].tree), TRUE); }
    break;

  case 124:

/* Line 1455 of yacc.c  */
#line 813 "./parser.y"
    { (yyval.str) = "parameter"; }
    break;

  case 125:

/* Line 1455 of yacc.c  */
#line 815 "./parser.y"
    {
	(yyval.tree) = IDL_param_dcl_new ((yyvsp[(2) - (5)].paramattr), (yyvsp[(4) - (5)].tree), (yyvsp[(5) - (5)].tree));
	assign_props (IDL_PARAM_DCL ((yyval.tree)).simple_declarator, (yyvsp[(1) - (5)].hash_table));
}
    break;

  case 126:

/* Line 1455 of yacc.c  */
#line 821 "./parser.y"
    { (yyval.paramattr) = IDL_PARAM_IN; }
    break;

  case 127:

/* Line 1455 of yacc.c  */
#line 822 "./parser.y"
    { (yyval.paramattr) = IDL_PARAM_OUT; }
    break;

  case 128:

/* Line 1455 of yacc.c  */
#line 823 "./parser.y"
    { (yyval.paramattr) = IDL_PARAM_INOUT; }
    break;

  case 129:

/* Line 1455 of yacc.c  */
#line 824 "./parser.y"
    {
	yyerrorv ("Missing direction attribute (in, out, inout) before parameter");
	IDL_tree_free ((yyvsp[(1) - (1)].tree));
}
    break;

  case 130:

/* Line 1455 of yacc.c  */
#line 830 "./parser.y"
    { (yyval.tree) = NULL; }
    break;

  case 132:

/* Line 1455 of yacc.c  */
#line 834 "./parser.y"
    { (yyval.tree) = NULL; }
    break;

  case 134:

/* Line 1455 of yacc.c  */
#line 840 "./parser.y"
    { (yyval.tree) = (yyvsp[(3) - (4)].tree); }
    break;

  case 135:

/* Line 1455 of yacc.c  */
#line 845 "./parser.y"
    { (yyval.tree) = (yyvsp[(3) - (4)].tree); }
    break;

  case 148:

/* Line 1455 of yacc.c  */
#line 864 "./parser.y"
    { do_binop ((yyval.tree), IDL_BINOP_OR, (yyvsp[(1) - (3)].tree), (yyvsp[(3) - (3)].tree)); }
    break;

  case 150:

/* Line 1455 of yacc.c  */
#line 868 "./parser.y"
    { do_binop ((yyval.tree), IDL_BINOP_XOR, (yyvsp[(1) - (3)].tree), (yyvsp[(3) - (3)].tree)); }
    break;

  case 152:

/* Line 1455 of yacc.c  */
#line 872 "./parser.y"
    { do_binop ((yyval.tree), IDL_BINOP_AND, (yyvsp[(1) - (3)].tree), (yyvsp[(3) - (3)].tree)); }
    break;

  case 154:

/* Line 1455 of yacc.c  */
#line 876 "./parser.y"
    { do_binop ((yyval.tree), IDL_BINOP_SHR, (yyvsp[(1) - (3)].tree), (yyvsp[(3) - (3)].tree)); }
    break;

  case 155:

/* Line 1455 of yacc.c  */
#line 877 "./parser.y"
    { do_binop ((yyval.tree), IDL_BINOP_SHL, (yyvsp[(1) - (3)].tree), (yyvsp[(3) - (3)].tree)); }
    break;

  case 157:

/* Line 1455 of yacc.c  */
#line 881 "./parser.y"
    { do_binop ((yyval.tree), IDL_BINOP_ADD, (yyvsp[(1) - (3)].tree), (yyvsp[(3) - (3)].tree)); }
    break;

  case 158:

/* Line 1455 of yacc.c  */
#line 882 "./parser.y"
    { do_binop ((yyval.tree), IDL_BINOP_SUB, (yyvsp[(1) - (3)].tree), (yyvsp[(3) - (3)].tree)); }
    break;

  case 160:

/* Line 1455 of yacc.c  */
#line 886 "./parser.y"
    { do_binop ((yyval.tree), IDL_BINOP_MULT, (yyvsp[(1) - (3)].tree), (yyvsp[(3) - (3)].tree)); }
    break;

  case 161:

/* Line 1455 of yacc.c  */
#line 887 "./parser.y"
    { do_binop ((yyval.tree), IDL_BINOP_DIV, (yyvsp[(1) - (3)].tree), (yyvsp[(3) - (3)].tree)); }
    break;

  case 162:

/* Line 1455 of yacc.c  */
#line 888 "./parser.y"
    { do_binop ((yyval.tree), IDL_BINOP_MOD, (yyvsp[(1) - (3)].tree), (yyvsp[(3) - (3)].tree)); }
    break;

  case 163:

/* Line 1455 of yacc.c  */
#line 891 "./parser.y"
    { do_unaryop ((yyval.tree), (yyvsp[(1) - (2)].unaryop), (yyvsp[(2) - (2)].tree)); }
    break;

  case 165:

/* Line 1455 of yacc.c  */
#line 895 "./parser.y"
    { (yyval.unaryop) = IDL_UNARYOP_MINUS; }
    break;

  case 166:

/* Line 1455 of yacc.c  */
#line 896 "./parser.y"
    { (yyval.unaryop) = IDL_UNARYOP_PLUS; }
    break;

  case 167:

/* Line 1455 of yacc.c  */
#line 897 "./parser.y"
    { (yyval.unaryop) = IDL_UNARYOP_COMPLEMENT; }
    break;

  case 168:

/* Line 1455 of yacc.c  */
#line 900 "./parser.y"
    {
	IDL_tree p, literal;
	
	assert (IDL_NODE_TYPE ((yyvsp[(1) - (1)].tree)) == IDLN_IDENT);

	p = IDL_NODE_UP ((yyvsp[(1) - (1)].tree));
	
	if ((literal = IDL_resolve_const_exp ((yyvsp[(1) - (1)].tree), IDLN_ANY))) {
		++IDL_NODE_REFS (literal);
		(yyval.tree) = literal;
		IDL_tree_free ((yyvsp[(1) - (1)].tree));
	} else
		(yyval.tree) = (yyvsp[(1) - (1)].tree);
}
    break;

  case 170:

/* Line 1455 of yacc.c  */
#line 915 "./parser.y"
    { (yyval.tree) = (yyvsp[(2) - (3)].tree); }
    break;

  case 177:

/* Line 1455 of yacc.c  */
#line 927 "./parser.y"
    { (yyval.str) = "enum"; }
    break;

  case 178:

/* Line 1455 of yacc.c  */
#line 930 "./parser.y"
    {
	(yyval.tree) = IDL_type_enum_new ((yyvsp[(4) - (7)].tree), (yyvsp[(6) - (7)].tree));
	assign_props (IDL_TYPE_ENUM ((yyval.tree)).ident, (yyvsp[(1) - (7)].hash_table));
}
    break;

  case 179:

/* Line 1455 of yacc.c  */
#line 936 "./parser.y"
    {
	assert ((yyvsp[(1) - (1)].tree) != NULL);
	assert (IDL_NODE_TYPE ((yyvsp[(1) - (1)].tree)) == IDLN_GENTREE);
	assert (IDL_NODE_TYPE (IDL_GENTREE ((yyvsp[(1) - (1)].tree)).data) == IDLN_IDENT);
	(yyval.tree) = IDL_GENTREE ((yyvsp[(1) - (1)].tree)).data;
}
    break;

  case 181:

/* Line 1455 of yacc.c  */
#line 945 "./parser.y"
    { (yyval.tree) = (yyvsp[(2) - (2)].tree); }
    break;

  case 182:

/* Line 1455 of yacc.c  */
#line 947 "./parser.y"
    {
	IDL_tree p;

	assert (IDL_NODE_TYPE ((yyvsp[(1) - (3)].tree)) == IDLN_GENTREE);
	assert (IDL_NODE_TYPE ((yyvsp[(3) - (3)].tree)) == IDLN_IDENT);

#ifdef YYDEBUG
	if (yydebug)
		fprintf (stderr, "ns: looking in `%s' for `%s'\n", 
		  IDL_IDENT (IDL_GENTREE ((yyvsp[(1) - (3)].tree)).data).str, IDL_IDENT((yyvsp[(3) - (3)].tree)).str);
#endif

	if ((p = IDL_ns_lookup_this_scope (__IDL_root_ns, (yyvsp[(1) - (3)].tree), (yyvsp[(3) - (3)].tree), NULL)) == NULL) {
		yyerrorv ("`%s' undeclared identifier", IDL_IDENT ((yyvsp[(3) - (3)].tree)).str);
		IDL_tree_free ((yyvsp[(3) - (3)].tree));
		YYABORT;
	}
	IDL_tree_free ((yyvsp[(3) - (3)].tree));
#ifdef REF_IDENTS
	++IDL_NODE_REFS (IDL_GENTREE (p).data);
#endif
	(yyval.tree) = p;
}
    break;

  case 183:

/* Line 1455 of yacc.c  */
#line 972 "./parser.y"
    { (yyval.tree) = list_start ((yyvsp[(1) - (1)].tree), TRUE); }
    break;

  case 184:

/* Line 1455 of yacc.c  */
#line 974 "./parser.y"
    { (yyval.tree) = list_chain ((yyvsp[(1) - (3)].tree), (yyvsp[(3) - (3)].tree), TRUE); }
    break;

  case 185:

/* Line 1455 of yacc.c  */
#line 977 "./parser.y"
    { (yyval.tree) = list_start ((yyvsp[(1) - (1)].tree), TRUE); }
    break;

  case 186:

/* Line 1455 of yacc.c  */
#line 978 "./parser.y"
    { (yyval.tree) = list_chain ((yyvsp[(1) - (2)].tree), (yyvsp[(2) - (2)].tree), TRUE); }
    break;

  case 187:

/* Line 1455 of yacc.c  */
#line 982 "./parser.y"
    {
	char *s;

	(yyval.tree) = IDL_member_new ((yyvsp[(1) - (3)].tree), (yyvsp[(2) - (3)].tree));
	if (IDL_NODE_TYPE ((yyvsp[(1) - (3)].tree)) == IDLN_IDENT &&
	    g_hash_table_lookup (__IDL_structunion_ht, (yyvsp[(1) - (3)].tree))) {
		s = IDL_ns_ident_to_qstring (IDL_LIST ((yyvsp[(2) - (3)].tree)).data, "::", 0);
		yyerrorv ("Member `%s'", s);
		do_token_error (IDL_NODE_UP ((yyvsp[(1) - (3)].tree)), "recurses", TRUE);
		g_free (s);
	}
}
    break;

  case 201:

/* Line 1455 of yacc.c  */
#line 1015 "./parser.y"
    { (yyval.tree) = IDL_type_sequence_new ((yyvsp[(3) - (6)].tree), (yyvsp[(5) - (6)].tree)); }
    break;

  case 202:

/* Line 1455 of yacc.c  */
#line 1018 "./parser.y"
    { (yyval.tree) = IDL_type_sequence_new ((yyvsp[(3) - (4)].tree), NULL); }
    break;

  case 203:

/* Line 1455 of yacc.c  */
#line 1021 "./parser.y"
    { (yyval.tree) = IDL_type_float_new (IDL_FLOAT_TYPE_FLOAT); }
    break;

  case 204:

/* Line 1455 of yacc.c  */
#line 1022 "./parser.y"
    { (yyval.tree) = IDL_type_float_new (IDL_FLOAT_TYPE_DOUBLE); }
    break;

  case 205:

/* Line 1455 of yacc.c  */
#line 1023 "./parser.y"
    { (yyval.tree) = IDL_type_float_new (IDL_FLOAT_TYPE_LONGDOUBLE); }
    break;

  case 206:

/* Line 1455 of yacc.c  */
#line 1028 "./parser.y"
    { (yyval.tree) = IDL_type_fixed_new ((yyvsp[(3) - (6)].tree), (yyvsp[(5) - (6)].tree)); }
    break;

  case 207:

/* Line 1455 of yacc.c  */
#line 1031 "./parser.y"
    { (yyval.tree) = IDL_type_fixed_new (NULL, NULL); }
    break;

  case 208:

/* Line 1455 of yacc.c  */
#line 1034 "./parser.y"
    { (yyval.tree) = IDL_type_integer_new (TRUE, (yyvsp[(1) - (1)].integer)); }
    break;

  case 209:

/* Line 1455 of yacc.c  */
#line 1035 "./parser.y"
    { (yyval.tree) = IDL_type_integer_new (FALSE, (yyvsp[(1) - (1)].integer)); }
    break;

  case 210:

/* Line 1455 of yacc.c  */
#line 1038 "./parser.y"
    { (yyval.integer) = IDL_INTEGER_TYPE_SHORT; }
    break;

  case 211:

/* Line 1455 of yacc.c  */
#line 1039 "./parser.y"
    { (yyval.integer) = IDL_INTEGER_TYPE_LONG; }
    break;

  case 212:

/* Line 1455 of yacc.c  */
#line 1040 "./parser.y"
    { (yyval.integer) = IDL_INTEGER_TYPE_LONGLONG; }
    break;

  case 216:

/* Line 1455 of yacc.c  */
#line 1052 "./parser.y"
    { (yyval.integer) = IDL_INTEGER_TYPE_SHORT; }
    break;

  case 217:

/* Line 1455 of yacc.c  */
#line 1053 "./parser.y"
    { (yyval.integer) = IDL_INTEGER_TYPE_LONG; }
    break;

  case 218:

/* Line 1455 of yacc.c  */
#line 1054 "./parser.y"
    { (yyval.integer) = IDL_INTEGER_TYPE_LONGLONG; }
    break;

  case 222:

/* Line 1455 of yacc.c  */
#line 1066 "./parser.y"
    { (yyval.tree) = IDL_type_char_new (); }
    break;

  case 223:

/* Line 1455 of yacc.c  */
#line 1069 "./parser.y"
    { (yyval.tree) = IDL_type_wide_char_new (); }
    break;

  case 224:

/* Line 1455 of yacc.c  */
#line 1072 "./parser.y"
    { (yyval.tree) = IDL_type_boolean_new (); }
    break;

  case 225:

/* Line 1455 of yacc.c  */
#line 1075 "./parser.y"
    { (yyval.tree) = IDL_type_octet_new (); }
    break;

  case 226:

/* Line 1455 of yacc.c  */
#line 1078 "./parser.y"
    { (yyval.tree) = IDL_type_any_new (); }
    break;

  case 227:

/* Line 1455 of yacc.c  */
#line 1081 "./parser.y"
    { (yyval.tree) = IDL_type_object_new (); }
    break;

  case 228:

/* Line 1455 of yacc.c  */
#line 1084 "./parser.y"
    { (yyval.tree) = IDL_type_typecode_new (); }
    break;

  case 229:

/* Line 1455 of yacc.c  */
#line 1089 "./parser.y"
    { (yyval.tree) = IDL_type_string_new ((yyvsp[(3) - (4)].tree)); }
    break;

  case 230:

/* Line 1455 of yacc.c  */
#line 1090 "./parser.y"
    { (yyval.tree) = IDL_type_string_new (NULL); }
    break;

  case 231:

/* Line 1455 of yacc.c  */
#line 1095 "./parser.y"
    { (yyval.tree) = IDL_type_wide_string_new ((yyvsp[(3) - (4)].tree)); }
    break;

  case 232:

/* Line 1455 of yacc.c  */
#line 1096 "./parser.y"
    { (yyval.tree) = IDL_type_wide_string_new (NULL); }
    break;

  case 233:

/* Line 1455 of yacc.c  */
#line 1099 "./parser.y"
    { (yyval.tree) = list_start ((yyvsp[(1) - (1)].tree), TRUE); }
    break;

  case 234:

/* Line 1455 of yacc.c  */
#line 1101 "./parser.y"
    { (yyval.tree) = list_chain ((yyvsp[(1) - (3)].tree), (yyvsp[(3) - (3)].tree), TRUE); }
    break;

  case 239:

/* Line 1455 of yacc.c  */
#line 1114 "./parser.y"
    { (yyval.tree) = list_start ((yyvsp[(1) - (1)].tree), TRUE); }
    break;

  case 240:

/* Line 1455 of yacc.c  */
#line 1116 "./parser.y"
    { (yyval.tree) = list_chain ((yyvsp[(1) - (3)].tree), (yyvsp[(3) - (3)].tree), TRUE); }
    break;

  case 241:

/* Line 1455 of yacc.c  */
#line 1120 "./parser.y"
    {
	IDL_tree p;
	int i;

	(yyval.tree) = IDL_type_array_new ((yyvsp[(1) - (2)].tree), (yyvsp[(2) - (2)].tree));
	for (i = 1, p = (yyvsp[(2) - (2)].tree); p; ++i, p = IDL_LIST (p).next)
		if (!IDL_LIST (p).data) {
			char *s = IDL_ns_ident_to_qstring ((yyvsp[(1) - (2)].tree), "::", 0);
			yyerrorv ("Missing value in dimension %d of array `%s'", i, s);
			g_free (s);
		}
}
    break;

  case 242:

/* Line 1455 of yacc.c  */
#line 1134 "./parser.y"
    { (yyval.tree) = list_start ((yyvsp[(1) - (1)].tree), FALSE); }
    break;

  case 243:

/* Line 1455 of yacc.c  */
#line 1136 "./parser.y"
    { (yyval.tree) = list_chain ((yyvsp[(1) - (2)].tree), (yyvsp[(2) - (2)].tree), FALSE); }
    break;

  case 244:

/* Line 1455 of yacc.c  */
#line 1139 "./parser.y"
    { (yyval.tree) = (yyvsp[(2) - (3)].tree); }
    break;

  case 245:

/* Line 1455 of yacc.c  */
#line 1140 "./parser.y"
    { (yyval.tree) = NULL; }
    break;

  case 246:

/* Line 1455 of yacc.c  */
#line 1144 "./parser.y"
    {
	(yyval.hash_table) = g_hash_table_new (IDL_strcase_hash, IDL_strcase_equal);
	g_hash_table_insert ((yyval.hash_table), (yyvsp[(1) - (2)].str), (yyvsp[(2) - (2)].str));
}
    break;

  case 247:

/* Line 1455 of yacc.c  */
#line 1150 "./parser.y"
    {
	(yyval.hash_table) = (yyvsp[(1) - (4)].hash_table);
	g_hash_table_insert ((yyval.hash_table), (yyvsp[(3) - (4)].str), (yyvsp[(4) - (4)].str));
}
    break;

  case 248:

/* Line 1455 of yacc.c  */
#line 1154 "./parser.y"
    {
	(yyval.hash_table) = g_hash_table_new (IDL_strcase_hash, IDL_strcase_equal);
	g_hash_table_insert ((yyval.hash_table), (yyvsp[(1) - (1)].str), g_strdup (""));
}
    break;

  case 249:

/* Line 1455 of yacc.c  */
#line 1159 "./parser.y"
    {
	(yyval.hash_table) = (yyvsp[(1) - (3)].hash_table);
	g_hash_table_insert ((yyval.hash_table), (yyvsp[(3) - (3)].str), g_strdup (""));
}
    break;

  case 250:

/* Line 1455 of yacc.c  */
#line 1165 "./parser.y"
    { (yyval.tree) = IDL_ident_new ((yyvsp[(1) - (1)].str)); }
    break;

  case 251:

/* Line 1455 of yacc.c  */
#line 1168 "./parser.y"
    {
	assert ((yyvsp[(1) - (1)].tree) != NULL);
	assert (IDL_NODE_TYPE ((yyvsp[(1) - (1)].tree)) == IDLN_GENTREE);
	assert (IDL_NODE_TYPE (IDL_GENTREE ((yyvsp[(1) - (1)].tree)).data) == IDLN_IDENT);
	(yyval.tree) = IDL_GENTREE ((yyvsp[(1) - (1)].tree)).data;
}
    break;

  case 252:

/* Line 1455 of yacc.c  */
#line 1176 "./parser.y"
    {
	IDL_tree	old_top = IDL_GENTREE (IDL_NS (__IDL_root_ns).current).data;
	IDL_ns_push_scope (__IDL_root_ns, (yyvsp[(1) - (1)].tree));
#ifdef YYDEBUG
	if (yydebug)
		fprintf (stderr, "ns: entering new scope `%s' of `%s'\n", 
		       IDL_IDENT (IDL_GENTREE (IDL_NS (__IDL_root_ns).current).data).str, IDL_IDENT(old_top).str);
#endif
	assert (IDL_NODE_TYPE (IDL_GENTREE ((yyvsp[(1) - (1)].tree)).data) == IDLN_IDENT);
	(yyval.tree) = IDL_GENTREE ((yyvsp[(1) - (1)].tree)).data;
}
    break;

  case 253:

/* Line 1455 of yacc.c  */
#line 1189 "./parser.y"
    {
	IDL_tree	old_top = IDL_GENTREE (IDL_NS (__IDL_root_ns).current).data;
	IDL_ns_push_scope (__IDL_root_ns, (yyvsp[(1) - (1)].tree));
	assert (IDL_NS (__IDL_root_ns).current != NULL);
	assert (IDL_NODE_TYPE (IDL_NS (__IDL_root_ns).current) == IDLN_GENTREE);
	assert (IDL_GENTREE (IDL_NS (__IDL_root_ns).current).data != NULL);
	assert (IDL_NODE_TYPE (IDL_GENTREE (IDL_NS (__IDL_root_ns).current).data) == IDLN_IDENT);
#ifdef YYDEBUG
	if (yydebug)
		fprintf (stderr,"ns: entering new/prev scope `%s' of `%s'\n",
		       IDL_IDENT (IDL_GENTREE (IDL_NS (__IDL_root_ns).current).data).str, IDL_IDENT(old_top).str);
#endif
	assert (IDL_NODE_TYPE (IDL_GENTREE ((yyvsp[(1) - (1)].tree)).data) == IDLN_IDENT);
	(yyval.tree) = IDL_GENTREE ((yyvsp[(1) - (1)].tree)).data;
}
    break;

  case 254:

/* Line 1455 of yacc.c  */
#line 1206 "./parser.y"
    {
	IDL_tree cur_top = IDL_GENTREE (IDL_NS (__IDL_root_ns).current).data;
	IDL_ns_pop_scope (__IDL_root_ns);
#ifdef YYDEBUG
	if (yydebug)
		fprintf (stderr, "ns: pop scope from `%s' to `%s'\n", 
		       IDL_IDENT(cur_top).str,
		       IDL_IDENT (IDL_GENTREE (IDL_NS (__IDL_root_ns).current).data).str);
#endif
}
    break;

  case 255:

/* Line 1455 of yacc.c  */
#line 1218 "./parser.y"
    {
	IDL_tree p;

	if ((p = IDL_ns_place_new (__IDL_root_ns, (yyvsp[(1) - (1)].tree))) == NULL) {
		IDL_tree q;
		int i;

		p = IDL_ns_lookup_cur_scope (__IDL_root_ns, (yyvsp[(1) - (1)].tree), NULL);

		for (i = 0, q = IDL_GENTREE (p).data;
		     q && (IDL_NODE_TYPE (q) == IDLN_IDENT ||
			   IDL_NODE_TYPE (q) == IDLN_LIST) && i < 4;
		     ++i)
			if (IDL_NODE_UP (q))
				q = IDL_NODE_UP (q);

		if (q) {
			IDL_tree_error ((yyvsp[(1) - (1)].tree), "`%s' conflicts", IDL_IDENT ((yyvsp[(1) - (1)].tree)).str);
			do_token_error (q, "with", FALSE);
		} else
			yyerrorv ("`%s' duplicate identifier", IDL_IDENT ((yyvsp[(1) - (1)].tree)).str);

		IDL_tree_free ((yyvsp[(1) - (1)].tree));
		YYABORT;
	}
	assert (IDL_IDENT ((yyvsp[(1) - (1)].tree))._ns_ref == p);
#ifdef REF_IDENTS
	++IDL_NODE_REFS (IDL_GENTREE (p).data);
#endif
	if (__IDL_new_ident_comments != NULL) {
		assert (IDL_IDENT ((yyvsp[(1) - (1)].tree)).comments == NULL);
		IDL_IDENT ((yyvsp[(1) - (1)].tree)).comments = __IDL_new_ident_comments;
		__IDL_new_ident_comments = NULL;
	}
	(yyval.tree) = p;
}
    break;

  case 256:

/* Line 1455 of yacc.c  */
#line 1256 "./parser.y"
    {
	IDL_tree p;

	if ((p = IDL_ns_resolve_ident (__IDL_root_ns, (yyvsp[(1) - (1)].tree))) == NULL) {
		yyerrorv ("`%s' undeclared identifier", IDL_IDENT ((yyvsp[(1) - (1)].tree)).str);
		IDL_tree_free ((yyvsp[(1) - (1)].tree));
		YYABORT;
	}
	IDL_tree_free ((yyvsp[(1) - (1)].tree));
	assert (IDL_GENTREE (p).data != NULL);
	assert (IDL_IDENT (IDL_GENTREE (p).data)._ns_ref == p);
#ifdef REF_IDENTS
	++IDL_NODE_REFS (IDL_GENTREE (p).data);
#endif
	(yyval.tree) = p;
}
    break;

  case 257:

/* Line 1455 of yacc.c  */
#line 1275 "./parser.y"
    {
	IDL_tree p;

	if ((p = IDL_ns_lookup_cur_scope (__IDL_root_ns, (yyvsp[(1) - (1)].tree), NULL)) == NULL) {
#ifdef YYDEBUG
		if (yydebug)
			fprintf (stderr, "ns: place_new `%s' in `%s'\n", 
		  	  IDL_IDENT((yyvsp[(1) - (1)].tree)).str,
			  IDL_IDENT(IDL_GENTREE (IDL_NS (__IDL_root_ns).current).data).str );
#endif
		p = IDL_ns_place_new (__IDL_root_ns, (yyvsp[(1) - (1)].tree));
		assert (p != NULL);
		assert (IDL_IDENT ((yyvsp[(1) - (1)].tree))._ns_ref == p);
		if (__IDL_new_ident_comments != NULL) {
			assert (IDL_IDENT ((yyvsp[(1) - (1)].tree)).comments == NULL);
			IDL_IDENT ((yyvsp[(1) - (1)].tree)).comments = __IDL_new_ident_comments;
			__IDL_new_ident_comments = NULL;
		}
	} else {
		IDL_tree_free ((yyvsp[(1) - (1)].tree));
		assert (IDL_GENTREE (p).data != NULL);
		assert (IDL_IDENT (IDL_GENTREE (p).data)._ns_ref == p);
	}
#ifdef REF_IDENTS
	++IDL_NODE_REFS (IDL_GENTREE (p).data);
#endif
	(yyval.tree) = p;
}
    break;

  case 258:

/* Line 1455 of yacc.c  */
#line 1305 "./parser.y"
    {
	IDL_tree p;

	if ((p = IDL_ns_lookup_this_scope (
		__IDL_root_ns,IDL_NS (__IDL_root_ns).file, (yyvsp[(1) - (1)].tree), NULL)) == NULL) {
		yyerrorv ("`%s' undeclared identifier", IDL_IDENT ((yyvsp[(1) - (1)].tree)).str);
		IDL_tree_free ((yyvsp[(1) - (1)].tree));
		YYABORT;
	}
	IDL_tree_free ((yyvsp[(1) - (1)].tree));
	assert (IDL_GENTREE (p).data != NULL);
	assert (IDL_IDENT (IDL_GENTREE (p).data)._ns_ref == p);
#ifdef REF_IDENTS
	++IDL_NODE_REFS (IDL_GENTREE (p).data);
#endif
	(yyval.tree) = p;
}
    break;

  case 259:

/* Line 1455 of yacc.c  */
#line 1324 "./parser.y"
    { (yyval.tree) = list_start ((yyvsp[(1) - (1)].tree), TRUE); }
    break;

  case 260:

/* Line 1455 of yacc.c  */
#line 1326 "./parser.y"
    { (yyval.tree) = list_chain ((yyvsp[(1) - (3)].tree), (yyvsp[(3) - (3)].tree), TRUE); }
    break;

  case 261:

/* Line 1455 of yacc.c  */
#line 1329 "./parser.y"
    {
	IDL_tree literal, ident = NULL;
	IDL_longlong_t value = 0;

	if ((literal = IDL_resolve_const_exp ((yyvsp[(1) - (1)].tree), IDLN_INTEGER))) {
		assert (IDL_NODE_TYPE (literal) == IDLN_INTEGER);
		++IDL_NODE_REFS (literal);
		value = IDL_INTEGER (literal).value;
		if ( literal != (yyvsp[(1) - (1)].tree) )
			IDL_tree_free ((yyvsp[(1) - (1)].tree));
	}

	if (literal && IDL_NODE_UP (literal) &&
	    IDL_NODE_TYPE (IDL_NODE_UP (literal)) == IDLN_CONST_DCL)
		ident = IDL_CONST_DCL (IDL_NODE_UP (literal)).ident;
	
	if (literal == NULL) {
		if (!(__IDL_flags & IDLF_NO_EVAL_CONST))
			yyerror ("Could not resolve constant expression");
		(yyval.tree) = (yyvsp[(1) - (1)].tree);
	} else if (value == 0) {
		yyerror ("Zero array size is illegal");
		if (ident)
			IDL_tree_error (ident, "From constant declared here");
		(yyval.tree) = NULL;
	} else if (value < 0) {
		yywarningv (IDL_WARNING1, "Cannot use negative value %"
			    IDL_LL "d, using %" IDL_LL "d",
			   value, -value);
		if (ident)
			IDL_tree_warning (ident,
					  IDL_WARNING1, "From constant declared here");
		(yyval.tree) = IDL_integer_new (-value);
	}
	else
		(yyval.tree) = IDL_integer_new (value);
}
    break;

  case 262:

/* Line 1455 of yacc.c  */
#line 1368 "./parser.y"
    { (yyval.declspec) = 0; }
    break;

  case 263:

/* Line 1455 of yacc.c  */
#line 1369 "./parser.y"
    {
	(yyval.declspec) = IDL_parse_declspec ((yyvsp[(1) - (1)].str));
	g_free ((yyvsp[(1) - (1)].str));
}
    break;

  case 264:

/* Line 1455 of yacc.c  */
#line 1375 "./parser.y"
    { (yyval.hash_table) = NULL; }
    break;

  case 265:

/* Line 1455 of yacc.c  */
#line 1376 "./parser.y"
    {
	/* Enable property scanning */
	if (__IDL_flags & IDLF_PROPERTIES)
		__IDL_flagsi |= IDLFP_PROPERTIES;
	else {
		yyerror ("Property syntax not enabled");
		YYABORT;
	}
}
    break;

  case 266:

/* Line 1455 of yacc.c  */
#line 1385 "./parser.y"
    { (yyval.hash_table) = (yyvsp[(3) - (4)].hash_table); }
    break;

  case 267:

/* Line 1455 of yacc.c  */
#line 1388 "./parser.y"
    { (yyval.tree) = IDL_integer_new ((yyvsp[(1) - (1)].integer)); }
    break;

  case 268:

/* Line 1455 of yacc.c  */
#line 1391 "./parser.y"
    { (yyval.tree) = IDL_string_new ((yyvsp[(1) - (1)].str)); }
    break;

  case 269:

/* Line 1455 of yacc.c  */
#line 1394 "./parser.y"
    { (yyval.tree) = IDL_char_new ((yyvsp[(1) - (1)].str)); }
    break;

  case 270:

/* Line 1455 of yacc.c  */
#line 1397 "./parser.y"
    { (yyval.tree) = IDL_fixed_new ((yyvsp[(1) - (1)].str)); }
    break;

  case 271:

/* Line 1455 of yacc.c  */
#line 1400 "./parser.y"
    { (yyval.tree) = IDL_float_new ((yyvsp[(1) - (1)].floatp)); }
    break;

  case 272:

/* Line 1455 of yacc.c  */
#line 1403 "./parser.y"
    { (yyval.tree) = IDL_boolean_new (TRUE); }
    break;

  case 273:

/* Line 1455 of yacc.c  */
#line 1404 "./parser.y"
    { (yyval.tree) = IDL_boolean_new (FALSE); }
    break;

  case 274:

/* Line 1455 of yacc.c  */
#line 1407 "./parser.y"
    {
	(yyval.tree) = (yyvsp[(2) - (2)].tree);
	assign_declspec ((yyval.tree), (yyvsp[(1) - (2)].declspec));
}
    break;

  case 275:

/* Line 1455 of yacc.c  */
#line 1413 "./parser.y"
    {
	(yyval.tree) = (yyvsp[(1) - (1)].tree);
}
    break;

  case 277:

/* Line 1455 of yacc.c  */
#line 1419 "./parser.y"
    {
	char *catstr = g_malloc (strlen ((yyvsp[(1) - (2)].str)) + strlen ((yyvsp[(2) - (2)].str)) + 1);
	strcpy (catstr, (yyvsp[(1) - (2)].str)); g_free ((yyvsp[(1) - (2)].str));
	strcat (catstr, (yyvsp[(2) - (2)].str)); g_free ((yyvsp[(2) - (2)].str));
	(yyval.str) = catstr;
}
    break;

  case 278:

/* Line 1455 of yacc.c  */
#line 1427 "./parser.y"
    {
	char *s = IDL_do_escapes ((yyvsp[(1) - (1)].str));
	g_free ((yyvsp[(1) - (1)].str));
	(yyval.str) = s;
}
    break;

  case 279:

/* Line 1455 of yacc.c  */
#line 1434 "./parser.y"
    {
	char *s = IDL_do_escapes ((yyvsp[(1) - (1)].str));
	g_free ((yyvsp[(1) - (1)].str));
	(yyval.str) = s;
}
    break;



/* Line 1455 of yacc.c  */
#line 3803 "y.tab.c"
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
#line 1441 "./parser.y"


void __IDL_parser_reset (void)
{
	yyclearin;
}

static const char *IDL_ns_get_cur_prefix (IDL_ns ns)
{
	IDL_tree p;

	p = IDL_NS (ns).current;

	assert (p != NULL);

	while (p && !IDL_GENTREE (p)._cur_prefix)
		p = IDL_NODE_UP (p);

	return p ? IDL_GENTREE (p)._cur_prefix : NULL;
}

gchar *IDL_ns_ident_make_repo_id (IDL_ns ns, IDL_tree p,
				  const char *p_prefix, int *major, int *minor)
{
	GString *s = g_string_new (NULL);
	const char *prefix;
	char *q;

	assert (p != NULL);
	
	if (IDL_NODE_TYPE (p) == IDLN_IDENT)
		p = IDL_IDENT_TO_NS (p);

	assert (p != NULL);

	prefix = p_prefix ? p_prefix : IDL_ns_get_cur_prefix (ns);

	q = IDL_ns_ident_to_qstring (p, "/", 0);
	g_string_printf (s, "IDL:%s%s%s:%d.%d",
			  prefix ? prefix : "",
			  prefix && *prefix ? "/" : "",
			  q,
			  major ? *major : 1,
			  minor ? *minor : 0);
	g_free (q);

	q = s->str;
	g_string_free (s, FALSE);

	return q;
}

static const char *get_name_token (const char *s, char **tok)
{
	const char *begin = s;
	int state = 0;

	if (!s)
		return NULL;

	while (g_ascii_isspace (*s)) ++s;
	
	while (1) switch (state) {
	case 0:		/* Unknown */
		if (*s == ':')
			state = 1;
		else if (isalnum ((int)*s) || *s == '_') {
			begin = s;
			state = 2;
		} else
			return NULL;
		break;
	case 1:		/* Scope */
		if (strncmp (s, "::", 2) == 0) {
			char *r = g_malloc (3);
			strcpy (r, "::");
			*tok = r;
			return s + 2;
		} else	/* Invalid */
			return NULL;
		break;
	case 2:
		if (isalnum ((int)*s) || *s == '_')
			++s;
		else {
			char *r = g_malloc (s - begin + 1);
			strncpy (r, begin, s - begin + 1);
			r[s - begin] = 0;
			*tok = r;
			return s;
		}
		break;
	}
}

static IDL_tree IDL_ns_pragma_parse_name (IDL_ns ns, const char *s)
{
	IDL_tree p = IDL_NS (ns).current, q;
	int start = 1;
	char *tok;

	/* This is a hack to allow directives for an ident (such
	 * as and interface) to be located within the scope of
	 * that identifier. */
	if ( p && (q=IDL_GENTREE(p).data)!=0
	  && IDL_NODE_TYPE(q)==IDLN_IDENT
	  && strcmp(s,IDL_IDENT(q).str)==0 ) {
		return p;
	}

	while (p && *s && (s = get_name_token (s, &tok))) {
		if (tok == NULL)
			return NULL;
		if (strcmp (tok, "::") == 0) {
			if (start) {
				/* Globally scoped */
				p = IDL_NS (ns).file;
			}
			g_free (tok);
		} else {
			IDL_tree ident = IDL_ident_new (tok);
			p = IDL_ns_lookup_this_scope (__IDL_root_ns, p, ident, NULL);
			IDL_tree_free (ident);
		}
		start = 0;
	}
	
	return p;
}

void IDL_ns_ID (IDL_ns ns, const char *s)
{
	char name[1024], id[1024];
	IDL_tree p, ident;
	int n;

	n = sscanf (s, "%1023s \"%1023s\"", name, id);
	if (n < 2 && __IDL_is_parsing) {
		yywarning (IDL_WARNING1, "Malformed pragma ID");
		return;
	}
	if (id[strlen (id) - 1] == '"')
		id[strlen (id) - 1] = 0;

	p = IDL_ns_pragma_parse_name (__IDL_root_ns, name);
	if (!p && __IDL_is_parsing) {
		yywarningv (IDL_WARNING1, "Unknown identifier `%s' in pragma ID", name);
		return;
	}

	/* We have resolved the identifier, so assign the repo id */
	assert (IDL_NODE_TYPE (p) == IDLN_GENTREE);
	assert (IDL_GENTREE (p).data != NULL);
	assert (IDL_NODE_TYPE (IDL_GENTREE (p).data) == IDLN_IDENT);
	ident = IDL_GENTREE (p).data;

	if (IDL_IDENT_REPO_ID (ident) != NULL)
		g_free (IDL_IDENT_REPO_ID (ident));

	IDL_IDENT_REPO_ID (ident) = g_strdup (id);
}

void IDL_ns_version (IDL_ns ns, const char *s)
{
	char name[1024];
	int n, major, minor;
	IDL_tree p, ident;

	n = sscanf (s, "%1023s %u.%u", name, &major, &minor);
	if (n < 3 && __IDL_is_parsing) {
		yywarning (IDL_WARNING1, "Malformed pragma version");
		return;
	}

	p = IDL_ns_pragma_parse_name (__IDL_root_ns, name);
	if (!p && __IDL_is_parsing) {
		yywarningv (IDL_WARNING1, "Unknown identifier `%s' in pragma version", name);
		return;
	}

	/* We have resolved the identifier, so assign the repo id */
	assert (IDL_NODE_TYPE (p) == IDLN_GENTREE);
	assert (IDL_GENTREE (p).data != NULL);
	assert (IDL_NODE_TYPE (IDL_GENTREE (p).data) == IDLN_IDENT);
	ident = IDL_GENTREE (p).data;

	if (IDL_IDENT_REPO_ID (ident) != NULL) {
		char *v = strrchr (IDL_IDENT_REPO_ID (ident), ':');
		if (v) {
			GString *s;

			*v = 0;
			s = g_string_new (NULL);
			g_string_printf (s, "%s:%d.%d",
					  IDL_IDENT_REPO_ID (ident), major, minor);
			g_free (IDL_IDENT_REPO_ID (ident));
			IDL_IDENT_REPO_ID (ident) = s->str;
			g_string_free (s, FALSE);
		} else if (__IDL_is_parsing)
			yywarningv (IDL_WARNING1, "Cannot find RepositoryID OMG IDL version in ID `%s'",
				    IDL_IDENT_REPO_ID (ident));
	} else
		IDL_IDENT_REPO_ID (ident) =
			IDL_ns_ident_make_repo_id (
				__IDL_root_ns, p, NULL, &major, &minor);
}

int IDL_inhibit_get (void)
{
	g_return_val_if_fail (__IDL_is_parsing, -1);

	return __IDL_inhibits;
}

void IDL_inhibit_push (void)
{
	g_return_if_fail (__IDL_is_parsing);

	++__IDL_inhibits;
}

void IDL_inhibit_pop (void)
{
	g_return_if_fail (__IDL_is_parsing);

	if (--__IDL_inhibits < 0)
		__IDL_inhibits = 0;
}

static void IDL_inhibit (IDL_ns ns, const char *s)
{
	if (g_ascii_strcasecmp ("push", s) == 0)
		IDL_inhibit_push ();
	else if (g_ascii_strcasecmp ("pop", s) == 0)
		IDL_inhibit_pop ();
}

static void IDL_typecodes_as_tok (IDL_ns ns, const char *s)
{
	if (g_ascii_strcasecmp ("push", s) == 0)
		++(__IDL_typecodes_as_tok);
	else if (g_ascii_strcasecmp ("pop", s) == 0)
		--(__IDL_typecodes_as_tok);
}

static void IDL_pidl (IDL_ns ns, const char *s)
{
	if (g_ascii_strcasecmp ("push", s) == 0)
		++(__IDL_pidl);
	else if (g_ascii_strcasecmp ("pop", s) == 0)
		--(__IDL_pidl);
}

void __IDL_do_pragma (const char *s)
{
	int n;
	char directive[256];

	g_return_if_fail (__IDL_is_parsing);
	g_return_if_fail (s != NULL);

	if (sscanf (s, "%255s%n", directive, &n) < 1)
		return;
	s += n;
	while (g_ascii_isspace (*s)) ++s;

	if (strcmp (directive, "prefix") == 0)
		IDL_ns_prefix (__IDL_root_ns, s);
	else if (strcmp (directive, "ID") == 0)
		IDL_ns_ID (__IDL_root_ns, s);
	else if (strcmp (directive, "version") == 0)
		IDL_ns_version (__IDL_root_ns, s);
	else if (strcmp (directive, "inhibit") == 0)
		IDL_inhibit (__IDL_root_ns, s);
	else if (strcmp (directive, "typecodes_as_tok") == 0)
		IDL_typecodes_as_tok (__IDL_root_ns, s);
	else if (strcmp (directive, "pidl") == 0)
		IDL_pidl (__IDL_root_ns, s);
}

static IDL_declspec_t IDL_parse_declspec (const char *strspec)
{
	IDL_declspec_t flags = IDLF_DECLSPEC_EXIST;

	if (strspec == NULL)
		return flags;

	if (strcmp (strspec, "inhibit") == 0)
		flags |= IDLF_DECLSPEC_INHIBIT;
	if (strcmp (strspec, "pidl") == 0)
		flags |= IDLF_DECLSPEC_PIDL;
	else if (__IDL_is_parsing)
		yywarningv (IDL_WARNING1, "Ignoring unknown declspec `%s'", strspec);

	return flags;
}

IDL_tree IDL_file_set (const char *filename, int line)
{
	IDL_fileinfo *fi;
	IDL_tree tree = NULL;

	g_return_val_if_fail (__IDL_is_parsing, NULL);

	if (filename) {
		const char *oldfilename = __IDL_cur_filename;
		gboolean wasInhibit = IS_INHIBIT_STATE();
		gboolean isTop = 
#ifdef HAVE_CPP_PIPE_STDIN
			strlen (filename)==0;
#else
			__IDL_tmp_filename &&
			strcmp (filename, __IDL_tmp_filename)==0;
#endif
		if ( isTop ) {
			filename = __IDL_real_filename;
			__IDL_flagsi &= ~IDLFP_IN_INCLUDES;
		} else {
			__IDL_flagsi |= IDLFP_IN_INCLUDES;
		}

		if ((fi=g_hash_table_lookup(__IDL_filename_hash, filename)) ) {
			__IDL_cur_fileinfo = fi;
			++(fi->seenCnt);
		} else {
			fi = g_new0 (IDL_fileinfo, 1);
			fi->name = g_strdup(filename);
			g_hash_table_insert (__IDL_filename_hash, fi->name, fi);
		}
		__IDL_cur_fileinfo = fi;
		__IDL_cur_filename = fi->name;
		if ( (__IDL_flags & IDLF_SRCFILES)!=0
		  && (oldfilename==0 
		  	    || strcmp(oldfilename,fi->name)!=0) ) {
			tree = IDL_srcfile_new(fi->name, fi->seenCnt, isTop, wasInhibit);
		}
	}

	if (__IDL_cur_line > 0)
		__IDL_cur_line = line;
	return tree;
}

void IDL_file_get (const char **filename, int *line)
{
	g_return_if_fail (__IDL_is_parsing);

	if (filename)
		*filename = __IDL_cur_filename;

	if (line)
		*line = __IDL_cur_line;
}

static int do_token_error (IDL_tree p, const char *message, gboolean prev)
{
	int dienow;
	char *what = NULL, *who = NULL;

	assert (p != NULL);

	dienow = IDL_tree_get_node_info (p, &what, &who);

	assert (what != NULL);
	
	if (who && *who)
		IDL_tree_error (p, "%s %s `%s'", message, what, who);
	else
		IDL_tree_error (p, "%s %s", message, what);
	
	return dienow;
}

static void illegal_context_type_error (IDL_tree p, const char *what)
{
	GString *s = g_string_new (NULL);

	g_string_printf (s, "Illegal type `%%s' for %s", what);
	illegal_type_error (p, s->str);
	g_string_free (s, TRUE);
}

static void illegal_type_error (IDL_tree p, const char *message)
{
	GString *s;

	s = IDL_tree_to_IDL_string (p, NULL, IDLF_OUTPUT_NO_NEWLINES);
	yyerrorv (message, s->str);
	g_string_free (s, TRUE);
}

static IDL_tree list_start (IDL_tree a, gboolean filter_null)
{
	IDL_tree p;

	if (!a && filter_null)
		return NULL;

	p = IDL_list_new (a);

	return p;
}

static IDL_tree list_chain (IDL_tree a, IDL_tree b, gboolean filter_null)
{
	IDL_tree p;

	if (filter_null) {
		if (!b)
			return a;
		if (!a)
			return list_start (b, filter_null);
	}

	p = IDL_list_new (b);
	a = IDL_list_concat (a, p);

	return a;
}

static IDL_tree zlist_chain (IDL_tree a, IDL_tree b, gboolean filter_null)
{
	if (a == NULL)
		return list_start (b, filter_null);
	else
		return list_chain (a, b, filter_null);
}

static int IDL_binop_chktypes (enum IDL_binop op, IDL_tree a, IDL_tree b)
{
	if (IDL_NODE_TYPE (a) != IDLN_BINOP &&
	    IDL_NODE_TYPE (b) != IDLN_BINOP &&
	    IDL_NODE_TYPE (a) != IDLN_UNARYOP &&
	    IDL_NODE_TYPE (b) != IDLN_UNARYOP &&
	    IDL_NODE_TYPE (a) != IDL_NODE_TYPE (b)) {
		yyerror ("Invalid mix of types in constant expression");
		return -1;
	}

	switch (op) {
	case IDL_BINOP_MULT:
	case IDL_BINOP_DIV:
	case IDL_BINOP_ADD:
	case IDL_BINOP_SUB:
		break;

	case IDL_BINOP_MOD:
	case IDL_BINOP_SHR:
	case IDL_BINOP_SHL:
	case IDL_BINOP_AND:
	case IDL_BINOP_OR:
	case IDL_BINOP_XOR:
		if ((IDL_NODE_TYPE (a) != IDLN_INTEGER ||
		     IDL_NODE_TYPE (b) != IDLN_INTEGER) &&
		    !(IDL_NODE_TYPE (a) == IDLN_BINOP ||
		      IDL_NODE_TYPE (b) == IDLN_BINOP ||
		      IDL_NODE_TYPE (a) == IDLN_UNARYOP ||
		      IDL_NODE_TYPE (b) == IDLN_UNARYOP)) {
			yyerror ("Invalid operation on non-integer value");
			return -1;
		}
		break;
	}

	return 0;
}

static int IDL_unaryop_chktypes (enum IDL_unaryop op, IDL_tree a)
{
	switch (op) {
	case IDL_UNARYOP_PLUS:
	case IDL_UNARYOP_MINUS:
		break;

	case IDL_UNARYOP_COMPLEMENT:
		if (IDL_NODE_TYPE (a) != IDLN_INTEGER &&
		    !(IDL_NODE_TYPE (a) == IDLN_BINOP ||
		      IDL_NODE_TYPE (a) == IDLN_UNARYOP)) {
			yyerror ("Operand to complement must be integer");
			return -1;
		}
		break;
	}

	return 0;
}

static IDL_tree IDL_binop_eval_integer (enum IDL_binop op, IDL_tree a, IDL_tree b)
{
	IDL_tree p = NULL;

	assert (IDL_NODE_TYPE (a) == IDLN_INTEGER);

	switch (op) {
	case IDL_BINOP_MULT:
		p = IDL_integer_new (IDL_INTEGER (a).value * IDL_INTEGER (b).value);
		break;

	case IDL_BINOP_DIV:
		if (IDL_INTEGER (b).value == 0) {
			yyerror ("Divide by zero in constant expression");
			return NULL;
		}
		p = IDL_integer_new (IDL_INTEGER (a).value / IDL_INTEGER (b).value);
		break;

	case IDL_BINOP_ADD:
		p = IDL_integer_new (IDL_INTEGER (a).value + IDL_INTEGER (b).value);
		break;

	case IDL_BINOP_SUB:
		p = IDL_integer_new (IDL_INTEGER (a).value - IDL_INTEGER (b).value);
		break;

	case IDL_BINOP_MOD:
		if (IDL_INTEGER (b).value == 0) {
			yyerror ("Modulo by zero in constant expression");
			return NULL;
		}
		p = IDL_integer_new (IDL_INTEGER (a).value % IDL_INTEGER (b).value);
		break;

	case IDL_BINOP_SHR:
		p = IDL_integer_new (IDL_INTEGER (a).value >> IDL_INTEGER (b).value);
		break;

	case IDL_BINOP_SHL:
		p = IDL_integer_new (IDL_INTEGER (a).value << IDL_INTEGER (b).value);
		break;

	case IDL_BINOP_AND:
		p = IDL_integer_new (IDL_INTEGER (a).value & IDL_INTEGER (b).value);
		break;

	case IDL_BINOP_OR:
		p = IDL_integer_new (IDL_INTEGER (a).value | IDL_INTEGER (b).value);
		break;

	case IDL_BINOP_XOR:
		p = IDL_integer_new (IDL_INTEGER (a).value ^ IDL_INTEGER (b).value);
		break;
	}

	return p;
}

static IDL_tree IDL_binop_eval_float (enum IDL_binop op, IDL_tree a, IDL_tree b)
{
	IDL_tree p = NULL;

	assert (IDL_NODE_TYPE (a) == IDLN_FLOAT);

	switch (op) {
	case IDL_BINOP_MULT:
		p = IDL_float_new (IDL_FLOAT (a).value * IDL_FLOAT (b).value);
		break;

	case IDL_BINOP_DIV:
		if (IDL_FLOAT (b).value == 0.0) {
			yyerror ("Divide by zero in constant expression");
			return NULL;
		}
		p = IDL_float_new (IDL_FLOAT (a).value / IDL_FLOAT (b).value);
		break;

	case IDL_BINOP_ADD:
		p = IDL_float_new (IDL_FLOAT (a).value + IDL_FLOAT (b).value);
		break;

	case IDL_BINOP_SUB:
		p = IDL_float_new (IDL_FLOAT (a).value - IDL_FLOAT (b).value);
		break;

	default:
		break;
	}

	return p;
}

static IDL_tree IDL_binop_eval (enum IDL_binop op, IDL_tree a, IDL_tree b)
{
	assert (IDL_NODE_TYPE (a) == IDL_NODE_TYPE (b));

	switch (IDL_NODE_TYPE (a)) {
	case IDLN_INTEGER: return IDL_binop_eval_integer (op, a, b);
	case IDLN_FLOAT: return IDL_binop_eval_float (op, a, b);
	default: return NULL;
	}
}

static IDL_tree IDL_unaryop_eval_integer (enum IDL_unaryop op, IDL_tree a)
{
	IDL_tree p = NULL;

	assert (IDL_NODE_TYPE (a) == IDLN_INTEGER);

	switch (op) {
	case IDL_UNARYOP_PLUS:
		p = IDL_integer_new (IDL_INTEGER (a).value);
		break;

	case IDL_UNARYOP_MINUS:
		p = IDL_integer_new (-IDL_INTEGER (a).value);
		break;

	case IDL_UNARYOP_COMPLEMENT:
		p = IDL_integer_new (~IDL_INTEGER (a).value);
		break;
	}
       
	return p;
}

static IDL_tree IDL_unaryop_eval_fixed (enum IDL_unaryop op, IDL_tree a)
{
	IDL_tree p = NULL;

	assert (IDL_NODE_TYPE (a) == IDLN_FIXED);

	switch (op) {
	case IDL_UNARYOP_PLUS:
		p = IDL_fixed_new (IDL_FIXED (a).value);
		break;

	default:
		break;
	}
       
	return p;
}

static IDL_tree IDL_unaryop_eval_float (enum IDL_unaryop op, IDL_tree a)
{
	IDL_tree p = NULL;

	assert (IDL_NODE_TYPE (a) == IDLN_FLOAT);

	switch (op) {
	case IDL_UNARYOP_PLUS:
		p = IDL_float_new (IDL_FLOAT (a).value);
		break;

	case IDL_UNARYOP_MINUS:
		p = IDL_float_new (-IDL_FLOAT (a).value);
		break;

	default:
		break;
	}
       
	return p;
}

static IDL_tree IDL_unaryop_eval (enum IDL_unaryop op, IDL_tree a)
{
	switch (IDL_NODE_TYPE (a)) {
	case IDLN_INTEGER: return IDL_unaryop_eval_integer (op, a);
	case IDLN_FIXED: return IDL_unaryop_eval_fixed (op, a);
	case IDLN_FLOAT: return IDL_unaryop_eval_float (op, a);
	default: return NULL;
	}
}

IDL_tree IDL_resolve_const_exp (IDL_tree p, IDL_tree_type type)
{
	gboolean resolved_value = FALSE, die = FALSE;
	gboolean wrong_type = FALSE;

	while (!resolved_value && !die) {
		if (IDL_NODE_TYPE (p) == IDLN_IDENT) {
			IDL_tree q = IDL_NODE_UP (p);
			
			assert (q != NULL);
			if (IDL_NODE_UP (q) &&
			    IDL_NODE_TYPE (IDL_NODE_UP (q)) == IDLN_TYPE_ENUM) {
				p = q;
				die = TRUE;
				break;
			} else if (IDL_NODE_TYPE (q) != IDLN_CONST_DCL) {
				p = q;
				wrong_type = TRUE;
				die = TRUE;
			} else
 				p = IDL_CONST_DCL (q).const_exp;
		}
		
		if (p == NULL ||
		    IDL_NODE_TYPE (p) == IDLN_BINOP ||
		    IDL_NODE_TYPE (p) == IDLN_UNARYOP) {
			die = TRUE;
			continue;
		}
		
		resolved_value = IDL_NODE_IS_LITERAL (p);
	}

	if (resolved_value &&
	    type != IDLN_ANY &&
	    IDL_NODE_TYPE (p) != type)
		wrong_type = TRUE;
	
	if (wrong_type) {
		yyerror ("Invalid type for constant");
		IDL_tree_error (p, "Previous resolved type declaration");
		return NULL;
	}

	return resolved_value ? p : NULL;
}

void IDL_queue_new_ident_comment (const char *str)
{
	g_return_if_fail (str != NULL);

	__IDL_new_ident_comments = g_slist_append (__IDL_new_ident_comments, g_strdup (str));
}

/*
 * Local variables:
 * mode: C
 * c-basic-offset: 8
 * tab-width: 8
 * indent-tabs-mode: t
 * End:
 */

