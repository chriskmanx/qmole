
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
#define yyparse         cmdyyparse
#define yylex           cmdyylex
#define yyerror         cmdyyerror
#define yylval          cmdyylval
#define yychar          cmdyychar
#define yydebug         cmdyydebug
#define yynerrs         cmdyynerrs


/* Copy the first part of user declarations.  */

/* Line 189 of yacc.c  */
#line 1 "src/cmdparse.y"

/*
 * vim:ts=4:sw=4:expandtab
 *
 * i3 - an improved dynamic tiling window manager
 * © 2009-2011 Michael Stapelberg and contributors (see also: LICENSE)
 *
 * cmdparse.y: the parser for commands you send to i3 (or bind on keys)
 *
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <float.h>

#include "all.h"

/** When the command did not include match criteria (!), we use the currently
 * focused command. Do not confuse this case with a command which included
 * criteria but which did not match any windows. This macro has to be called in
 * every command.
 */
#define HANDLE_EMPTY_MATCH do { \
    if (match_is_empty(&current_match)) { \
        owindow *ow = smalloc(sizeof(owindow)); \
        ow->con = focused; \
        TAILQ_INIT(&owindows); \
        TAILQ_INSERT_TAIL(&owindows, ow, owindows); \
    } \
} while (0)

typedef struct yy_buffer_state *YY_BUFFER_STATE;
extern int cmdyylex(struct context *context);
extern int cmdyyparse(void);
extern int cmdyylex_destroy(void);
extern FILE *cmdyyin;
YY_BUFFER_STATE cmdyy_scan_string(const char *);

static struct context *context;
static Match current_match;

/*
 * Helper data structure for an operation window (window on which the operation
 * will be performed). Used to build the TAILQ owindows.
 *
 */
typedef struct owindow {
    Con *con;
    TAILQ_ENTRY(owindow) owindows;
} owindow;
static TAILQ_HEAD(owindows_head, owindow) owindows;

/* Holds the JSON which will be returned via IPC or NULL for the default return
 * message */
static char *json_output;

/* We don’t need yydebug for now, as we got decent error messages using
 * yyerror(). Should you ever want to extend the parser, it might be handy
 * to just comment it in again, so it stays here. */
//int cmdyydebug = 1;

void cmdyyerror(const char *error_message) {
    ELOG("\n");
    ELOG("CMD: %s\n", error_message);
    ELOG("CMD: in command:\n");
    ELOG("CMD:   %s\n", context->line_copy);
    ELOG("CMD:   ");
    for (int c = 1; c <= context->last_column; c++)
        if (c >= context->first_column)
                printf("^");
        else printf(" ");
    printf("\n");
    ELOG("\n");
    context->compact_error = sstrdup(error_message);
}

int cmdyywrap() {
    return 1;
}

char *parse_cmd(const char *new) {
    json_output = NULL;
    LOG("COMMAND: *%s*\n", new);
    cmdyy_scan_string(new);

    match_init(&current_match);
    context = scalloc(sizeof(struct context));
    context->filename = "cmd";
    if (cmdyyparse() != 0) {
        fprintf(stderr, "Could not parse command\n");
        sasprintf(&json_output, "{\"success\":false, \"error\":\"%s at position %d\"}",
                  context->compact_error, context->first_column);
        FREE(context->line_copy);
        FREE(context->compact_error);
        free(context);
        return json_output;
    }
    printf("done, json output = %s\n", json_output);

    cmdyylex_destroy();
    FREE(context->line_copy);
    FREE(context->compact_error);
    free(context);
    return json_output;
}

/*
 * Returns true if a is definitely greater than b (using the given epsilon)
 *
 */
bool definitelyGreaterThan(float a, float b, float epsilon) {
    return (a - b) > ( (fabs(a) < fabs(b) ? fabs(b) : fabs(a)) * epsilon);
}



/* Line 189 of yacc.c  */
#line 199 "src/cmdparse.tab.c"

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
     TOK_EXEC = 258,
     TOK_EXIT = 259,
     TOK_RELOAD = 260,
     TOK_RESTART = 261,
     TOK_KILL = 262,
     TOK_WINDOW = 263,
     TOK_CLIENT = 264,
     TOK_FULLSCREEN = 265,
     TOK_GLOBAL = 266,
     TOK_LAYOUT = 267,
     TOK_DEFAULT = 268,
     TOK_STACKED = 269,
     TOK_TABBED = 270,
     TOK_BORDER = 271,
     TOK_NORMAL = 272,
     TOK_NONE = 273,
     TOK_1PIXEL = 274,
     TOK_MODE = 275,
     TOK_TILING = 276,
     TOK_FLOATING = 277,
     TOK_MODE_TOGGLE = 278,
     TOK_ENABLE = 279,
     TOK_DISABLE = 280,
     TOK_WORKSPACE = 281,
     TOK_OUTPUT = 282,
     TOK_TOGGLE = 283,
     TOK_FOCUS = 284,
     TOK_MOVE = 285,
     TOK_OPEN = 286,
     TOK_NEXT = 287,
     TOK_PREV = 288,
     TOK_SPLIT = 289,
     TOK_HORIZONTAL = 290,
     TOK_VERTICAL = 291,
     TOK_UP = 292,
     TOK_DOWN = 293,
     TOK_LEFT = 294,
     TOK_RIGHT = 295,
     TOK_PARENT = 296,
     TOK_CHILD = 297,
     TOK_APPEND_LAYOUT = 298,
     TOK_MARK = 299,
     TOK_RESIZE = 300,
     TOK_GROW = 301,
     TOK_SHRINK = 302,
     TOK_PX = 303,
     TOK_OR = 304,
     TOK_PPT = 305,
     TOK_NOP = 306,
     TOK_BACK_AND_FORTH = 307,
     TOK_NO_STARTUP_ID = 308,
     TOK_CLASS = 309,
     TOK_INSTANCE = 310,
     TOK_WINDOW_ROLE = 311,
     TOK_ID = 312,
     TOK_CON_ID = 313,
     TOK_TITLE = 314,
     STR = 315,
     NUMBER = 316
   };
#endif



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 214 of yacc.c  */
#line 121 "src/cmdparse.y"

    char *string;
    char chr;
    int number;



/* Line 214 of yacc.c  */
#line 304 "src/cmdparse.tab.c"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */


/* Line 264 of yacc.c  */
#line 316 "src/cmdparse.tab.c"

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
#define YYFINAL  6
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   115

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  69
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  42
/* YYNRULES -- Number of rules.  */
#define YYNRULES  103
/* YYNRULES -- Number of states.  */
#define YYNSTATES  142

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   316

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,    66,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,    62,
       2,    65,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    63,     2,    64,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,    67,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    68,     2,
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
      55,    56,    57,    58,    59,    60,    61
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     7,     9,    12,    13,    17,    19,    21,
      24,    26,    30,    34,    38,    42,    46,    50,    54,    56,
      60,    62,    64,    66,    68,    70,    72,    74,    76,    78,
      80,    82,    84,    86,    88,    90,    92,    94,    96,    98,
     102,   103,   105,   107,   109,   111,   113,   116,   119,   122,
     124,   126,   128,   130,   132,   135,   136,   138,   140,   143,
     146,   149,   152,   154,   157,   158,   160,   163,   165,   167,
     169,   171,   174,   176,   178,   180,   183,   185,   187,   189,
     191,   195,   199,   203,   207,   211,   214,   217,   219,   221,
     223,   226,   229,   235,   236,   239,   240,   244,   246,   248,
     250,   252,   254,   256
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      70,     0,    -1,    70,    62,    71,    -1,    71,    -1,    72,
      77,    -1,    -1,    73,    75,    74,    -1,    63,    -1,    64,
      -1,    75,    76,    -1,    76,    -1,    54,    65,    60,    -1,
      55,    65,    60,    -1,    56,    65,    60,    -1,    58,    65,
      60,    -1,    57,    65,    60,    -1,    44,    65,    60,    -1,
      59,    65,    60,    -1,    78,    -1,    77,    66,    78,    -1,
      79,    -1,    81,    -1,    83,    -1,    82,    -1,    97,    -1,
     101,    -1,   100,    -1,    99,    -1,    89,    -1,    84,    -1,
      87,    -1,    90,    -1,    91,    -1,    93,    -1,    95,    -1,
     103,    -1,   105,    -1,   104,    -1,   110,    -1,     3,    80,
      60,    -1,    -1,    53,    -1,     4,    -1,     5,    -1,     6,
      -1,    29,    -1,    29,   109,    -1,    29,    85,    -1,    29,
      86,    -1,    21,    -1,    22,    -1,    23,    -1,    41,    -1,
      42,    -1,     7,    88,    -1,    -1,     8,    -1,     9,    -1,
      26,    32,    -1,    26,    33,    -1,    26,    52,    -1,    26,
      60,    -1,    31,    -1,    10,    92,    -1,    -1,    11,    -1,
      34,    94,    -1,    35,    -1,    67,    -1,    36,    -1,    68,
      -1,    22,    96,    -1,    24,    -1,    25,    -1,    28,    -1,
      16,    98,    -1,    17,    -1,    18,    -1,    19,    -1,    28,
      -1,    30,   109,   106,    -1,    30,    26,    60,    -1,    30,
      26,    32,    -1,    30,    26,    33,    -1,    30,    27,    60,
      -1,    43,    60,    -1,    12,   102,    -1,    13,    -1,    14,
      -1,    15,    -1,    44,    60,    -1,    51,    60,    -1,    45,
     108,   109,   106,   107,    -1,    -1,    61,    48,    -1,    -1,
      49,    61,    50,    -1,    46,    -1,    47,    -1,    37,    -1,
      38,    -1,    39,    -1,    40,    -1,    20,    60,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   206,   206,   207,   222,   225,   226,   233,   249,   297,
     298,   302,   308,   314,   320,   335,   350,   356,   365,   366,
     370,   371,   372,   373,   374,   375,   376,   377,   378,   379,
     380,   381,   382,   383,   384,   385,   386,   387,   388,   392,
     404,   405,   409,   417,   429,   437,   483,   510,   542,   553,
     554,   555,   559,   560,   564,   584,   585,   586,   590,   595,
     600,   605,   630,   642,   659,   660,   664,   675,   676,   677,
     678,   682,   707,   708,   709,   713,   733,   734,   735,   736,
     740,   764,   787,   803,   819,   867,   877,   897,   898,   899,
     903,   922,   932,  1020,  1023,  1031,  1034,  1041,  1042,  1046,
    1047,  1048,  1049,  1053
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "\"exec\"", "\"exit\"", "\"reload\"",
  "\"restart\"", "\"kill\"", "\"window\"", "\"client\"", "\"fullscreen\"",
  "\"global\"", "\"layout\"", "\"default\"", "\"stacked\"", "\"tabbed\"",
  "\"border\"", "\"normal\"", "\"none\"", "\"1pixel\"", "\"mode\"",
  "\"tiling\"", "\"floating\"", "\"mode_toggle\"", "\"enable\"",
  "\"disable\"", "\"workspace\"", "\"output\"", "\"toggle\"", "\"focus\"",
  "\"move\"", "\"open\"", "\"next\"", "\"prev\"", "\"split\"",
  "\"horizontal\"", "\"vertical\"", "\"up\"", "\"down\"", "\"left\"",
  "\"right\"", "\"parent\"", "\"child\"", "\"append_layout\"", "\"mark\"",
  "\"resize\"", "\"grow\"", "\"shrink\"", "\"px\"", "\"or\"", "\"ppt\"",
  "\"nop\"", "\"back_and_forth\"", "\"--no-startup-id\"", "\"class\"",
  "\"instance\"", "\"window_role\"", "\"id\"", "\"con_id\"", "\"title\"",
  "\"<string>\"", "\"<number>\"", "';'", "'['", "']'", "'='", "','", "'h'",
  "'v'", "$accept", "commands", "command", "match", "matchstart",
  "matchend", "criteria", "criterion", "operations", "operation", "exec",
  "optional_no_startup_id", "exit", "reload", "restart", "focus",
  "window_mode", "level", "kill", "optional_kill_mode", "workspace",
  "open", "fullscreen", "fullscreen_mode", "split", "split_direction",
  "floating", "boolean", "border", "border_style", "move", "append_layout",
  "layout", "layout_mode", "mark", "nop", "resize", "resize_px",
  "resize_tiling", "resize_way", "direction", "mode", 0
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
     315,   316,    59,    91,    93,    61,    44,   104,   118
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    69,    70,    70,    71,    72,    72,    73,    74,    75,
      75,    76,    76,    76,    76,    76,    76,    76,    77,    77,
      78,    78,    78,    78,    78,    78,    78,    78,    78,    78,
      78,    78,    78,    78,    78,    78,    78,    78,    78,    79,
      80,    80,    81,    82,    83,    84,    84,    84,    84,    85,
      85,    85,    86,    86,    87,    88,    88,    88,    89,    89,
      89,    89,    90,    91,    92,    92,    93,    94,    94,    94,
      94,    95,    96,    96,    96,    97,    98,    98,    98,    98,
      99,    99,    99,    99,    99,   100,   101,   102,   102,   102,
     103,   104,   105,   106,   106,   107,   107,   108,   108,   109,
     109,   109,   109,   110
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     3,     1,     2,     0,     3,     1,     1,     2,
       1,     3,     3,     3,     3,     3,     3,     3,     1,     3,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     3,
       0,     1,     1,     1,     1,     1,     2,     2,     2,     1,
       1,     1,     1,     1,     2,     0,     1,     1,     2,     2,
       2,     2,     1,     2,     0,     1,     2,     1,     1,     1,
       1,     2,     1,     1,     1,     2,     1,     1,     1,     1,
       3,     3,     3,     3,     3,     2,     2,     1,     1,     1,
       2,     2,     5,     0,     2,     0,     3,     1,     1,     1,
       1,     1,     1,     2
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       5,     7,     0,     3,     0,     0,     1,     5,    40,    42,
      43,    44,    55,    64,     0,     0,     0,     0,     0,    45,
       0,    62,     0,     0,     0,     0,     0,     4,    18,    20,
      21,    23,    22,    29,    30,    28,    31,    32,    33,    34,
      24,    27,    26,    25,    35,    37,    36,    38,     0,     0,
       0,     0,     0,     0,     0,     0,    10,     2,    41,     0,
      56,    57,    54,    65,    63,    87,    88,    89,    86,    76,
      77,    78,    79,    75,   103,    72,    73,    74,    71,    58,
      59,    60,    61,    49,    50,    51,    99,   100,   101,   102,
      52,    53,    47,    48,    46,     0,     0,    93,    67,    69,
      68,    70,    66,    85,    90,    97,    98,     0,    91,     0,
       0,     0,     0,     0,     0,     0,     0,     8,     6,     9,
      39,    82,    83,    81,    84,     0,    80,    93,    19,    16,
      11,    12,    13,    15,    14,    17,    94,    95,     0,    92,
       0,    96
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     2,     3,     4,     5,   118,    55,    56,    27,    28,
      29,    59,    30,    31,    32,    33,    92,    93,    34,    62,
      35,    36,    37,    64,    38,   102,    39,    78,    40,    73,
      41,    42,    43,    68,    44,    45,    46,   126,   139,   107,
      94,    47
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -54
static const yytype_int8 yypact[] =
{
     -53,   -54,     1,   -54,    -1,    20,   -54,   -53,   -41,   -54,
     -54,   -54,    23,     5,    21,    67,   -40,    -2,   -15,    30,
      54,   -54,   -28,   -36,   -33,     8,   -22,   -18,   -54,   -54,
     -54,   -54,   -54,   -54,   -54,   -54,   -54,   -54,   -54,   -54,
     -54,   -54,   -54,   -54,   -54,   -54,   -54,   -54,   -16,    -3,
       0,    17,    18,    24,    25,     2,   -54,   -54,   -54,   -13,
     -54,   -54,   -54,   -54,   -54,   -54,   -54,   -54,   -54,   -54,
     -54,   -54,   -54,   -54,   -54,   -54,   -54,   -54,   -54,   -54,
     -54,   -54,   -54,   -54,   -54,   -54,   -54,   -54,   -54,   -54,
     -54,   -54,   -54,   -54,   -54,   -19,    13,    27,   -54,   -54,
     -54,   -54,   -54,   -54,   -54,   -54,   -54,    59,   -54,    -1,
      40,    41,    42,    43,    44,    45,    46,   -54,   -54,   -54,
     -54,   -54,   -54,   -54,   -54,    60,   -54,    27,   -54,   -54,
     -54,   -54,   -54,   -54,   -54,   -54,   -54,    58,    48,   -54,
      61,   -54
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -54,   -54,   103,   -54,   -54,   -54,   -54,    57,   -54,     4,
     -54,   -54,   -54,   -54,   -54,   -54,   -54,   -54,   -54,   -54,
     -54,   -54,   -54,   -54,   -54,   -54,   -54,   -54,   -54,   -54,
     -54,   -54,   -54,   -54,   -54,   -54,   -54,   -12,   -54,   -54,
     -20,   -54
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_uint8 yytable[] =
{
      97,     6,     8,     9,    10,    11,    12,    98,    99,    13,
       1,    14,    58,   121,   122,    15,    63,    79,    80,    16,
      74,    17,    75,    76,   103,    18,    77,   104,    19,    20,
      21,    60,    61,    22,    65,    66,    67,    81,   108,   100,
     101,   123,    23,    24,    25,    82,    48,   120,   109,   110,
      26,    83,    84,    85,   105,   106,    49,    50,    51,    52,
      53,    54,   111,     7,    48,   112,   117,    86,    87,    88,
      89,    90,    91,   124,    49,    50,    51,    52,    53,    54,
      95,    96,   113,   114,    69,    70,    71,   127,   125,   115,
     116,    86,    87,    88,    89,    72,    86,    87,    88,    89,
     129,   130,   131,   132,   133,   134,   135,   138,   136,   140,
      57,   141,   119,   128,     0,   137
};

static const yytype_int8 yycheck[] =
{
      20,     0,     3,     4,     5,     6,     7,    35,    36,    10,
      63,    12,    53,    32,    33,    16,    11,    32,    33,    20,
      60,    22,    24,    25,    60,    26,    28,    60,    29,    30,
      31,     8,     9,    34,    13,    14,    15,    52,    60,    67,
      68,    60,    43,    44,    45,    60,    44,    60,    66,    65,
      51,    21,    22,    23,    46,    47,    54,    55,    56,    57,
      58,    59,    65,    62,    44,    65,    64,    37,    38,    39,
      40,    41,    42,    60,    54,    55,    56,    57,    58,    59,
      26,    27,    65,    65,    17,    18,    19,   107,    61,    65,
      65,    37,    38,    39,    40,    28,    37,    38,    39,    40,
      60,    60,    60,    60,    60,    60,    60,    49,    48,    61,
       7,    50,    55,   109,    -1,   127
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    63,    70,    71,    72,    73,     0,    62,     3,     4,
       5,     6,     7,    10,    12,    16,    20,    22,    26,    29,
      30,    31,    34,    43,    44,    45,    51,    77,    78,    79,
      81,    82,    83,    84,    87,    89,    90,    91,    93,    95,
      97,    99,   100,   101,   103,   104,   105,   110,    44,    54,
      55,    56,    57,    58,    59,    75,    76,    71,    53,    80,
       8,     9,    88,    11,    92,    13,    14,    15,   102,    17,
      18,    19,    28,    98,    60,    24,    25,    28,    96,    32,
      33,    52,    60,    21,    22,    23,    37,    38,    39,    40,
      41,    42,    85,    86,   109,    26,    27,   109,    35,    36,
      67,    68,    94,    60,    60,    46,    47,   108,    60,    66,
      65,    65,    65,    65,    65,    65,    65,    64,    74,    76,
      60,    32,    33,    60,    60,    61,   106,   109,    78,    60,
      60,    60,    60,    60,    60,    60,    48,   106,    49,   107,
      61,    50
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
# define YYLEX yylex (context)
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
        case 3:

/* Line 1455 of yacc.c  */
#line 208 "src/cmdparse.y"
    {
        owindow *current;

        printf("single command completely parsed, dropping state...\n");
        while (!TAILQ_EMPTY(&owindows)) {
            current = TAILQ_FIRST(&owindows);
            TAILQ_REMOVE(&owindows, current, owindows);
            free(current);
        }
        match_init(&current_match);
    ;}
    break;

  case 6:

/* Line 1455 of yacc.c  */
#line 227 "src/cmdparse.y"
    {
        printf("match parsed\n");
    ;}
    break;

  case 7:

/* Line 1455 of yacc.c  */
#line 234 "src/cmdparse.y"
    {
        printf("start\n");
        match_init(&current_match);
        TAILQ_INIT(&owindows);
        /* copy all_cons */
        Con *con;
        TAILQ_FOREACH(con, &all_cons, all_cons) {
            owindow *ow = smalloc(sizeof(owindow));
            ow->con = con;
            TAILQ_INSERT_TAIL(&owindows, ow, owindows);
        }
    ;}
    break;

  case 8:

/* Line 1455 of yacc.c  */
#line 250 "src/cmdparse.y"
    {
        owindow *next, *current;

        printf("match specification finished, matching...\n");
        /* copy the old list head to iterate through it and start with a fresh
         * list which will contain only matching windows */
        struct owindows_head old = owindows;
        TAILQ_INIT(&owindows);
        for (next = TAILQ_FIRST(&old); next != TAILQ_END(&old);) {
            /* make a copy of the next pointer and advance the pointer to the
             * next element as we are going to invalidate the element’s
             * next/prev pointers by calling TAILQ_INSERT_TAIL later */
            current = next;
            next = TAILQ_NEXT(next, owindows);

            printf("checking if con %p / %s matches\n", current->con, current->con->name);
            if (current_match.con_id != NULL) {
                if (current_match.con_id == current->con) {
                    printf("matches container!\n");
                    TAILQ_INSERT_TAIL(&owindows, current, owindows);

                }
            } else if (current_match.mark != NULL && current->con->mark != NULL &&
                       regex_matches(current_match.mark, current->con->mark)) {
                printf("match by mark\n");
                TAILQ_INSERT_TAIL(&owindows, current, owindows);
            } else {
                if (current->con->window == NULL)
                    continue;
                if (match_matches_window(&current_match, current->con->window)) {
                    printf("matches window!\n");
                    TAILQ_INSERT_TAIL(&owindows, current, owindows);
                } else {
                    printf("doesnt match\n");
                    free(current);
                }
            }
        }

        TAILQ_FOREACH(current, &owindows, owindows) {
            printf("matching: %p / %s\n", current->con, current->con->name);
        }

    ;}
    break;

  case 11:

/* Line 1455 of yacc.c  */
#line 303 "src/cmdparse.y"
    {
        printf("criteria: class = %s\n", (yyvsp[(3) - (3)].string));
        current_match.class = regex_new((yyvsp[(3) - (3)].string));
        free((yyvsp[(3) - (3)].string));
    ;}
    break;

  case 12:

/* Line 1455 of yacc.c  */
#line 309 "src/cmdparse.y"
    {
        printf("criteria: instance = %s\n", (yyvsp[(3) - (3)].string));
        current_match.instance = regex_new((yyvsp[(3) - (3)].string));
        free((yyvsp[(3) - (3)].string));
    ;}
    break;

  case 13:

/* Line 1455 of yacc.c  */
#line 315 "src/cmdparse.y"
    {
        printf("criteria: window_role = %s\n", (yyvsp[(3) - (3)].string));
        current_match.role = regex_new((yyvsp[(3) - (3)].string));
        free((yyvsp[(3) - (3)].string));
    ;}
    break;

  case 14:

/* Line 1455 of yacc.c  */
#line 321 "src/cmdparse.y"
    {
        printf("criteria: id = %s\n", (yyvsp[(3) - (3)].string));
        char *end;
        long parsed = strtol((yyvsp[(3) - (3)].string), &end, 10);
        if (parsed == LONG_MIN ||
            parsed == LONG_MAX ||
            parsed < 0 ||
            (end && *end != '\0')) {
            ELOG("Could not parse con id \"%s\"\n", (yyvsp[(3) - (3)].string));
        } else {
            current_match.con_id = (Con*)parsed;
            printf("id as int = %p\n", current_match.con_id);
        }
    ;}
    break;

  case 15:

/* Line 1455 of yacc.c  */
#line 336 "src/cmdparse.y"
    {
        printf("criteria: window id = %s\n", (yyvsp[(3) - (3)].string));
        char *end;
        long parsed = strtol((yyvsp[(3) - (3)].string), &end, 10);
        if (parsed == LONG_MIN ||
            parsed == LONG_MAX ||
            parsed < 0 ||
            (end && *end != '\0')) {
            ELOG("Could not parse window id \"%s\"\n", (yyvsp[(3) - (3)].string));
        } else {
            current_match.id = parsed;
            printf("window id as int = %d\n", current_match.id);
        }
    ;}
    break;

  case 16:

/* Line 1455 of yacc.c  */
#line 351 "src/cmdparse.y"
    {
        printf("criteria: mark = %s\n", (yyvsp[(3) - (3)].string));
        current_match.mark = regex_new((yyvsp[(3) - (3)].string));
        free((yyvsp[(3) - (3)].string));
    ;}
    break;

  case 17:

/* Line 1455 of yacc.c  */
#line 357 "src/cmdparse.y"
    {
        printf("criteria: title = %s\n", (yyvsp[(3) - (3)].string));
        current_match.title = regex_new((yyvsp[(3) - (3)].string));
        free((yyvsp[(3) - (3)].string));
    ;}
    break;

  case 39:

/* Line 1455 of yacc.c  */
#line 393 "src/cmdparse.y"
    {
        char *command = (yyvsp[(3) - (3)].string);
        bool no_startup_id = (yyvsp[(2) - (3)].number);

        printf("should execute %s, no_startup_id = %d\n", command, no_startup_id);
        start_application(command, no_startup_id);
        free((yyvsp[(3) - (3)].string));
    ;}
    break;

  case 40:

/* Line 1455 of yacc.c  */
#line 404 "src/cmdparse.y"
    { (yyval.number) = false; ;}
    break;

  case 41:

/* Line 1455 of yacc.c  */
#line 405 "src/cmdparse.y"
    { (yyval.number) = true; ;}
    break;

  case 42:

/* Line 1455 of yacc.c  */
#line 410 "src/cmdparse.y"
    {
        printf("exit, bye bye\n");
        exit(0);
    ;}
    break;

  case 43:

/* Line 1455 of yacc.c  */
#line 418 "src/cmdparse.y"
    {
        printf("reloading\n");
        kill_configerror_nagbar(false);
        load_configuration(conn, NULL, true);
        x_set_i3_atoms();
        /* Send an IPC event just in case the ws names have changed */
        ipc_send_event("workspace", I3_IPC_EVENT_WORKSPACE, "{\"change\":\"reload\"}");
    ;}
    break;

  case 44:

/* Line 1455 of yacc.c  */
#line 430 "src/cmdparse.y"
    {
        printf("restarting i3\n");
        i3_restart(false);
    ;}
    break;

  case 45:

/* Line 1455 of yacc.c  */
#line 438 "src/cmdparse.y"
    {
        owindow *current;

        if (match_is_empty(&current_match)) {
            ELOG("You have to specify which window/container should be focused.\n");
            ELOG("Example: [class=\"urxvt\" title=\"irssi\"] focus\n");

            sasprintf(&json_output, "{\"success\":false, \"error\":\"You have to "
                      "specify which window/container should be focused\"}");
            break;
        }

        int count = 0;
        TAILQ_FOREACH(current, &owindows, owindows) {
            Con *ws = con_get_workspace(current->con);

            /* If the container is not on the current workspace,
             * workspace_show() will switch to a different workspace and (if
             * enabled) trigger a mouse pointer warp to the currently focused
             * container (!) on the target workspace.
             *
             * Therefore, before calling workspace_show(), we make sure that
             * 'current' will be focused on the workspace. However, we cannot
             * just con_focus(current) because then the pointer will not be
             * warped at all (the code thinks we are already there).
             *
             * So we focus 'current' to make it the currently focused window of
             * the target workspace, then revert focus. */
            Con *currently_focused = focused;
            con_focus(current->con);
            con_focus(currently_focused);

            /* Now switch to the workspace, then focus */
            workspace_show(ws);
            LOG("focusing %p / %s\n", current->con, current->con->name);
            con_focus(current->con);
            count++;
        }

        if (count > 1)
            LOG("WARNING: Your criteria for the focus command matches %d containers, "
                "while only exactly one container can be focused at a time.\n", count);

        tree_render();
    ;}
    break;

  case 46:

/* Line 1455 of yacc.c  */
#line 484 "src/cmdparse.y"
    {
        int direction = (yyvsp[(2) - (2)].number);
        switch (direction) {
            case TOK_LEFT:
                LOG("Focusing left\n");
                tree_next('p', HORIZ);
                break;
            case TOK_RIGHT:
                LOG("Focusing right\n");
                tree_next('n', HORIZ);
                break;
            case TOK_UP:
                LOG("Focusing up\n");
                tree_next('p', VERT);
                break;
            case TOK_DOWN:
                LOG("Focusing down\n");
                tree_next('n', VERT);
                break;
            default:
                ELOG("Invalid focus direction (%d)\n", direction);
                break;
        }

        tree_render();
    ;}
    break;

  case 47:

/* Line 1455 of yacc.c  */
#line 511 "src/cmdparse.y"
    {
        printf("should focus: ");

        if ((yyvsp[(2) - (2)].number) == TOK_TILING)
            printf("tiling\n");
        else if ((yyvsp[(2) - (2)].number) == TOK_FLOATING)
            printf("floating\n");
        else printf("mode toggle\n");

        Con *ws = con_get_workspace(focused);
        Con *current;
        if (ws != NULL) {
            int to_focus = (yyvsp[(2) - (2)].number);
            if ((yyvsp[(2) - (2)].number) == TOK_MODE_TOGGLE) {
                current = TAILQ_FIRST(&(ws->focus_head));
                if (current != NULL && current->type == CT_FLOATING_CON)
                    to_focus = TOK_TILING;
                else to_focus = TOK_FLOATING;
            }
            TAILQ_FOREACH(current, &(ws->focus_head), focused) {
                if ((to_focus == TOK_FLOATING && current->type != CT_FLOATING_CON) ||
                    (to_focus == TOK_TILING && current->type == CT_FLOATING_CON))
                    continue;

                con_focus(con_descend_focused(current));
                break;
            }
        }

        tree_render();
    ;}
    break;

  case 48:

/* Line 1455 of yacc.c  */
#line 543 "src/cmdparse.y"
    {
        if ((yyvsp[(2) - (2)].number) == TOK_PARENT)
            level_up();
        else level_down();

        tree_render();
    ;}
    break;

  case 49:

/* Line 1455 of yacc.c  */
#line 553 "src/cmdparse.y"
    { (yyval.number) = TOK_TILING; ;}
    break;

  case 50:

/* Line 1455 of yacc.c  */
#line 554 "src/cmdparse.y"
    { (yyval.number) = TOK_FLOATING; ;}
    break;

  case 51:

/* Line 1455 of yacc.c  */
#line 555 "src/cmdparse.y"
    { (yyval.number) = TOK_MODE_TOGGLE; ;}
    break;

  case 52:

/* Line 1455 of yacc.c  */
#line 559 "src/cmdparse.y"
    { (yyval.number) = TOK_PARENT; ;}
    break;

  case 53:

/* Line 1455 of yacc.c  */
#line 560 "src/cmdparse.y"
    { (yyval.number) = TOK_CHILD;  ;}
    break;

  case 54:

/* Line 1455 of yacc.c  */
#line 565 "src/cmdparse.y"
    {
        owindow *current;

        printf("killing!\n");
        /* check if the match is empty, not if the result is empty */
        if (match_is_empty(&current_match))
            tree_close_con((yyvsp[(2) - (2)].number));
        else {
            TAILQ_FOREACH(current, &owindows, owindows) {
                printf("matching: %p / %s\n", current->con, current->con->name);
                tree_close(current->con, (yyvsp[(2) - (2)].number), false, false);
            }
        }

        tree_render();
    ;}
    break;

  case 55:

/* Line 1455 of yacc.c  */
#line 584 "src/cmdparse.y"
    { (yyval.number) = KILL_WINDOW; ;}
    break;

  case 56:

/* Line 1455 of yacc.c  */
#line 585 "src/cmdparse.y"
    { (yyval.number) = KILL_WINDOW; ;}
    break;

  case 57:

/* Line 1455 of yacc.c  */
#line 586 "src/cmdparse.y"
    { (yyval.number) = KILL_CLIENT; ;}
    break;

  case 58:

/* Line 1455 of yacc.c  */
#line 591 "src/cmdparse.y"
    {
        workspace_show(workspace_next());
        tree_render();
    ;}
    break;

  case 59:

/* Line 1455 of yacc.c  */
#line 596 "src/cmdparse.y"
    {
        workspace_show(workspace_prev());
        tree_render();
    ;}
    break;

  case 60:

/* Line 1455 of yacc.c  */
#line 601 "src/cmdparse.y"
    {
        workspace_back_and_forth();
        tree_render();
    ;}
    break;

  case 61:

/* Line 1455 of yacc.c  */
#line 606 "src/cmdparse.y"
    {
        printf("should switch to workspace %s\n", (yyvsp[(2) - (2)].string));

        Con *ws = con_get_workspace(focused);

        /* Check if the command wants to switch to the current workspace */
        if (strcmp(ws->name, (yyvsp[(2) - (2)].string)) == 0) {
            printf("This workspace is already focused.\n");
            if (config.workspace_auto_back_and_forth) {
                workspace_back_and_forth();
                free((yyvsp[(2) - (2)].string));
                tree_render();
            }
            break;
        }

        workspace_show_by_name((yyvsp[(2) - (2)].string));
        free((yyvsp[(2) - (2)].string));

        tree_render();
    ;}
    break;

  case 62:

/* Line 1455 of yacc.c  */
#line 631 "src/cmdparse.y"
    {
        printf("opening new container\n");
        Con *con = tree_open_con(NULL, NULL);
        con_focus(con);
        sasprintf(&json_output, "{\"success\":true, \"id\":%ld}", (long int)con);

        tree_render();
    ;}
    break;

  case 63:

/* Line 1455 of yacc.c  */
#line 643 "src/cmdparse.y"
    {
        printf("toggling fullscreen, mode = %s\n", ((yyvsp[(2) - (2)].number) == CF_OUTPUT ? "normal" : "global"));
        owindow *current;

        HANDLE_EMPTY_MATCH;

        TAILQ_FOREACH(current, &owindows, owindows) {
            printf("matching: %p / %s\n", current->con, current->con->name);
            con_toggle_fullscreen(current->con, (yyvsp[(2) - (2)].number));
        }

        tree_render();
    ;}
    break;

  case 64:

/* Line 1455 of yacc.c  */
#line 659 "src/cmdparse.y"
    { (yyval.number) = CF_OUTPUT; ;}
    break;

  case 65:

/* Line 1455 of yacc.c  */
#line 660 "src/cmdparse.y"
    { (yyval.number) = CF_GLOBAL; ;}
    break;

  case 66:

/* Line 1455 of yacc.c  */
#line 665 "src/cmdparse.y"
    {
        /* TODO: use matches */
        printf("splitting in direction %c\n", (yyvsp[(2) - (2)].number));
        tree_split(focused, ((yyvsp[(2) - (2)].number) == 'v' ? VERT : HORIZ));

        tree_render();
    ;}
    break;

  case 67:

/* Line 1455 of yacc.c  */
#line 675 "src/cmdparse.y"
    { (yyval.number) = 'h'; ;}
    break;

  case 68:

/* Line 1455 of yacc.c  */
#line 676 "src/cmdparse.y"
    { (yyval.number) = 'h'; ;}
    break;

  case 69:

/* Line 1455 of yacc.c  */
#line 677 "src/cmdparse.y"
    { (yyval.number) = 'v'; ;}
    break;

  case 70:

/* Line 1455 of yacc.c  */
#line 678 "src/cmdparse.y"
    { (yyval.number) = 'v'; ;}
    break;

  case 71:

/* Line 1455 of yacc.c  */
#line 683 "src/cmdparse.y"
    {
        HANDLE_EMPTY_MATCH;

        owindow *current;
        TAILQ_FOREACH(current, &owindows, owindows) {
            printf("matching: %p / %s\n", current->con, current->con->name);
            if ((yyvsp[(2) - (2)].number) == TOK_TOGGLE) {
                printf("should toggle mode\n");
                toggle_floating_mode(current->con, false);
            } else {
                printf("should switch mode to %s\n", ((yyvsp[(2) - (2)].number) == TOK_FLOATING ? "floating" : "tiling"));
                if ((yyvsp[(2) - (2)].number) == TOK_ENABLE) {
                    floating_enable(current->con, false);
                } else {
                    floating_disable(current->con, false);
                }
            }
        }

        tree_render();
    ;}
    break;

  case 72:

/* Line 1455 of yacc.c  */
#line 707 "src/cmdparse.y"
    { (yyval.number) = TOK_ENABLE; ;}
    break;

  case 73:

/* Line 1455 of yacc.c  */
#line 708 "src/cmdparse.y"
    { (yyval.number) = TOK_DISABLE; ;}
    break;

  case 74:

/* Line 1455 of yacc.c  */
#line 709 "src/cmdparse.y"
    { (yyval.number) = TOK_TOGGLE; ;}
    break;

  case 75:

/* Line 1455 of yacc.c  */
#line 714 "src/cmdparse.y"
    {
        printf("border style should be changed to %d\n", (yyvsp[(2) - (2)].number));
        owindow *current;

        HANDLE_EMPTY_MATCH;

        TAILQ_FOREACH(current, &owindows, owindows) {
            printf("matching: %p / %s\n", current->con, current->con->name);
            if ((yyvsp[(2) - (2)].number) == TOK_TOGGLE) {
                current->con->border_style++;
                current->con->border_style %= 3;
            } else current->con->border_style = (yyvsp[(2) - (2)].number);
        }

        tree_render();
    ;}
    break;

  case 76:

/* Line 1455 of yacc.c  */
#line 733 "src/cmdparse.y"
    { (yyval.number) = BS_NORMAL; ;}
    break;

  case 77:

/* Line 1455 of yacc.c  */
#line 734 "src/cmdparse.y"
    { (yyval.number) = BS_NONE; ;}
    break;

  case 78:

/* Line 1455 of yacc.c  */
#line 735 "src/cmdparse.y"
    { (yyval.number) = BS_1PIXEL; ;}
    break;

  case 79:

/* Line 1455 of yacc.c  */
#line 736 "src/cmdparse.y"
    { (yyval.number) = TOK_TOGGLE; ;}
    break;

  case 80:

/* Line 1455 of yacc.c  */
#line 741 "src/cmdparse.y"
    {
        int direction = (yyvsp[(2) - (3)].number);
        int px = (yyvsp[(3) - (3)].number);

        /* TODO: make 'move' work with criteria. */
        printf("moving in direction %d\n", direction);
        if (con_is_floating(focused)) {
            printf("floating move with %d pixels\n", px);
            if (direction == TOK_LEFT) {
                focused->parent->rect.x -= px;
            } else if (direction == TOK_RIGHT) {
                focused->parent->rect.x += px;
            } else if (direction == TOK_UP) {
                focused->parent->rect.y -= px;
            } else if (direction == TOK_DOWN) {
                focused->parent->rect.y += px;
            }
        } else {
            tree_move(direction);
        }

        tree_render();
    ;}
    break;

  case 81:

/* Line 1455 of yacc.c  */
#line 765 "src/cmdparse.y"
    {
        owindow *current;

        /* Error out early to not create a non-existing workspace (in
         * workspace_get()) if we are not actually able to move anything. */
        if (match_is_empty(&current_match) && focused->type == CT_WORKSPACE)
            break;

        printf("should move window to workspace %s\n", (yyvsp[(3) - (3)].string));
        /* get the workspace */
        Con *ws = workspace_get((yyvsp[(3) - (3)].string), NULL);
        free((yyvsp[(3) - (3)].string));

        HANDLE_EMPTY_MATCH;

        TAILQ_FOREACH(current, &owindows, owindows) {
            printf("matching: %p / %s\n", current->con, current->con->name);
            con_move_to_workspace(current->con, ws, true, false);
        }

        tree_render();
    ;}
    break;

  case 82:

/* Line 1455 of yacc.c  */
#line 788 "src/cmdparse.y"
    {
        owindow *current;

        /* get the workspace */
        Con *ws = workspace_next();

        HANDLE_EMPTY_MATCH;

        TAILQ_FOREACH(current, &owindows, owindows) {
            printf("matching: %p / %s\n", current->con, current->con->name);
            con_move_to_workspace(current->con, ws, true, false);
        }

        tree_render();
    ;}
    break;

  case 83:

/* Line 1455 of yacc.c  */
#line 804 "src/cmdparse.y"
    {
        owindow *current;

        /* get the workspace */
        Con *ws = workspace_prev();

        HANDLE_EMPTY_MATCH;

        TAILQ_FOREACH(current, &owindows, owindows) {
            printf("matching: %p / %s\n", current->con, current->con->name);
            con_move_to_workspace(current->con, ws, true, false);
        }

        tree_render();
    ;}
    break;

  case 84:

/* Line 1455 of yacc.c  */
#line 820 "src/cmdparse.y"
    {
        owindow *current;

        printf("should move window to output %s", (yyvsp[(3) - (3)].string));

        HANDLE_EMPTY_MATCH;

        /* get the output */
        Output *current_output = NULL;
        Output *output;

        TAILQ_FOREACH(current, &owindows, owindows)
            current_output = get_output_containing(current->con->rect.x, current->con->rect.y);

        assert(current_output != NULL);

        if (strcasecmp((yyvsp[(3) - (3)].string), "up") == 0)
            output = get_output_next(D_UP, current_output);
        else if (strcasecmp((yyvsp[(3) - (3)].string), "down") == 0)
            output = get_output_next(D_DOWN, current_output);
        else if (strcasecmp((yyvsp[(3) - (3)].string), "left") == 0)
            output = get_output_next(D_LEFT, current_output);
        else if (strcasecmp((yyvsp[(3) - (3)].string), "right") == 0)
            output = get_output_next(D_RIGHT, current_output);
        else
            output = get_output_by_name((yyvsp[(3) - (3)].string));
        free((yyvsp[(3) - (3)].string));

        if (!output)
            break;

        /* get visible workspace on output */
        Con *ws = NULL;
        GREP_FIRST(ws, output_get_content(output->con), workspace_is_visible(child));
        if (!ws)
            break;

        TAILQ_FOREACH(current, &owindows, owindows) {
            printf("matching: %p / %s\n", current->con, current->con->name);
            con_move_to_workspace(current->con, ws, true, false);
        }

        tree_render();
    ;}
    break;

  case 85:

/* Line 1455 of yacc.c  */
#line 868 "src/cmdparse.y"
    {
        printf("restoring \"%s\"\n", (yyvsp[(2) - (2)].string));
        tree_append_json((yyvsp[(2) - (2)].string));
        free((yyvsp[(2) - (2)].string));
        tree_render();
    ;}
    break;

  case 86:

/* Line 1455 of yacc.c  */
#line 878 "src/cmdparse.y"
    {
        printf("changing layout to %d\n", (yyvsp[(2) - (2)].number));
        owindow *current;

        /* check if the match is empty, not if the result is empty */
        if (match_is_empty(&current_match))
            con_set_layout(focused->parent, (yyvsp[(2) - (2)].number));
        else {
            TAILQ_FOREACH(current, &owindows, owindows) {
                printf("matching: %p / %s\n", current->con, current->con->name);
                con_set_layout(current->con, (yyvsp[(2) - (2)].number));
            }
        }

        tree_render();
    ;}
    break;

  case 87:

/* Line 1455 of yacc.c  */
#line 897 "src/cmdparse.y"
    { (yyval.number) = L_DEFAULT; ;}
    break;

  case 88:

/* Line 1455 of yacc.c  */
#line 898 "src/cmdparse.y"
    { (yyval.number) = L_STACKED; ;}
    break;

  case 89:

/* Line 1455 of yacc.c  */
#line 899 "src/cmdparse.y"
    { (yyval.number) = L_TABBED; ;}
    break;

  case 90:

/* Line 1455 of yacc.c  */
#line 904 "src/cmdparse.y"
    {
        printf("marking window with str %s\n", (yyvsp[(2) - (2)].string));
        owindow *current;

        HANDLE_EMPTY_MATCH;

        TAILQ_FOREACH(current, &owindows, owindows) {
            printf("matching: %p / %s\n", current->con, current->con->name);
            current->con->mark = sstrdup((yyvsp[(2) - (2)].string));
        }

        free((yyvsp[(2) - (2)].string));

        tree_render();
    ;}
    break;

  case 91:

/* Line 1455 of yacc.c  */
#line 923 "src/cmdparse.y"
    {
        printf("-------------------------------------------------\n");
        printf("  NOP: %s\n", (yyvsp[(2) - (2)].string));
        printf("-------------------------------------------------\n");
        free((yyvsp[(2) - (2)].string));
    ;}
    break;

  case 92:

/* Line 1455 of yacc.c  */
#line 933 "src/cmdparse.y"
    {
        /* resize <grow|shrink> <direction> [<px> px] [or <ppt> ppt] */
        printf("resizing in way %d, direction %d, px %d or ppt %d\n", (yyvsp[(2) - (5)].number), (yyvsp[(3) - (5)].number), (yyvsp[(4) - (5)].number), (yyvsp[(5) - (5)].number));
        int direction = (yyvsp[(3) - (5)].number);
        int px = (yyvsp[(4) - (5)].number);
        int ppt = (yyvsp[(5) - (5)].number);
        if ((yyvsp[(2) - (5)].number) == TOK_SHRINK) {
            px *= -1;
            ppt *= -1;
        }

        if (con_is_floating(focused)) {
            printf("floating resize\n");
            if (direction == TOK_UP) {
                focused->parent->rect.y -= px;
                focused->parent->rect.height += px;
            } else if (direction == TOK_DOWN) {
                focused->parent->rect.height += px;
            } else if (direction == TOK_LEFT) {
                focused->parent->rect.x -= px;
                focused->parent->rect.width += px;
            } else {
                focused->parent->rect.width += px;
            }
        } else {
            LOG("tiling resize\n");
            /* get the appropriate current container (skip stacked/tabbed cons) */
            Con *current = focused;
            while (current->parent->layout == L_STACKED ||
                   current->parent->layout == L_TABBED)
                current = current->parent;
            /* get the default percentage */
            int children = con_num_children(current->parent);
            Con *other;
            LOG("ins. %d children\n", children);
            double percentage = 1.0 / children;
            LOG("default percentage = %f\n", percentage);

            orientation_t orientation = current->parent->orientation;

            if ((orientation == HORIZ &&
                 (direction == TOK_UP || direction == TOK_DOWN)) ||
                (orientation == VERT &&
                 (direction == TOK_LEFT || direction == TOK_RIGHT))) {
                LOG("You cannot resize in that direction. Your focus is in a %s split container currently.\n",
                    (orientation == HORIZ ? "horizontal" : "vertical"));
                break;
            }

            if (direction == TOK_UP || direction == TOK_LEFT) {
                other = TAILQ_PREV(current, nodes_head, nodes);
            } else {
                other = TAILQ_NEXT(current, nodes);
            }
            if (other == TAILQ_END(workspaces)) {
                LOG("No other container in this direction found, cannot resize.\n");
                break;
            }
            LOG("other->percent = %f\n", other->percent);
            LOG("current->percent before = %f\n", current->percent);
            if (current->percent == 0.0)
                current->percent = percentage;
            if (other->percent == 0.0)
                other->percent = percentage;
            double new_current_percent = current->percent + ((double)ppt / 100.0);
            double new_other_percent = other->percent - ((double)ppt / 100.0);
            LOG("new_current_percent = %f\n", new_current_percent);
            LOG("new_other_percent = %f\n", new_other_percent);
            /* Ensure that the new percentages are positive and greater than
             * 0.05 to have a reasonable minimum size. */
            if (definitelyGreaterThan(new_current_percent, 0.05, DBL_EPSILON) &&
                definitelyGreaterThan(new_other_percent, 0.05, DBL_EPSILON)) {
                current->percent += ((double)ppt / 100.0);
                other->percent -= ((double)ppt / 100.0);
                LOG("current->percent after = %f\n", current->percent);
                LOG("other->percent after = %f\n", other->percent);
            } else {
                LOG("Not resizing, already at minimum size\n");
            }
        }

        tree_render();
    ;}
    break;

  case 93:

/* Line 1455 of yacc.c  */
#line 1020 "src/cmdparse.y"
    {
        (yyval.number) = 10;
    ;}
    break;

  case 94:

/* Line 1455 of yacc.c  */
#line 1024 "src/cmdparse.y"
    {
        (yyval.number) = (yyvsp[(1) - (2)].number);
    ;}
    break;

  case 95:

/* Line 1455 of yacc.c  */
#line 1031 "src/cmdparse.y"
    {
        (yyval.number) = 10;
    ;}
    break;

  case 96:

/* Line 1455 of yacc.c  */
#line 1035 "src/cmdparse.y"
    {
        (yyval.number) = (yyvsp[(2) - (3)].number);
    ;}
    break;

  case 97:

/* Line 1455 of yacc.c  */
#line 1041 "src/cmdparse.y"
    { (yyval.number) = TOK_GROW; ;}
    break;

  case 98:

/* Line 1455 of yacc.c  */
#line 1042 "src/cmdparse.y"
    { (yyval.number) = TOK_SHRINK; ;}
    break;

  case 99:

/* Line 1455 of yacc.c  */
#line 1046 "src/cmdparse.y"
    { (yyval.number) = TOK_UP; ;}
    break;

  case 100:

/* Line 1455 of yacc.c  */
#line 1047 "src/cmdparse.y"
    { (yyval.number) = TOK_DOWN; ;}
    break;

  case 101:

/* Line 1455 of yacc.c  */
#line 1048 "src/cmdparse.y"
    { (yyval.number) = TOK_LEFT; ;}
    break;

  case 102:

/* Line 1455 of yacc.c  */
#line 1049 "src/cmdparse.y"
    { (yyval.number) = TOK_RIGHT; ;}
    break;

  case 103:

/* Line 1455 of yacc.c  */
#line 1054 "src/cmdparse.y"
    {
        switch_mode((yyvsp[(2) - (2)].string));
    ;}
    break;



/* Line 1455 of yacc.c  */
#line 2779 "src/cmdparse.tab.c"
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



