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
     ALT_TOK = 258,
     APP_TOK = 259,
     BACK_TOK = 260,
     BORDER_TOK = 261,
     BTN1_CLICK2_TOK = 262,
     BTN1_CLICK_TOK = 263,
     BTN1_DOWN_TOK = 264,
     BTN1_UP_TOK = 265,
     BTN2_CLICK2_TOK = 266,
     BTN2_CLICK_TOK = 267,
     BTN2_DOWN_TOK = 268,
     BTN2_UP_TOK = 269,
     BTN3_CLICK2_TOK = 270,
     BTN3_CLICK_TOK = 271,
     BTN3_DOWN_TOK = 272,
     BTN3_UP_TOK = 273,
     BTN4_CLICK2_TOK = 274,
     BTN4_CLICK_TOK = 275,
     BTN4_DOWN_TOK = 276,
     BTN4_UP_TOK = 277,
     BTN5_CLICK2_TOK = 278,
     BTN5_CLICK_TOK = 279,
     BTN5_DOWN_TOK = 280,
     BTN5_UP_TOK = 281,
     BUTTONS_TOK = 282,
     CTRL_TOK = 283,
     FBEEP_TOK = 284,
     FCIRCLE_DOWN_TOK = 285,
     FCIRCLE_UP_TOK = 286,
     FEXEC_TOK = 287,
     FFOCUS_COLOR_TOK = 288,
     FFOCUS_KEY_TOK = 289,
     FKILL_TOK = 290,
     FLOWER_TOK = 291,
     FMAXIMIZE_TOK = 292,
     FMENU_TOK = 293,
     FMINIMIZE_TOK = 294,
     FMOVE_TOK = 295,
     FNEXT_CMAP_TOK = 296,
     FNEXT_KEY_TOK = 297,
     FNOP_TOK = 298,
     FNORMALIZE_TOK = 299,
     FNORMANDRAISE_TOK = 300,
     FPACK_ICONS_TOK = 301,
     FPASS_KEYS_TOK = 302,
     FPOST_WMENU_TOK = 303,
     FPREV_CMAP_TOK = 304,
     FPREV_KEY_TOK = 305,
     FQUIT_MWM_TOK = 306,
     FRAISE_LOWER_TOK = 307,
     FRAISE_TOK = 308,
     FRAME_TOK = 309,
     FREE_FAMILY_TOK = 310,
     FREFRESH_TOK = 311,
     FREFRESH_WIN_TOK = 312,
     FRESIZE_TOK = 313,
     FRESTART_TOK = 314,
     FRESTOREANDRAISE_TOK = 315,
     FRESTORE_TOK = 316,
     FSCREEN_TOK = 317,
     FSEND_MSG_TOK = 318,
     FSEPARATOR_TOK = 319,
     FSET_BEHAVIOR_TOK = 320,
     FTITLE_TOK = 321,
     FWINDOWLIST_TOK = 322,
     FDESK_TOK = 323,
     FTOGGLE_PAGE_TOK = 324,
     FGOTO_PAGE_TOK = 325,
     ICON_TOK = 326,
     KEY_TOK = 327,
     KEYS_TOK = 328,
     LOCK_TOK = 329,
     MENU_TOK = 330,
     MENUB_TOK = 331,
     MINIMIZEB_TOK = 332,
     MAXIMIZEB_TOK = 333,
     MOD1_TOK = 334,
     MOD2_TOK = 335,
     MOD3_TOK = 336,
     MOD4_TOK = 337,
     MOD5_TOK = 338,
     NEXT_TOK = 339,
     PREV_TOK = 340,
     ROOT_TOK = 341,
     SHIFT_TOK = 342,
     TITLE_TOK = 343,
     TRANSIENT_TOK = 344,
     WINDOW_TOK = 345,
     WITHIN_TOK = 346,
     STRING_TOK = 347
   };
#endif
/* Tokens.  */
#define ALT_TOK 258
#define APP_TOK 259
#define BACK_TOK 260
#define BORDER_TOK 261
#define BTN1_CLICK2_TOK 262
#define BTN1_CLICK_TOK 263
#define BTN1_DOWN_TOK 264
#define BTN1_UP_TOK 265
#define BTN2_CLICK2_TOK 266
#define BTN2_CLICK_TOK 267
#define BTN2_DOWN_TOK 268
#define BTN2_UP_TOK 269
#define BTN3_CLICK2_TOK 270
#define BTN3_CLICK_TOK 271
#define BTN3_DOWN_TOK 272
#define BTN3_UP_TOK 273
#define BTN4_CLICK2_TOK 274
#define BTN4_CLICK_TOK 275
#define BTN4_DOWN_TOK 276
#define BTN4_UP_TOK 277
#define BTN5_CLICK2_TOK 278
#define BTN5_CLICK_TOK 279
#define BTN5_DOWN_TOK 280
#define BTN5_UP_TOK 281
#define BUTTONS_TOK 282
#define CTRL_TOK 283
#define FBEEP_TOK 284
#define FCIRCLE_DOWN_TOK 285
#define FCIRCLE_UP_TOK 286
#define FEXEC_TOK 287
#define FFOCUS_COLOR_TOK 288
#define FFOCUS_KEY_TOK 289
#define FKILL_TOK 290
#define FLOWER_TOK 291
#define FMAXIMIZE_TOK 292
#define FMENU_TOK 293
#define FMINIMIZE_TOK 294
#define FMOVE_TOK 295
#define FNEXT_CMAP_TOK 296
#define FNEXT_KEY_TOK 297
#define FNOP_TOK 298
#define FNORMALIZE_TOK 299
#define FNORMANDRAISE_TOK 300
#define FPACK_ICONS_TOK 301
#define FPASS_KEYS_TOK 302
#define FPOST_WMENU_TOK 303
#define FPREV_CMAP_TOK 304
#define FPREV_KEY_TOK 305
#define FQUIT_MWM_TOK 306
#define FRAISE_LOWER_TOK 307
#define FRAISE_TOK 308
#define FRAME_TOK 309
#define FREE_FAMILY_TOK 310
#define FREFRESH_TOK 311
#define FREFRESH_WIN_TOK 312
#define FRESIZE_TOK 313
#define FRESTART_TOK 314
#define FRESTOREANDRAISE_TOK 315
#define FRESTORE_TOK 316
#define FSCREEN_TOK 317
#define FSEND_MSG_TOK 318
#define FSEPARATOR_TOK 319
#define FSET_BEHAVIOR_TOK 320
#define FTITLE_TOK 321
#define FWINDOWLIST_TOK 322
#define FDESK_TOK 323
#define FTOGGLE_PAGE_TOK 324
#define FGOTO_PAGE_TOK 325
#define ICON_TOK 326
#define KEY_TOK 327
#define KEYS_TOK 328
#define LOCK_TOK 329
#define MENU_TOK 330
#define MENUB_TOK 331
#define MINIMIZEB_TOK 332
#define MAXIMIZEB_TOK 333
#define MOD1_TOK 334
#define MOD2_TOK 335
#define MOD3_TOK 336
#define MOD4_TOK 337
#define MOD5_TOK 338
#define NEXT_TOK 339
#define PREV_TOK 340
#define ROOT_TOK 341
#define SHIFT_TOK 342
#define TITLE_TOK 343
#define TRANSIENT_TOK 344
#define WINDOW_TOK 345
#define WITHIN_TOK 346
#define STRING_TOK 347




/* Copy the first part of user declarations.  */
#line 1 "mwmparse.y"

/**
 *
 * $Id: mwmparse.y,v 1.1 2004/08/28 19:27:39 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2002 LessTif Development Team
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
 **/

#include "LTconfig.h"

#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#else
#error "you lose (I don't know how to fix this)"
#endif
#include <X11/Xfuncs.h>

#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>

#include "mwm.h"


extern int yydebug;
extern int yylex(void);

static int num_items;
static MenuRoot *cur_menu;
static ScreenInfo *pscr;
static Boolean skip_test;
static Boolean button_bind_match;
static Boolean button_bind_found;
static Boolean key_bind_match;
static Boolean key_bind_found;

/*
 * this must be here for the parser
 */
static int lineno;



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
#line 228 "mwmparse.y"
{
    char	*string;
    int		number;
    KeySym	key;
    struct {
	int func;
	char *arg;
    } function;
    struct {
	int type;
	char *string;
    } label;
    long	modifiers;
    struct {
	int button;
	int event;
	int count;
	int modifiers;
    } button;
    struct {
	int modifiers;
	KeySym key;
    } hotkey;
}
/* Line 187 of yacc.c.  */
#line 378 "mwmparse.c"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 391 "mwmparse.c"

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
#define YYFINAL  2
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   385

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  102
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  30
/* YYNRULES -- Number of rules.  */
#define YYNRULES  133
/* YYNRULES -- Number of states.  */
#define YYNSTATES  176

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   347

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   100,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,   101,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      96,     2,    97,     2,    98,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,    99,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    93,    95,    94,     2,     2,     2,     2,
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
      85,    86,    87,    88,    89,    90,    91,    92
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     6,     7,     9,    11,    13,    14,    21,
      24,    26,    27,    31,    32,    39,    42,    44,    45,    49,
      50,    57,    60,    62,    63,    68,    70,    71,    75,    77,
      79,    81,    83,    85,    87,    89,    91,    93,    95,    97,
      99,   104,   108,   114,   119,   122,   124,   126,   128,   130,
     132,   134,   136,   138,   140,   142,   144,   146,   148,   150,
     152,   154,   156,   158,   160,   162,   164,   166,   168,   170,
     172,   174,   176,   178,   180,   182,   184,   186,   188,   190,
     193,   196,   197,   199,   202,   205,   208,   211,   213,   215,
     217,   220,   222,   225,   227,   229,   231,   234,   236,   238,
     240,   242,   244,   246,   248,   251,   253,   256,   259,   261,
     263,   265,   267,   269,   272,   275,   278,   280,   282,   284,
     286,   288,   290,   291,   294,   296,   298,   300,   302,   304,
     306,   308,   310,   312
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
     103,     0,    -1,   103,   104,    -1,    -1,   105,    -1,   109,
      -1,   113,    -1,    -1,    27,   131,   106,    93,   107,    94,
      -1,   107,   108,    -1,     1,    -1,    -1,   120,   118,   128,
      -1,    -1,    73,   131,   110,    93,   111,    94,    -1,   111,
     112,    -1,     1,    -1,    -1,   121,   118,   128,    -1,    -1,
      75,   131,   114,    93,   115,    94,    -1,   115,   116,    -1,
       1,    -1,    -1,   125,   127,   117,   128,    -1,   121,    -1,
      -1,   118,    95,   119,    -1,   119,    -1,    86,    -1,    71,
      -1,    76,    -1,    77,    -1,    78,    -1,    90,    -1,    88,
      -1,    54,    -1,     6,    -1,     4,    -1,     1,    -1,   122,
      96,   124,    97,    -1,    96,   124,    97,    -1,   122,    96,
      72,    97,    92,    -1,    96,    72,    97,    92,    -1,   122,
     123,    -1,   123,    -1,    28,    -1,    87,    -1,     3,    -1,
      74,    -1,    79,    -1,    80,    -1,    81,    -1,    82,    -1,
      83,    -1,     9,    -1,    10,    -1,     8,    -1,     7,    -1,
      13,    -1,    14,    -1,    12,    -1,    11,    -1,    17,    -1,
      18,    -1,    16,    -1,    15,    -1,    21,    -1,    22,    -1,
      20,    -1,    19,    -1,    25,    -1,    26,    -1,    24,    -1,
      23,    -1,     1,    -1,   131,    -1,   126,    -1,     1,    -1,
      98,   131,    -1,    99,   131,    -1,    -1,    29,    -1,    30,
     129,    -1,    31,   129,    -1,    32,   131,    -1,   100,   131,
      -1,    33,    -1,    34,    -1,    35,    -1,    36,   129,    -1,
      37,    -1,    38,   131,    -1,    39,    -1,    40,    -1,    41,
      -1,    42,   129,    -1,    43,    -1,    44,    -1,    45,    -1,
      46,    -1,    47,    -1,    48,    -1,    49,    -1,    50,   129,
      -1,    51,    -1,    53,   129,    -1,    52,   129,    -1,    56,
      -1,    57,    -1,    58,    -1,    61,    -1,    60,    -1,    59,
     129,    -1,    62,   129,    -1,    63,   131,    -1,    64,    -1,
      65,    -1,    66,    -1,    67,    -1,     1,    -1,   130,    -1,
      -1,   101,   131,    -1,    86,    -1,    90,    -1,    89,    -1,
      71,    -1,    91,    -1,    55,    -1,    84,    -1,    85,    -1,
       5,    -1,    92,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   254,   254,   255,   258,   259,   260,   264,   263,   285,
     286,   292,   295,   402,   401,   423,   424,   430,   433,   486,
     485,   496,   497,   503,   506,   608,   611,   614,   616,   620,
     622,   624,   626,   628,   630,   632,   634,   636,   638,   640,
     644,   649,   656,   661,   668,   670,   674,   676,   678,   680,
     682,   684,   686,   688,   690,   694,   700,   706,   712,   718,
     724,   730,   736,   742,   748,   754,   760,   766,   772,   778,
     784,   790,   796,   802,   808,   814,   823,   825,   827,   835,
     839,   850,   853,   858,   863,   868,   873,   878,   883,   888,
     893,   898,   903,   908,   913,   918,   923,   928,   933,   938,
     943,   948,   953,   958,   963,   968,   973,   978,   983,   988,
     993,   998,  1003,  1008,  1013,  1018,  1023,  1028,  1033,  1038,
    1043,  1051,  1054,  1057,  1059,  1061,  1063,  1065,  1067,  1069,
    1071,  1073,  1075,  1078
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "ALT_TOK", "APP_TOK", "BACK_TOK",
  "BORDER_TOK", "BTN1_CLICK2_TOK", "BTN1_CLICK_TOK", "BTN1_DOWN_TOK",
  "BTN1_UP_TOK", "BTN2_CLICK2_TOK", "BTN2_CLICK_TOK", "BTN2_DOWN_TOK",
  "BTN2_UP_TOK", "BTN3_CLICK2_TOK", "BTN3_CLICK_TOK", "BTN3_DOWN_TOK",
  "BTN3_UP_TOK", "BTN4_CLICK2_TOK", "BTN4_CLICK_TOK", "BTN4_DOWN_TOK",
  "BTN4_UP_TOK", "BTN5_CLICK2_TOK", "BTN5_CLICK_TOK", "BTN5_DOWN_TOK",
  "BTN5_UP_TOK", "BUTTONS_TOK", "CTRL_TOK", "FBEEP_TOK",
  "FCIRCLE_DOWN_TOK", "FCIRCLE_UP_TOK", "FEXEC_TOK", "FFOCUS_COLOR_TOK",
  "FFOCUS_KEY_TOK", "FKILL_TOK", "FLOWER_TOK", "FMAXIMIZE_TOK",
  "FMENU_TOK", "FMINIMIZE_TOK", "FMOVE_TOK", "FNEXT_CMAP_TOK",
  "FNEXT_KEY_TOK", "FNOP_TOK", "FNORMALIZE_TOK", "FNORMANDRAISE_TOK",
  "FPACK_ICONS_TOK", "FPASS_KEYS_TOK", "FPOST_WMENU_TOK", "FPREV_CMAP_TOK",
  "FPREV_KEY_TOK", "FQUIT_MWM_TOK", "FRAISE_LOWER_TOK", "FRAISE_TOK",
  "FRAME_TOK", "FREE_FAMILY_TOK", "FREFRESH_TOK", "FREFRESH_WIN_TOK",
  "FRESIZE_TOK", "FRESTART_TOK", "FRESTOREANDRAISE_TOK", "FRESTORE_TOK",
  "FSCREEN_TOK", "FSEND_MSG_TOK", "FSEPARATOR_TOK", "FSET_BEHAVIOR_TOK",
  "FTITLE_TOK", "FWINDOWLIST_TOK", "FDESK_TOK", "FTOGGLE_PAGE_TOK",
  "FGOTO_PAGE_TOK", "ICON_TOK", "KEY_TOK", "KEYS_TOK", "LOCK_TOK",
  "MENU_TOK", "MENUB_TOK", "MINIMIZEB_TOK", "MAXIMIZEB_TOK", "MOD1_TOK",
  "MOD2_TOK", "MOD3_TOK", "MOD4_TOK", "MOD5_TOK", "NEXT_TOK", "PREV_TOK",
  "ROOT_TOK", "SHIFT_TOK", "TITLE_TOK", "TRANSIENT_TOK", "WINDOW_TOK",
  "WITHIN_TOK", "STRING_TOK", "'{'", "'}'", "'|'", "'<'", "'>'", "'@'",
  "'_'", "'!'", "'-'", "$accept", "res_file", "binding", "button_bindings",
  "@1", "button_list", "button_binding", "key_bindings", "@2", "key_list",
  "key_binding", "menu_bindings", "@3", "item_list", "item_binding",
  "accelerator", "context", "object", "button", "key", "modifier_list",
  "modifier_name", "button_event_name", "label", "bitmap_file", "mnemonic",
  "function", "optional_arg", "arg", "string", 0
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
     345,   346,   347,   123,   125,   124,    60,    62,    64,    95,
      33,    45
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,   102,   103,   103,   104,   104,   104,   106,   105,   107,
     107,   107,   108,   110,   109,   111,   111,   111,   112,   114,
     113,   115,   115,   115,   116,   117,   117,   118,   118,   119,
     119,   119,   119,   119,   119,   119,   119,   119,   119,   119,
     120,   120,   121,   121,   122,   122,   123,   123,   123,   123,
     123,   123,   123,   123,   123,   124,   124,   124,   124,   124,
     124,   124,   124,   124,   124,   124,   124,   124,   124,   124,
     124,   124,   124,   124,   124,   124,   125,   125,   125,   126,
     127,   127,   128,   128,   128,   128,   128,   128,   128,   128,
     128,   128,   128,   128,   128,   128,   128,   128,   128,   128,
     128,   128,   128,   128,   128,   128,   128,   128,   128,   128,
     128,   128,   128,   128,   128,   128,   128,   128,   128,   128,
     128,   129,   129,   130,   130,   130,   130,   130,   130,   130,
     130,   130,   130,   131
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     2,     0,     1,     1,     1,     0,     6,     2,
       1,     0,     3,     0,     6,     2,     1,     0,     3,     0,
       6,     2,     1,     0,     4,     1,     0,     3,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       4,     3,     5,     4,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     2,
       2,     0,     1,     2,     2,     2,     2,     1,     1,     1,
       2,     1,     2,     1,     1,     1,     2,     1,     1,     1,
       1,     1,     1,     1,     2,     1,     2,     2,     1,     1,
       1,     1,     1,     2,     2,     2,     1,     1,     1,     1,
       1,     1,     0,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       3,     0,     1,     0,     0,     0,     2,     4,     5,     6,
     133,     7,    13,    19,     0,     0,     0,     0,     0,     0,
      10,     0,    16,     0,    22,     0,    48,    46,    49,    50,
      51,    52,    53,    54,    47,     8,     0,     9,     0,     0,
      45,    14,     0,    15,     0,     0,    78,    20,     0,    21,
      81,    77,    76,    75,    58,    57,    55,    56,    62,    61,
      59,    60,    66,    65,    63,    64,    70,    69,    67,    68,
      74,    73,    71,    72,     0,    39,    38,    37,    36,    30,
      31,    32,    33,    29,    35,    34,     0,    28,     0,    44,
       0,     0,     0,    79,     0,    26,    41,   120,    82,   122,
     122,     0,    87,    88,    89,   122,    91,     0,    93,    94,
      95,   122,    97,    98,    99,   100,   101,   102,   103,   122,
     105,   122,   122,   108,   109,   110,   122,   112,   111,   122,
       0,   116,   117,   118,   119,     0,     0,    12,     0,     0,
      18,     0,    80,     0,    25,   132,   129,   127,   130,   131,
     124,   126,   125,   128,     0,    83,   121,    84,    85,    90,
      92,    96,   104,   107,   106,   113,   114,   115,    27,    86,
      40,    43,     0,    24,   123,    42
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     6,     7,    14,    21,    37,     8,    15,    23,
      43,     9,    16,    25,    49,   143,    86,    87,    38,    44,
      45,    40,    74,    50,    51,    95,   137,   155,   156,    11
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -105
static const yytype_int16 yypact[] =
{
    -105,    11,  -105,   -87,   -87,   -87,  -105,  -105,  -105,  -105,
    -105,  -105,  -105,  -105,   -86,   -85,   -83,   117,   200,     1,
    -105,   174,  -105,   224,  -105,     2,  -105,  -105,  -105,  -105,
    -105,  -105,  -105,  -105,  -105,  -105,   359,  -105,    31,   234,
    -105,  -105,   -59,  -105,    31,   257,  -105,  -105,   -87,  -105,
     -80,  -105,  -105,  -105,  -105,  -105,  -105,  -105,  -105,  -105,
    -105,  -105,  -105,  -105,  -105,  -105,  -105,  -105,  -105,  -105,
    -105,  -105,  -105,  -105,   -81,  -105,  -105,  -105,  -105,  -105,
    -105,  -105,  -105,  -105,  -105,  -105,    16,  -105,   359,  -105,
     -77,    16,   -58,  -105,   -87,   269,  -105,  -105,  -105,   135,
     135,   -87,  -105,  -105,  -105,   135,  -105,   -87,  -105,  -105,
    -105,   135,  -105,  -105,  -105,  -105,  -105,  -105,  -105,   135,
    -105,   135,   135,  -105,  -105,  -105,   135,  -105,  -105,   135,
     -87,  -105,  -105,  -105,  -105,    31,   -87,  -105,   -75,   -68,
    -105,   -70,  -105,   122,  -105,  -105,  -105,  -105,  -105,  -105,
    -105,  -105,  -105,  -105,   -87,  -105,  -105,  -105,  -105,  -105,
    -105,  -105,  -105,  -105,  -105,  -105,  -105,  -105,  -105,  -105,
    -105,  -105,   -64,  -105,  -105,  -105
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
    -105,  -105,  -105,  -105,  -105,  -105,  -105,  -105,  -105,  -105,
    -105,  -105,  -105,  -105,  -105,  -105,   -15,  -104,  -105,   -61,
      15,   -33,   -49,  -105,  -105,  -105,   -73,   -96,  -105,    -4
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -24
static const yytype_int16 yytable[] =
{
      12,    13,    24,    46,   157,    10,    89,    17,    18,   159,
      19,     2,    89,    90,   141,   161,    96,    97,   140,    94,
     139,    52,   170,   162,   171,   163,   164,   172,   175,    91,
     165,   168,    75,   166,   144,    76,    39,    77,     3,   138,
       0,     0,     0,     0,    93,    98,    99,   100,   101,   102,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,   122,
     173,     0,   123,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,   134,     4,    78,     5,     0,     0,     0,
     142,     0,     0,   -23,    10,   -23,    47,   158,     0,   -23,
      48,     0,    79,   160,     0,     0,     0,    80,    81,    82,
       0,   135,     0,     0,     0,     0,   136,    83,    20,    84,
     -11,    85,     0,    97,     0,     0,   167,     0,     0,     0,
       0,     0,   169,     0,     0,     0,     0,     0,     0,     0,
     145,     0,     0,     0,     0,   -11,     0,     0,     0,     0,
     174,    98,    99,   100,   101,   102,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,     0,    26,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,   134,
     146,   -11,     0,     0,     0,     0,   -11,   -11,   -11,   -11,
     -11,    22,    27,   -17,   -11,     0,   147,     0,     0,     0,
       0,   -11,     0,   -11,     0,     0,     0,     0,     0,   148,
     149,   150,   136,     0,   151,   152,   153,    26,   -17,     0,
       0,     0,     0,     0,     0,     0,   154,    26,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    28,     0,
       0,     0,    27,    29,    30,    31,    32,    33,     0,     0,
      26,    34,    27,     0,     0,     0,     0,     0,    35,     0,
      36,     0,    26,     0,   -17,     0,     0,     0,     0,   -17,
     -17,   -17,   -17,   -17,     0,    27,     0,   -17,     0,     0,
       0,     0,     0,     0,   -17,     0,   -17,    27,    28,     0,
       0,     0,     0,    29,    30,    31,    32,    33,    28,     0,
       0,    34,     0,    29,    30,    31,    32,    33,    41,     0,
      42,    34,     0,     0,     0,     0,     0,     0,     0,     0,
      88,    28,     0,     0,     0,     0,    29,    30,    31,    32,
      33,     0,     0,    28,    34,     0,     0,     0,    29,    30,
      31,    32,    33,    92,     0,     0,    34,     0,     0,     0,
      53,     0,     0,     0,     0,    42,    54,    55,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    73
};

static const yytype_int16 yycheck[] =
{
       4,     5,     1,     1,   100,    92,    39,    93,    93,   105,
      93,     0,    45,    72,    72,   111,    97,     1,    91,    99,
      97,    25,    97,   119,    92,   121,   122,    97,    92,    44,
     126,   135,     1,   129,    95,     4,    21,     6,    27,    88,
      -1,    -1,    -1,    -1,    48,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
     143,    -1,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    73,    54,    75,    -1,    -1,    -1,
      94,    -1,    -1,    92,    92,    94,    94,   101,    -1,    98,
      98,    -1,    71,   107,    -1,    -1,    -1,    76,    77,    78,
      -1,    95,    -1,    -1,    -1,    -1,   100,    86,     1,    88,
       3,    90,    -1,     1,    -1,    -1,   130,    -1,    -1,    -1,
      -1,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       5,    -1,    -1,    -1,    -1,    28,    -1,    -1,    -1,    -1,
     154,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    -1,     3,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    67,
      55,    74,    -1,    -1,    -1,    -1,    79,    80,    81,    82,
      83,     1,    28,     3,    87,    -1,    71,    -1,    -1,    -1,
      -1,    94,    -1,    96,    -1,    -1,    -1,    -1,    -1,    84,
      85,    86,   100,    -1,    89,    90,    91,     3,    28,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   101,     3,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,
      -1,    -1,    28,    79,    80,    81,    82,    83,    -1,    -1,
       3,    87,    28,    -1,    -1,    -1,    -1,    -1,    94,    -1,
      96,    -1,     3,    -1,    74,    -1,    -1,    -1,    -1,    79,
      80,    81,    82,    83,    -1,    28,    -1,    87,    -1,    -1,
      -1,    -1,    -1,    -1,    94,    -1,    96,    28,    74,    -1,
      -1,    -1,    -1,    79,    80,    81,    82,    83,    74,    -1,
      -1,    87,    -1,    79,    80,    81,    82,    83,    94,    -1,
      96,    87,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      96,    74,    -1,    -1,    -1,    -1,    79,    80,    81,    82,
      83,    -1,    -1,    74,    87,    -1,    -1,    -1,    79,    80,
      81,    82,    83,    96,    -1,    -1,    87,    -1,    -1,    -1,
       1,    -1,    -1,    -1,    -1,    96,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,   103,     0,    27,    73,    75,   104,   105,   109,   113,
      92,   131,   131,   131,   106,   110,   114,    93,    93,    93,
       1,   107,     1,   111,     1,   115,     3,    28,    74,    79,
      80,    81,    82,    83,    87,    94,    96,   108,   120,   122,
     123,    94,    96,   112,   121,   122,     1,    94,    98,   116,
     125,   126,   131,     1,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,   124,     1,     4,     6,    54,    71,
      76,    77,    78,    86,    88,    90,   118,   119,    96,   123,
      72,   118,    96,   131,    99,   127,    97,     1,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    95,   100,   128,   124,    97,
     128,    72,   131,   117,   121,     5,    55,    71,    84,    85,
      86,    89,    90,    91,   101,   129,   130,   129,   131,   129,
     131,   129,   129,   129,   129,   129,   129,   131,   119,   131,
      97,    92,    97,   128,   131,    92
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
        case 7:
#line 264 "mwmparse.y"
    {
			    if (!skip_test) {
				if (button_bind_found == False) {
				    if (strcmp((yyvsp[(2) - (2)].string), pscr->button_bindings) == 0)
					button_bind_match = True;
				    else
					button_bind_match = False;
				}
				else if (strcmp((yyvsp[(2) - (2)].string), pscr->button_bindings) == 0) {
				    yyerror("Ignoring duplicate button bindings");
				    button_bind_match = False;
				}
				else
				    button_bind_match = False;
			    }
			    else
				button_bind_match = True;
			}
    break;

  case 10:
#line 287 "mwmparse.y"
    {
			    yyerror("Invalid button binding");
			    yyerrok;
			    yyclearin;
			}
    break;

  case 12:
#line 296 "mwmparse.y"
    {
			    int contexts = (yyvsp[(2) - (3)].number);
			    int mods, func;
			    MenuItem *mi = NULL;
			    MouseButton *temp = NULL, *ptr;

			    if (!button_bind_match)
				break;

			    mods = (yyvsp[(1) - (3)].button).modifiers;
			    if ((contexts & C_WINDOW) &&
				(((mods == 0) || mods == AnyModifier))) {
			        pscr->buttons2grab &= ~((yyvsp[(1) - (3)].button).button);
			    }
			
			    func = (yyvsp[(3) - (3)].function).func;

			    if ((func == F_EXEC) ||
				(func == F_RESTART) ||
			        (func == F_CIRCULATE_UP) ||
				(func == F_CIRCULATE_DOWN) ||
			        (func == F_WARP)) {
			        mi = (MenuItem *)XtMalloc(sizeof(MenuItem));
			
			        mi->next = (MenuItem *) NULL;
			        mi->prev = (MenuItem *) NULL;
			        if ((func == F_EXEC) || (func == F_RESTART)) {
			            mi->item = XtNewString("DOIT");
			            mi->action = (yyvsp[(3) - (3)].function).arg;
			        }
			        else {
			            mi->item = XtNewString("DONT DOIT");
			            mi->action = (yyvsp[(3) - (3)].function).arg;
			        }
				mi->item2 = "";
			        mi->state = 0;
			        mi->func = func;
			        mi->strlen = strlen(mi->item);
				mi->strlen2 = 0;
			        mi->val1 = 0;
			        mi->val2 = 0;
			        mi->val1_unit = 1;
			        mi->val2_unit = 1;
			    }

			    temp = pscr->buttons;
			    pscr->buttons =
				(MouseButton *)XtMalloc(sizeof(MouseButton));
			    pscr->buttons->func = func;
			    if (func == F_POPUP)
				pscr->buttons->menu = (MenuRoot *)(yyvsp[(3) - (3)].function).arg;
			    else if (func == F_W_POPUP)
				pscr->buttons->menu = (MenuRoot *)DEFAULT_WIN_MENU_NAME;
			    else
				pscr->buttons->menu = NULL;
			    pscr->buttons->item = mi;
			    pscr->buttons->button =
				ffs((yyvsp[(1) - (3)].button).button) - 8;
			    pscr->buttons->context = contexts;
			    pscr->buttons->modifier = mods;
			    pscr->buttons->mask = (yyvsp[(1) - (3)].button).event;
			    pscr->buttons->count = (yyvsp[(1) - (3)].button).count;
			    pscr->buttons->val1 = 0;
			    pscr->buttons->val2 = 0;
			    pscr->buttons->val1_unit = pscr->d_width;
			    pscr->buttons->val2_unit = pscr->d_height;

			    /* if duplicate, skip second */
			    for (ptr = temp;
				 ptr != NULL;
				 ptr = ptr->next) {
                              /* If everything is the same except for having
                                  func's F_POPUP and F_WINDOWLIST, the
                                  second is a duplicate */
                               if (((ptr->func == F_POPUP || 
                                     ptr->func == F_WINDOWLIST) && 
                                    (pscr->buttons->func == F_POPUP || 
                                     pscr->buttons->func == F_WINDOWLIST)) &&
				    ptr->modifier == pscr->buttons->modifier &&
				    ptr->mask == pscr->buttons->mask &&
                                   ptr->count == pscr->buttons->count &&
                                   ptr->context == pscr->buttons->context &&
                                   ptr->button  == pscr->buttons->button) {

                                   XtFree((char *)pscr->buttons);
                                   pscr->buttons = temp;
                                   break;
                               }

                               if (ptr->func == pscr->buttons->func &&
                                   ptr->modifier == pscr->buttons->modifier &&
                                   ptr->mask == pscr->buttons->mask &&
                                   ptr->count == pscr->buttons->count &&
                                   ptr->button == pscr->buttons->button) {
				    ptr->context |= pscr->buttons->context;
				    XtFree((char *)pscr->buttons);
				    pscr->buttons = temp;
				    break;
				}
			    }
			    if (ptr == NULL)
				pscr->buttons->next = temp;
			}
    break;

  case 13:
#line 402 "mwmparse.y"
    {
			    if (!skip_test) {
				if (key_bind_found == False) {
				    if (strcmp((yyvsp[(2) - (2)].string), pscr->key_bindings) == 0)
					key_bind_match = True;
				    else
					key_bind_match = False;
				}
				else if (strcmp((yyvsp[(2) - (2)].string), pscr->key_bindings) == 0) {
				    yyerror("Ignoring duplicate key bindings");
					key_bind_match = False;
				}
				else
				    key_bind_match = False;
			    }
			    else
				key_bind_match = True;
			}
    break;

  case 16:
#line 425 "mwmparse.y"
    {
			    yyerror("Invalid key binding");
			    yyerrok;
			    yyclearin;
			}
    break;

  case 18:
#line 434 "mwmparse.y"
    {
			    FuncKey        *tmp = NULL;
			    KeySym          keysym;
			    KeyCode         keycode;
			    int             i, min, max;
			    int func, contexts;
			    char *ptr;

			    if (!key_bind_match)
				break;

			    ptr = (yyvsp[(3) - (3)].function).arg;
			    func = (yyvsp[(3) - (3)].function).func;
			    contexts = (yyvsp[(2) - (3)].number);

			    /*
			     * Don't let a 0 keycode go through, since that
			     * means AnyKey to the XGrabKey call in GrabKeys().
			     */
			    keysym = (yyvsp[(1) - (3)].hotkey).key;
			    if (keysym == 0)
				break;
			    if ((keycode = XKeysymToKeycode(dpy, (yyvsp[(1) - (3)].hotkey).key)) == 0)
				break;

			    XDisplayKeycodes(dpy, &min, &max);
			    for (i = min; i <= max; i++) {
				if (XKeycodeToKeysym(dpy, i, 0) == keysym) {
				    tmp = (FuncKey *)XtMalloc(sizeof(FuncKey));
				    tmp->next = pscr->keys;
				    pscr->keys = tmp;

				    tmp->name = "HOTKEY";
				    tmp->keycode = i;
				    tmp->cont = contexts;
				    tmp->mods = (yyvsp[(1) - (3)].hotkey).modifiers;
				    tmp->func = func;
				    if (func == F_W_POPUP)
					tmp->action = DEFAULT_WIN_MENU_NAME;
				    else
					tmp->action = ptr;
				    tmp->val1 = 0;
				    tmp->val2 = 0;
				    tmp->val1_unit = pscr->d_width;
				    tmp->val2_unit = pscr->d_height;
				    tmp->menu = NULL;
				}
			    }
			}
    break;

  case 19:
#line 486 "mwmparse.y"
    {
			    num_items = 0;
			    cur_menu = MENU_Create((yyvsp[(2) - (2)].string));
			}
    break;

  case 20:
#line 491 "mwmparse.y"
    {
			    MENU_Add(pscr, cur_menu);
			}
    break;

  case 22:
#line 498 "mwmparse.y"
    {
			    yyerror("Invalid menu item");
			    yyerrok;
			    yyclearin;
			}
    break;

  case 24:
#line 507 "mwmparse.y"
    {
			    MenuItem *tmp;
			    int width, width2;
			    FuncKey        *tmpk = NULL;
			    KeySym          keysym;
			    KeyCode         keycode;
			    int             i, min, max;
			    int func, contexts;
			    char *ptr, *acc;

			    tmp = (MenuItem *)XtCalloc(1, sizeof(MenuItem));
			    if (cur_menu->first == NULL) {
				cur_menu->first = tmp;
				tmp->prev = NULL;
				tmp->next = NULL;
			    }
			    else {
				cur_menu->last->next = tmp;
				tmp->prev = cur_menu->last;
			    }
			    cur_menu->last = tmp;
			    cur_menu->items++;

			    if ((yyvsp[(4) - (4)].function).func == F_NOP)
				tmp->item = "";
			    else
				tmp->item = (yyvsp[(1) - (4)].label).string;
			    tmp->item2 = "";
			    MENU_FindHotKey(tmp, (yyvsp[(2) - (4)].key));
			    if (strcmp(tmp->item, "no-label") == 0)
				tmp->strlen = 0;
			    else
				tmp->strlen = strlen(tmp->item);

			    tmp->action = (yyvsp[(4) - (4)].function).arg;
			    tmp->next = NULL;
			    tmp->state = 0;
			    tmp->func = (yyvsp[(4) - (4)].function).func;
			    tmp->val1 = 0;
			    tmp->val2 = 0;
			    tmp->val1_unit = pscr->d_width;
			    tmp->val2_unit = pscr->d_height;
			
			    width = XTextWidth(pscr->components[MWM_MENU].font,
					       tmp->item, tmp->strlen);
			    if (tmp->func == F_POPUP)
			        width += 15;
			    if (width <= 0)
			        width = 1;
			    if (width > cur_menu->width)
			        cur_menu->width = width;

			    ptr = (yyvsp[(4) - (4)].function).arg;
			    func = (yyvsp[(4) - (4)].function).func;
			    contexts = C_WINDOW|C_ICON|C_FRAME|C_TITLE|C_LALL|C_RALL;

			    /*
			     * Don't let a 0 keycode go through, since that
			     * means AnyKey to the XGrabKey call in GrabKeys().
			     */
			    keysym = (yyvsp[(3) - (4)].hotkey).key;
			    if (keysym == 0)
				break;
			    if ((keycode = XKeysymToKeycode(dpy, (yyvsp[(3) - (4)].hotkey).key)) == 0)
				break;

			    acc = MENU_AcceleratorString(pscr,
							 (yyvsp[(3) - (4)].hotkey).key,
							 (yyvsp[(3) - (4)].hotkey).modifiers);
			    if (strlen(acc)) {
				tmp->item2 = XtNewString(acc);
				tmp->strlen2 = strlen(acc);
				width2 = XTextWidth(pscr->components[MWM_MENU].font,
						    tmp->item2, tmp->strlen2);
				if (width2 > cur_menu->width2)
				    cur_menu->width2 = width2;
			    }

			    XDisplayKeycodes(dpy, &min, &max);
			    for (i = min; i <= max; i++) {
				if (XKeycodeToKeysym(dpy, i, 0) == keysym) {
				    tmpk = (FuncKey *)XtMalloc(sizeof(FuncKey));
				    tmpk->next = pscr->keys;
				    pscr->keys = tmpk;

				    tmpk->name = "HOTKEY";
				    tmpk->keycode = i;
				    tmpk->cont = contexts;
				    tmpk->mods = (yyvsp[(3) - (4)].hotkey).modifiers;
				    tmpk->func = func;
				    tmpk->action = ptr;
				    tmpk->val1 = 0;
				    tmpk->val2 = 0;
				    tmpk->val1_unit = pscr->d_width;
				    tmpk->val2_unit = pscr->d_height;
				    tmpk->menu = NULL;
				}
			    }
			}
    break;

  case 25:
#line 609 "mwmparse.y"
    { (yyval.hotkey) = (yyvsp[(1) - (1)].hotkey); }
    break;

  case 26:
#line 611 "mwmparse.y"
    { (yyval.hotkey).modifiers = 0; (yyval.hotkey).key = 0; }
    break;

  case 27:
#line 615 "mwmparse.y"
    { (yyval.number) = (yyvsp[(1) - (3)].number) | (yyvsp[(3) - (3)].number); }
    break;

  case 28:
#line 617 "mwmparse.y"
    { (yyval.number) = (yyvsp[(1) - (1)].number); }
    break;

  case 29:
#line 621 "mwmparse.y"
    { (yyval.number) = C_ROOT; }
    break;

  case 30:
#line 623 "mwmparse.y"
    { (yyval.number) = C_ICON; }
    break;

  case 31:
#line 625 "mwmparse.y"
    { (yyval.number) = C_MENUB; }
    break;

  case 32:
#line 627 "mwmparse.y"
    { (yyval.number) = C_MINIMIZEB; }
    break;

  case 33:
#line 629 "mwmparse.y"
    { (yyval.number) = C_MAXIMIZEB; }
    break;

  case 34:
#line 631 "mwmparse.y"
    { (yyval.number) = C_WINDOW|C_FRAME|C_TITLE|C_LALL|C_RALL|C_ICON; }
    break;

  case 35:
#line 633 "mwmparse.y"
    { (yyval.number) = C_TITLE; }
    break;

  case 36:
#line 635 "mwmparse.y"
    { (yyval.number) = C_FRAME|C_TITLE|C_LALL|C_RALL; }
    break;

  case 37:
#line 637 "mwmparse.y"
    { (yyval.number) = C_FRAME; }
    break;

  case 38:
#line 639 "mwmparse.y"
    { (yyval.number) = C_WINDOW; }
    break;

  case 39:
#line 641 "mwmparse.y"
    { yyerror("Invalid context"); (yyval.number) = 0; }
    break;

  case 40:
#line 645 "mwmparse.y"
    {
			    (yyval.button) = (yyvsp[(3) - (4)].button);
			    (yyval.button).modifiers = (yyvsp[(1) - (4)].modifiers);
			}
    break;

  case 41:
#line 650 "mwmparse.y"
    {
			    (yyval.button) = (yyvsp[(2) - (3)].button);
			    (yyval.button).modifiers = 0;
			}
    break;

  case 42:
#line 657 "mwmparse.y"
    {
			    (yyval.hotkey).modifiers = (yyvsp[(1) - (5)].modifiers);
			    (yyval.hotkey).key = XStringToKeysym((yyvsp[(5) - (5)].string));
			}
    break;

  case 43:
#line 662 "mwmparse.y"
    {
			    (yyval.hotkey).modifiers = 0;
			    (yyval.hotkey).key = XStringToKeysym((yyvsp[(4) - (4)].string));
			}
    break;

  case 44:
#line 669 "mwmparse.y"
    { (yyval.modifiers) = (yyvsp[(1) - (2)].modifiers) | (yyvsp[(2) - (2)].modifiers); }
    break;

  case 45:
#line 671 "mwmparse.y"
    { (yyval.modifiers) = (yyvsp[(1) - (1)].modifiers); }
    break;

  case 46:
#line 675 "mwmparse.y"
    { (yyval.modifiers) = ControlMask; }
    break;

  case 47:
#line 677 "mwmparse.y"
    { (yyval.modifiers) = ShiftMask; }
    break;

  case 48:
#line 679 "mwmparse.y"
    { (yyval.modifiers) = pscr->alt_mask; }
    break;

  case 49:
#line 681 "mwmparse.y"
    { (yyval.modifiers) = LockMask; }
    break;

  case 50:
#line 683 "mwmparse.y"
    { (yyval.modifiers) = Mod1Mask; }
    break;

  case 51:
#line 685 "mwmparse.y"
    { (yyval.modifiers) = Mod2Mask; }
    break;

  case 52:
#line 687 "mwmparse.y"
    { (yyval.modifiers) = Mod3Mask; }
    break;

  case 53:
#line 689 "mwmparse.y"
    { (yyval.modifiers) = Mod4Mask; }
    break;

  case 54:
#line 691 "mwmparse.y"
    { (yyval.modifiers) = Mod5Mask; }
    break;

  case 55:
#line 695 "mwmparse.y"
    {
			    (yyval.button).button = Button1Mask;
			    (yyval.button).event = ButtonPressMask;
			    (yyval.button).count = 0;
			}
    break;

  case 56:
#line 701 "mwmparse.y"
    {
			    (yyval.button).button = Button1Mask;
			    (yyval.button).event = ButtonReleaseMask;
			    (yyval.button).count = 0;
			}
    break;

  case 57:
#line 707 "mwmparse.y"
    {
			    (yyval.button).button = Button1Mask;
			    (yyval.button).event = ButtonPressMask|ButtonReleaseMask;
			    (yyval.button).count = 1;
			}
    break;

  case 58:
#line 713 "mwmparse.y"
    {
			    (yyval.button).button = Button1Mask;
			    (yyval.button).event = ButtonPressMask|ButtonReleaseMask;
			    (yyval.button).count = 2;
			}
    break;

  case 59:
#line 719 "mwmparse.y"
    {
			    (yyval.button).button = Button2Mask;
			    (yyval.button).event = ButtonPressMask;
			    (yyval.button).count = 0;
			}
    break;

  case 60:
#line 725 "mwmparse.y"
    {
			    (yyval.button).button = Button2Mask;
			    (yyval.button).event = ButtonReleaseMask;
			    (yyval.button).count = 0;
			}
    break;

  case 61:
#line 731 "mwmparse.y"
    {
			    (yyval.button).button = Button2Mask;
			    (yyval.button).event = ButtonPressMask|ButtonReleaseMask;
			    (yyval.button).count = 1;
			}
    break;

  case 62:
#line 737 "mwmparse.y"
    {
			    (yyval.button).button = Button2Mask;
			    (yyval.button).event = ButtonPressMask|ButtonReleaseMask;
			    (yyval.button).count = 2;
			}
    break;

  case 63:
#line 743 "mwmparse.y"
    {
			    (yyval.button).button = Button3Mask;
			    (yyval.button).event = ButtonPressMask;
			    (yyval.button).count = 0;
			}
    break;

  case 64:
#line 749 "mwmparse.y"
    {
			    (yyval.button).button = Button3Mask;
			    (yyval.button).event = ButtonReleaseMask;
			    (yyval.button).count = 0;
			}
    break;

  case 65:
#line 755 "mwmparse.y"
    {
			    (yyval.button).button = Button3Mask;
			    (yyval.button).event = ButtonPressMask|ButtonReleaseMask;
			    (yyval.button).count = 1;
			}
    break;

  case 66:
#line 761 "mwmparse.y"
    {
			    (yyval.button).button = Button3Mask;
			    (yyval.button).event = ButtonPressMask|ButtonReleaseMask;
			    (yyval.button).count = 2;
			}
    break;

  case 67:
#line 767 "mwmparse.y"
    {
			    (yyval.button).button = Button4Mask;
			    (yyval.button).event = ButtonPressMask;
			    (yyval.button).count = 0;
			}
    break;

  case 68:
#line 773 "mwmparse.y"
    {
			    (yyval.button).button = Button4Mask;
			    (yyval.button).event = ButtonReleaseMask;
			    (yyval.button).count = 0;
			}
    break;

  case 69:
#line 779 "mwmparse.y"
    {
			    (yyval.button).button = Button4Mask;
			    (yyval.button).event = ButtonPressMask|ButtonReleaseMask;
			    (yyval.button).count = 1;
			}
    break;

  case 70:
#line 785 "mwmparse.y"
    {
			    (yyval.button).button = Button4Mask;
			    (yyval.button).event = ButtonPressMask|ButtonReleaseMask;
			    (yyval.button).count = 2;
			}
    break;

  case 71:
#line 791 "mwmparse.y"
    {
			    (yyval.button).button = Button5Mask;
			    (yyval.button).event = ButtonPressMask;
			    (yyval.button).count = 0;
			}
    break;

  case 72:
#line 797 "mwmparse.y"
    {
			    (yyval.button).button = Button5Mask;
			    (yyval.button).event = ButtonReleaseMask;
			    (yyval.button).count = 0;
			}
    break;

  case 73:
#line 803 "mwmparse.y"
    {
			    (yyval.button).button = Button5Mask;
			    (yyval.button).event = ButtonPressMask|ButtonReleaseMask;
			    (yyval.button).count = 1;
			}
    break;

  case 74:
#line 809 "mwmparse.y"
    {
			    (yyval.button).button = Button5Mask;
			    (yyval.button).event = ButtonPressMask|ButtonReleaseMask;
			    (yyval.button).count = 2;
			}
    break;

  case 75:
#line 815 "mwmparse.y"
    {
			    yyerror("Invalid button event name");
			    (yyval.button).button = 0;
			    (yyval.button).event = 0;
			    (yyval.button).count = 0;
			}
    break;

  case 76:
#line 824 "mwmparse.y"
    { (yyval.label).type = IS_STRING; (yyval.label).string = (yyvsp[(1) - (1)].string); }
    break;

  case 77:
#line 826 "mwmparse.y"
    { (yyval.label).type = IS_BITMAP; (yyval.label).string = (yyvsp[(1) - (1)].string); }
    break;

  case 78:
#line 828 "mwmparse.y"
    {
			    yyerror("Invalid label");
			    (yyval.label).type = IS_STRING;
			    (yyval.label).string = "";
			}
    break;

  case 79:
#line 836 "mwmparse.y"
    { (yyval.string) = (yyvsp[(2) - (2)].string); }
    break;

  case 80:
#line 840 "mwmparse.y"
    {
			    if (strlen((yyvsp[(2) - (2)].string)) != 1) {
				yyerror("Invalid mnemonic specification");
				(yyval.key) = 0;
			    }
			    else {
				(yyval.key) = XStringToKeysym((yyvsp[(2) - (2)].string));
			    }
			}
    break;

  case 81:
#line 850 "mwmparse.y"
    { (yyval.key) = 0; }
    break;

  case 82:
#line 854 "mwmparse.y"
    {
			    (yyval.function).func = F_BEEP;
			    (yyval.function).arg = "";
			}
    break;

  case 83:
#line 859 "mwmparse.y"
    {
			    (yyval.function).func = F_CIRCULATE_DOWN;
			    (yyval.function).arg = (yyvsp[(2) - (2)].string);
			}
    break;

  case 84:
#line 864 "mwmparse.y"
    {
			    (yyval.function).func = F_CIRCULATE_UP;
			    (yyval.function).arg = (yyvsp[(2) - (2)].string);
			}
    break;

  case 85:
#line 869 "mwmparse.y"
    {
			    (yyval.function).func = F_EXEC;
			    (yyval.function).arg = (yyvsp[(2) - (2)].string);
			}
    break;

  case 86:
#line 874 "mwmparse.y"
    {
			    (yyval.function).func = F_EXEC;
			    (yyval.function).arg = (yyvsp[(2) - (2)].string);
			}
    break;

  case 87:
#line 879 "mwmparse.y"
    {
			    (yyval.function).func = F_FOCUS_COLOR;
			    (yyval.function).arg = "";
			}
    break;

  case 88:
#line 884 "mwmparse.y"
    {
			    (yyval.function).func = F_FOCUS_KEY;
			    (yyval.function).arg = "";
			}
    break;

  case 89:
#line 889 "mwmparse.y"
    {
			    (yyval.function).func = F_CLOSE;
			    (yyval.function).arg = "";
			}
    break;

  case 90:
#line 894 "mwmparse.y"
    {
			    (yyval.function).func = F_LOWER;
			    (yyval.function).arg = (yyvsp[(2) - (2)].string);
			}
    break;

  case 91:
#line 899 "mwmparse.y"
    {
			    (yyval.function).func = F_MAXIMIZE;
			    (yyval.function).arg = "";
			}
    break;

  case 92:
#line 904 "mwmparse.y"
    {
			    (yyval.function).func = F_POPUP;
			    (yyval.function).arg = (yyvsp[(2) - (2)].string);
			}
    break;

  case 93:
#line 909 "mwmparse.y"
    {
			    (yyval.function).func = F_ICONIFY;
			    (yyval.function).arg = "";
			}
    break;

  case 94:
#line 914 "mwmparse.y"
    {
			    (yyval.function).func = F_MOVE;
			    (yyval.function).arg = "";
			}
    break;

  case 95:
#line 919 "mwmparse.y"
    {
			    (yyval.function).func = F_NEXT_CMAP;
			    (yyval.function).arg = "";
			}
    break;

  case 96:
#line 924 "mwmparse.y"
    {
			    (yyval.function).func = F_NEXT_KEY;
			    (yyval.function).arg = (yyvsp[(2) - (2)].string);
			}
    break;

  case 97:
#line 929 "mwmparse.y"
    {
			    (yyval.function).func = F_NOP;
			    (yyval.function).arg = "";
			}
    break;

  case 98:
#line 934 "mwmparse.y"
    {
			    (yyval.function).func = F_NORMALIZE;
			    (yyval.function).arg = "";
			}
    break;

  case 99:
#line 939 "mwmparse.y"
    {
			    (yyval.function).func = F_NORM_AND_RAISE;
			    (yyval.function).arg = "";
			}
    break;

  case 100:
#line 944 "mwmparse.y"
    {
			    (yyval.function).func = F_PACK_ICONS;
			    (yyval.function).arg = "";
			}
    break;

  case 101:
#line 949 "mwmparse.y"
    {
			    (yyval.function).func = F_PASS_KEYS;
			    (yyval.function).arg = "";
			}
    break;

  case 102:
#line 954 "mwmparse.y"
    {
			    (yyval.function).func = F_W_POPUP;
			    (yyval.function).arg = "";
			}
    break;

  case 103:
#line 959 "mwmparse.y"
    {
			    (yyval.function).func = F_PREV_CMAP;
			    (yyval.function).arg = "";
			}
    break;

  case 104:
#line 964 "mwmparse.y"
    {
			    (yyval.function).func = F_PREV_KEY;
			    (yyval.function).arg = (yyvsp[(2) - (2)].string);
			}
    break;

  case 105:
#line 969 "mwmparse.y"
    {
			    (yyval.function).func = F_QUIT;
			    (yyval.function).arg = "";
			}
    break;

  case 106:
#line 974 "mwmparse.y"
    {
			    (yyval.function).func = F_RAISE;
			    (yyval.function).arg = (yyvsp[(2) - (2)].string);
			}
    break;

  case 107:
#line 979 "mwmparse.y"
    {
			    (yyval.function).func = F_RAISELOWER;
			    (yyval.function).arg = (yyvsp[(2) - (2)].string);
			}
    break;

  case 108:
#line 984 "mwmparse.y"
    {
			    (yyval.function).func = F_REFRESH;
			    (yyval.function).arg = "";
			}
    break;

  case 109:
#line 989 "mwmparse.y"
    {
			    (yyval.function).func = F_REFRESH_WIN;
			    (yyval.function).arg = "";
			}
    break;

  case 110:
#line 994 "mwmparse.y"
    {
			    (yyval.function).func = F_RESIZE;
			    (yyval.function).arg = "";
			}
    break;

  case 111:
#line 999 "mwmparse.y"
    {
			    (yyval.function).func = F_ICONIFY;
			    (yyval.function).arg = "";
			}
    break;

  case 112:
#line 1004 "mwmparse.y"
    {
			    (yyval.function).func = F_RESTORE_AND_RAISE;
			    (yyval.function).arg = "";
			}
    break;

  case 113:
#line 1009 "mwmparse.y"
    {
			    (yyval.function).func = F_RESTART;
			    (yyval.function).arg = (yyvsp[(2) - (2)].string);
			}
    break;

  case 114:
#line 1014 "mwmparse.y"
    {
			    (yyval.function).func = F_SCREEN;
			    (yyval.function).arg = (yyvsp[(2) - (2)].string);
			}
    break;

  case 115:
#line 1019 "mwmparse.y"
    {
			    (yyval.function).func = F_SEND_MSG;
			    (yyval.function).arg = (yyvsp[(2) - (2)].string);
			}
    break;

  case 116:
#line 1024 "mwmparse.y"
    {
			    (yyval.function).func = F_NOP;
			    (yyval.function).arg = "";
			}
    break;

  case 117:
#line 1029 "mwmparse.y"
    {
			    (yyval.function).func = F_SET_BEHAVIOR;
			    (yyval.function).arg = "";
			}
    break;

  case 118:
#line 1034 "mwmparse.y"
    {
			    (yyval.function).func = F_TITLE;
			    (yyval.function).arg = "";
			}
    break;

  case 119:
#line 1039 "mwmparse.y"
    {
			    (yyval.function).func = F_WINDOWLIST;
			    (yyval.function).arg = "";
			}
    break;

  case 120:
#line 1044 "mwmparse.y"
    {
			    yyerror("Invalid mnemonic, accelerator, or function");
			    (yyval.function).func = F_NOP;
			    (yyval.function).arg = "";
			}
    break;

  case 121:
#line 1052 "mwmparse.y"
    { (yyval.string) = (yyvsp[(1) - (1)].string); }
    break;

  case 122:
#line 1054 "mwmparse.y"
    { (yyval.string) = ""; }
    break;

  case 123:
#line 1058 "mwmparse.y"
    { (yyval.string) = (yyvsp[(2) - (2)].string); }
    break;

  case 124:
#line 1060 "mwmparse.y"
    { (yyval.string) = "ROOT"; }
    break;

  case 125:
#line 1062 "mwmparse.y"
    { (yyval.string) = "WINDOW"; }
    break;

  case 126:
#line 1064 "mwmparse.y"
    { (yyval.string) = "TRANSIENT"; }
    break;

  case 127:
#line 1066 "mwmparse.y"
    { (yyval.string) = "ICON"; }
    break;

  case 128:
#line 1068 "mwmparse.y"
    { (yyval.string) = "WITHIN"; }
    break;

  case 129:
#line 1070 "mwmparse.y"
    { (yyval.string) = "FREE_FAMILY"; }
    break;

  case 130:
#line 1072 "mwmparse.y"
    { (yyval.string) = "NEXT"; }
    break;

  case 131:
#line 1074 "mwmparse.y"
    { (yyval.string) = "PREV"; }
    break;

  case 132:
#line 1076 "mwmparse.y"
    { (yyval.string) = "BACK"; }
    break;

  case 133:
#line 1079 "mwmparse.y"
    { (yyval.string) = (yyvsp[(1) - (1)].string); }
    break;


/* Line 1267 of yacc.c.  */
#line 2950 "mwmparse.c"
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


#line 1081 "mwmparse.y"


/*
 * these to variables are used by the parser to control input
 */
#define MAX_UNGET	1024
static const char *curpos;
static const char *endpos;
static const char *input_buf;
static char unget[MAX_UNGET];
static int upos = 0;

static char *_MwmRootMenu = DEFAULT_MWM_ROOT_MENU;
static char *_MwmKeyBindings = DEFAULT_MWM_KEY_BINDINGS;
static char *_MwmWindowMenu = DEFAULT_MWM_WINDOW_MENU;
static char *_MwmBehaviorKey = MWM_BEHAVIOR_KEY_BINDINGS;
static char *_MwmBuiltinButtonBindings = BUILTIN_MWM_BUTTON_BINDINGS;
static char *_MwmBuiltinMenuButtonBindings = BUILTIN_MENU_BUTTON_BINDINGS;
static char *_MwmBuiltinKillButtonBindings = BUILTIN_KILL_BUTTON_BINDINGS;
static char *_MwmBuiltinIconButtonBindings = BUILTIN_ICON_BUTTON_BINDINGS;

void
yyerror(const char *fmt, ...) {
    va_list arg_list;

    va_start(arg_list, fmt);
    vfprintf(stderr, fmt, arg_list);
    va_end(arg_list);
    fprintf(stderr, " at line %d\n", lineno);
}


char
mwm_getc(void) {
    char c;

    if (upos) {
	upos--;
	c = unget[upos];
	if (c == '\n')
	    lineno++;
	return c;
    }
    if (curpos >= endpos)
	return 0;
    else if (*curpos == 0) {
	curpos++;
	return 0;
    }
    c = *curpos;
    curpos++;
    if (c == '\n')
	lineno++;
    return c;
}


void
mwm_putc(char c) {
    printf("OUTPUT: %c\n", c);
}


void
mwm_unputc(char c) {
    if (upos == MAX_UNGET) {
	yyerror("Lexer can't back up anymore.\n");
	return;
    }
    if (c == '\n')
	lineno--;
    unget[upos] = c;
    upos++;
}


int
PARSE_buf(ScreenInfo *scr, char *buffer) {
    pscr = scr;
    lineno = 1;
    curpos = input_buf = buffer;
    endpos = input_buf + strlen(input_buf);
    upos = 0;
    return yyparse();
}


static void
do_standard(ScreenInfo *scr)
{
    skip_test = True;

    PARSE_buf(scr, _MwmRootMenu);
    PARSE_buf(scr, _MwmWindowMenu);
    PARSE_buf(scr, _MwmKeyBindings);
    PARSE_buf(scr, _MwmBehaviorKey);

    /* a certain amount seems to be expected as builtin */
    PARSE_buf(scr, _MwmBuiltinButtonBindings);
    if (Mwm.w_menu_button_click)
	PARSE_buf(scr, _MwmBuiltinMenuButtonBindings);
    if (Mwm.w_menu_button_click_2)
	PARSE_buf(scr, _MwmBuiltinKillButtonBindings);
    if (Mwm.icon_click)
	PARSE_buf(scr, _MwmBuiltinIconButtonBindings);

    PROP_SetBehavior(scr, False);
}


static void
do_custom(ScreenInfo *scr)
{
    char *configfile, *ptr;
    int fd;

    configfile=find_config_file();
    if (configfile==NULL)
    {
	yyerror("Cannot find configuration file.  "
		"Using builtin defaults.\n", configfile);
	do_standard(scr);
	return;
    }
    
#ifdef __EMX__
    if ((fd = open(configfile, O_RDONLY|O_TEXT)) < 0)
#else
    if ((fd = open(configfile, O_RDONLY)) < 0)
#endif
    {
	yyerror("Cannot open configuration file '%s'.  "
		"Using builtin defaults.\n", configfile);
	do_standard(scr);
    }
    else {
	int bytes_read, statrc;
	struct stat st;

	if (debugging)
 	   fprintf(stderr, "Reading configuration from '%s'\n", configfile);

	statrc=stat(configfile, &st);
	ptr = XtMalloc(st.st_size + 1);
	bytes_read=read(fd, ptr, st.st_size);
	close(fd);

	skip_test = False;
	ptr[bytes_read] = '\0';
	PARSE_buf(scr, ptr);

	/* a certain amount seems to be expected as builtin */
	skip_test = True;

	PARSE_buf(scr, _MwmBuiltinButtonBindings);
	if (Mwm.w_menu_button_click)
	PARSE_buf(scr, _MwmBuiltinMenuButtonBindings);
	if (Mwm.w_menu_button_click_2)
	PARSE_buf(scr, _MwmBuiltinKillButtonBindings);
	if (Mwm.icon_click)
	PARSE_buf(scr, _MwmBuiltinIconButtonBindings);

	PROP_SetBehavior(scr, True);

	XtFree(ptr);
    }

    XtFree(configfile);
}

void
PARSE_mwmrc(ScreenInfo *scr) {
    int flag;

    key_bind_found = False;
    button_bind_found = False;
    key_bind_match = False;
    button_bind_match = False;

    if ((flag = PROP_GetBehavior(scr)) & MWM_INFO_STARTUP_CUSTOM) {
	if (debugging)
	    printf("parsing custom file.\n");
	do_custom(scr);
    }
    else if (flag & MWM_INFO_STARTUP_STANDARD) {
	if (debugging)
	    printf("parsing standard bindings.\n");
	do_standard(scr);
    }
    else {
	if (debugging)
	    printf("parsing custom file by default.\n");
	do_custom(scr);
    }

    return;
}

