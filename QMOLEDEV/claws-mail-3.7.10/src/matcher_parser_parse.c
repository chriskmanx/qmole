
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
#line 1 "matcher_parser_parse.y"

/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (c) 2001-2007 by Hiroyuki Yamamoto & The Claws Mail Team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#include "defs.h"

#include <glib.h>
#include <glib/gi18n.h>

#include "utils.h"
#include "filtering.h"
#include "matcher.h"
#include "matcher_parser.h"
#include "matcher_parser_lex.h"
#include "colorlabel.h"

static gint error = 0;
static gint bool_op = 0;
static gint match_type = 0;
static gchar *header = NULL;

static MatcherProp *prop;

static GSList *matchers_list = NULL;

static gboolean enabled = TRUE;
static gchar *name = NULL;
static gint account_id = 0;
static MatcherList *cond;
static GSList *action_list = NULL;
static FilteringAction *action = NULL;
static gboolean matcher_is_fast = TRUE;
static gboolean disable_warnings = FALSE;

static FilteringProp *filtering;

static GSList **prefs_filtering = NULL;
static int enable_compatibility = 0;

enum {
        MATCHER_PARSE_FILE,
        MATCHER_PARSE_NO_EOL,
	MATCHER_PARSE_ENABLED,
	MATCHER_PARSE_NAME,
	MATCHER_PARSE_ACCOUNT,
        MATCHER_PARSE_CONDITION,
        MATCHER_PARSE_FILTERING_ACTION,
};

static int matcher_parse_op = MATCHER_PARSE_FILE;


/* ******************************************************************** */
/* redeclarations to avoid warnings */
void matcher_parserrestart(FILE *input_file);
void matcher_parser_init(void);
void matcher_parser_switch_to_buffer(void * new_buffer);
void matcher_parser_delete_buffer(void * b);
void matcher_parserpop_buffer_state(void);
int matcher_parserlex(void);

void matcher_parser_disable_warnings(const gboolean disable)
{
	disable_warnings = disable;
}

void matcher_parser_start_parsing(FILE *f)
{
	matcher_parserlineno = 1;
	matcher_parserrestart(f);
	account_id = 0;
	matcher_parserparse();
}

 
void * matcher_parser_scan_string(const char * str);
 
FilteringProp *matcher_parser_get_filtering(gchar *str)
{
	void *bufstate;
	void *tmp_str = NULL;
	
	/* little hack to allow passing rules with no names */
	if (!strncmp(str, "rulename ", 9))
		tmp_str = g_strdup(str);
	else 
		tmp_str = g_strconcat("rulename \"\" ", str, NULL);

	/* bad coding to enable the sub-grammar matching
	   in yacc */
	matcher_parserlineno = 1;
	matcher_parse_op = MATCHER_PARSE_NO_EOL;
	matcher_parserrestart(NULL);
	matcher_parserpop_buffer_state();
        matcher_parser_init();
	bufstate = matcher_parser_scan_string((const char *) tmp_str);
        matcher_parser_switch_to_buffer(bufstate);
	if (matcher_parserparse() != 0)
		filtering = NULL;
	matcher_parse_op = MATCHER_PARSE_FILE;
	matcher_parser_delete_buffer(bufstate);
	g_free(tmp_str);
	return filtering;
}

static gboolean check_quote_symetry(gchar *str)
{
	const gchar *walk;
	int ret = 0;
	
	if (str == NULL)
		return TRUE; /* heh, that's symetric */
	if (*str == '\0')
		return TRUE;
	for (walk = str; *walk; walk++) {
		if (*walk == '\"') {
			if (walk == str 	/* first char */
			|| *(walk - 1) != '\\') /* not escaped */
				ret ++;
		}
	}
	return !(ret % 2);
}

MatcherList *matcher_parser_get_name(gchar *str)
{
	void *bufstate;

	if (!check_quote_symetry(str)) {
		cond = NULL;
		return cond;
	}
	
	/* bad coding to enable the sub-grammar matching
	   in yacc */
	matcher_parserlineno = 1;
	matcher_parse_op = MATCHER_PARSE_NAME;
	matcher_parserrestart(NULL);
	matcher_parserpop_buffer_state();
        matcher_parser_init();
	bufstate = matcher_parser_scan_string(str);
	matcher_parserparse();
	matcher_parse_op = MATCHER_PARSE_FILE;
	matcher_parser_delete_buffer(bufstate);
	return cond;
}

MatcherList *matcher_parser_get_enabled(gchar *str)
{
	void *bufstate;

	if (!check_quote_symetry(str)) {
		cond = NULL;
		return cond;
	}
	
	/* bad coding to enable the sub-grammar matching
	   in yacc */
	matcher_parserlineno = 1;
	matcher_parse_op = MATCHER_PARSE_ENABLED;
	matcher_parserrestart(NULL);
	matcher_parserpop_buffer_state();
	matcher_parser_init();
	bufstate = matcher_parser_scan_string(str);
	matcher_parserparse();
	matcher_parse_op = MATCHER_PARSE_FILE;
	matcher_parser_delete_buffer(bufstate);
	return cond;
}

MatcherList *matcher_parser_get_account(gchar *str)
{
	void *bufstate;

	if (!check_quote_symetry(str)) {
		cond = NULL;
		return cond;
	}
	
	/* bad coding to enable the sub-grammar matching
	   in yacc */
	matcher_parserlineno = 1;
	matcher_parse_op = MATCHER_PARSE_ACCOUNT;
	matcher_parserrestart(NULL);
	matcher_parserpop_buffer_state();
	matcher_parser_init();
	bufstate = matcher_parser_scan_string(str);
	matcher_parserparse();
	matcher_parse_op = MATCHER_PARSE_FILE;
	matcher_parser_delete_buffer(bufstate);
	return cond;
}

MatcherList *matcher_parser_get_cond(gchar *str, gboolean *is_fast)
{
	void *bufstate;

	if (!check_quote_symetry(str)) {
		cond = NULL;
		return cond;
	}
	
	matcher_is_fast = TRUE;
	/* bad coding to enable the sub-grammar matching
	   in yacc */
	matcher_parserlineno = 1;
	matcher_parse_op = MATCHER_PARSE_CONDITION;
	matcher_parserrestart(NULL);
	matcher_parserpop_buffer_state();
        matcher_parser_init();
	bufstate = matcher_parser_scan_string(str);
	matcher_parserparse();
	matcher_parse_op = MATCHER_PARSE_FILE;
	matcher_parser_delete_buffer(bufstate);
	if (is_fast)
		*is_fast = matcher_is_fast;
	return cond;
}

GSList *matcher_parser_get_action_list(gchar *str)
{
	void *bufstate;

	if (!check_quote_symetry(str)) {
		action_list = NULL;
		return action_list;
	}
	
	/* bad coding to enable the sub-grammar matching
	   in yacc */
	matcher_parserlineno = 1;
	matcher_parse_op = MATCHER_PARSE_FILTERING_ACTION;
	matcher_parserrestart(NULL);
	matcher_parserpop_buffer_state();
        matcher_parser_init();
	bufstate = matcher_parser_scan_string(str);
	matcher_parserparse();
	matcher_parse_op = MATCHER_PARSE_FILE;
	matcher_parser_delete_buffer(bufstate);
	return action_list;
}

MatcherProp *matcher_parser_get_prop(gchar *str)
{
	MatcherList *list;
	MatcherProp *prop;

	matcher_parserlineno = 1;
	list = matcher_parser_get_cond(str, NULL);
	if (list == NULL)
		return NULL;

	if (list->matchers == NULL)
		return NULL;

	if (list->matchers->next != NULL)
		return NULL;

	prop = list->matchers->data;

	g_slist_free(list->matchers);
	g_free(list);

	return prop;
}

void matcher_parsererror(char *str)
{
	GSList *l;

	if (matchers_list) {
		for (l = matchers_list; l != NULL; l = g_slist_next(l)) {
			matcherprop_free((MatcherProp *)
					 l->data);
			l->data = NULL;
		}
		g_slist_free(matchers_list);
		matchers_list = NULL;
	}
	cond = NULL;
	if (!disable_warnings)
		g_warning("filtering parsing: %i: %s\n",
		  	matcher_parserlineno, str);
	error = 1;
}

int matcher_parserwrap(void)
{
	return 1;
}


/* Line 189 of yacc.c  */
#line 382 "matcher_parser_parse.c"

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

/* Line 214 of yacc.c  */
#line 309 "matcher_parser_parse.y"

	char *str;
	int value;



/* Line 214 of yacc.c  */
#line 651 "matcher_parser_parse.c"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */


/* Line 264 of yacc.c  */
#line 663 "matcher_parser_parse.c"

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
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   704

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  114
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  25
/* YYNRULES -- Number of rules.  */
#define YYNRULES  140
/* YYNRULES -- Number of states.  */
#define YYNSTATES  241

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   368

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
     105,   106,   107,   108,   109,   110,   111,   112,   113
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     4,     7,    10,    12,    14,    15,    18,
      21,    24,    31,    37,    43,    48,    53,    57,    59,    61,
      63,    65,    67,    69,    71,    73,    76,    79,    81,    84,
      86,    88,    90,    92,    94,    96,    98,   102,   104,   106,
     108,   110,   112,   114,   116,   118,   120,   122,   124,   126,
     128,   130,   132,   134,   136,   138,   140,   142,   144,   146,
     148,   150,   152,   154,   157,   160,   162,   164,   166,   168,
     172,   176,   180,   184,   188,   192,   196,   200,   204,   208,
     212,   216,   218,   220,   223,   226,   230,   234,   238,   242,
     246,   250,   253,   256,   259,   262,   265,   268,   269,   275,
     276,   282,   286,   290,   291,   297,   298,   304,   308,   312,
     316,   320,   323,   326,   329,   332,   335,   338,   340,   343,
     345,   347,   349,   351,   353,   355,   357,   359,   361,   365,
     369,   373,   376,   379,   382,   385,   387,   389,   391,   392,
     397
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
     115,     0,    -1,    -1,   116,   117,    -1,   118,   117,    -1,
     118,    -1,   120,    -1,    -1,   119,   121,    -1,     1,    68,
      -1,   112,    68,    -1,   122,   123,   124,   129,   125,    68,
      -1,   122,   123,   124,   129,   125,    -1,   122,   123,   129,
     125,    68,    -1,   122,   123,   129,   125,    -1,   123,   129,
     125,    68,    -1,   123,   129,   125,    -1,   122,    -1,   124,
      -1,   123,    -1,   129,    -1,   126,    -1,    68,    -1,   107,
      -1,   108,    -1,   109,   111,    -1,   110,   113,    -1,   126,
      -1,   127,   126,    -1,   127,    -1,   137,    -1,    46,    -1,
      47,    -1,    48,    -1,    49,    -1,   130,    -1,   130,   131,
     132,    -1,   132,    -1,    70,    -1,    69,    -1,     3,    -1,
       4,    -1,     5,    -1,     6,    -1,     7,    -1,     8,    -1,
       9,    -1,    10,    -1,    11,    -1,    12,    -1,    13,    -1,
      14,    -1,    15,    -1,    77,    -1,    78,    -1,    94,    -1,
      95,    -1,    96,    -1,    97,    -1,    98,    -1,    99,    -1,
      79,    -1,    80,    -1,    81,   113,    -1,    82,   113,    -1,
      83,    -1,    84,    -1,    85,    -1,    86,    -1,    16,   128,
     111,    -1,    17,   128,   111,    -1,    18,   128,   111,    -1,
      19,   128,   111,    -1,    20,   128,   111,    -1,    21,   128,
     111,    -1,    22,   128,   111,    -1,    23,   128,   111,    -1,
      24,   128,   111,    -1,    25,   128,   111,    -1,   100,   128,
     111,    -1,   101,   128,   111,    -1,   104,    -1,   105,    -1,
      26,   113,    -1,    27,   113,    -1,    28,   128,   111,    -1,
      29,   128,   111,    -1,    30,   128,   111,    -1,    31,   128,
     111,    -1,    32,   128,   111,    -1,    33,   128,   111,    -1,
      34,   113,    -1,    35,   113,    -1,    72,   113,    -1,    74,
     113,    -1,    75,   113,    -1,    76,   113,    -1,    -1,    36,
     111,   133,   128,   111,    -1,    -1,    37,   111,   134,   128,
     111,    -1,    38,   128,   111,    -1,    39,   128,   111,    -1,
      -1,    52,   111,   135,    54,   111,    -1,    -1,    53,   111,
     136,    54,   111,    -1,    40,   128,   111,    -1,    41,   128,
     111,    -1,    42,   128,   111,    -1,    43,   128,   111,    -1,
      44,   111,    -1,    45,   111,    -1,    61,   111,    -1,    51,
     111,    -1,   102,   111,    -1,   103,   111,    -1,   106,    -1,
      55,   111,    -1,    56,    -1,    57,    -1,    58,    -1,    59,
      -1,    60,    -1,    62,    -1,    63,    -1,    65,    -1,    66,
      -1,    64,   113,   111,    -1,    67,   113,   111,    -1,    73,
     113,   111,    -1,    71,   113,    -1,    87,   113,    -1,    50,
     113,    -1,    88,   113,    -1,    91,    -1,    92,    -1,    93,
      -1,    -1,    89,   111,   138,   111,    -1,    90,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   364,   364,   364,   372,   374,   378,   380,   380,   382,
     388,   421,   422,   423,   424,   425,   426,   435,   444,   453,
     462,   471,   480,   484,   488,   495,   502,   509,   541,   542,
     546,   554,   558,   562,   566,   573,   581,   585,   593,   597,
     604,   611,   618,   625,   632,   639,   646,   653,   660,   667,
     674,   681,   688,   695,   702,   709,   716,   723,   730,   737,
     744,   751,   758,   765,   776,   787,   794,   801,   808,   815,
     824,   833,   842,   851,   860,   869,   878,   887,   896,   905,
     914,   923,   930,   937,   946,   955,   964,   973,   982,   991,
    1000,  1009,  1018,  1027,  1036,  1044,  1052,  1061,  1060,  1074,
    1073,  1086,  1095,  1105,  1104,  1118,  1117,  1130,  1139,  1148,
    1157,  1166,  1175,  1187,  1196,  1205,  1214,  1223,  1230,  1239,
    1246,  1253,  1260,  1267,  1274,  1281,  1288,  1295,  1302,  1314,
    1326,  1338,  1347,  1356,  1364,  1372,  1376,  1380,  1385,  1384,
    1397
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "MATCHER_ALL", "MATCHER_UNREAD",
  "MATCHER_NOT_UNREAD", "MATCHER_NEW", "MATCHER_NOT_NEW", "MATCHER_MARKED",
  "MATCHER_NOT_MARKED", "MATCHER_DELETED", "MATCHER_NOT_DELETED",
  "MATCHER_REPLIED", "MATCHER_NOT_REPLIED", "MATCHER_FORWARDED",
  "MATCHER_NOT_FORWARDED", "MATCHER_SUBJECT", "MATCHER_NOT_SUBJECT",
  "MATCHER_FROM", "MATCHER_NOT_FROM", "MATCHER_TO", "MATCHER_NOT_TO",
  "MATCHER_CC", "MATCHER_NOT_CC", "MATCHER_TO_OR_CC",
  "MATCHER_NOT_TO_AND_NOT_CC", "MATCHER_AGE_GREATER", "MATCHER_AGE_LOWER",
  "MATCHER_NEWSGROUPS", "MATCHER_NOT_NEWSGROUPS", "MATCHER_INREPLYTO",
  "MATCHER_NOT_INREPLYTO", "MATCHER_REFERENCES", "MATCHER_NOT_REFERENCES",
  "MATCHER_SCORE_GREATER", "MATCHER_SCORE_LOWER", "MATCHER_HEADER",
  "MATCHER_NOT_HEADER", "MATCHER_HEADERS_PART", "MATCHER_NOT_HEADERS_PART",
  "MATCHER_MESSAGE", "MATCHER_NOT_MESSAGE", "MATCHER_BODY_PART",
  "MATCHER_NOT_BODY_PART", "MATCHER_TEST", "MATCHER_NOT_TEST",
  "MATCHER_MATCHCASE", "MATCHER_MATCH", "MATCHER_REGEXPCASE",
  "MATCHER_REGEXP", "MATCHER_SCORE", "MATCHER_MOVE",
  "MATCHER_FOUND_IN_ADDRESSBOOK", "MATCHER_NOT_FOUND_IN_ADDRESSBOOK",
  "MATCHER_IN", "MATCHER_COPY", "MATCHER_DELETE", "MATCHER_MARK",
  "MATCHER_UNMARK", "MATCHER_LOCK", "MATCHER_UNLOCK", "MATCHER_EXECUTE",
  "MATCHER_MARK_AS_READ", "MATCHER_MARK_AS_UNREAD", "MATCHER_FORWARD",
  "MATCHER_MARK_AS_SPAM", "MATCHER_MARK_AS_HAM",
  "MATCHER_FORWARD_AS_ATTACHMENT", "MATCHER_EOL", "MATCHER_OR",
  "MATCHER_AND", "MATCHER_COLOR", "MATCHER_SCORE_EQUAL",
  "MATCHER_REDIRECT", "MATCHER_SIZE_GREATER", "MATCHER_SIZE_SMALLER",
  "MATCHER_SIZE_EQUAL", "MATCHER_LOCKED", "MATCHER_NOT_LOCKED",
  "MATCHER_PARTIAL", "MATCHER_NOT_PARTIAL", "MATCHER_COLORLABEL",
  "MATCHER_NOT_COLORLABEL", "MATCHER_IGNORE_THREAD",
  "MATCHER_NOT_IGNORE_THREAD", "MATCHER_WATCH_THREAD",
  "MATCHER_NOT_WATCH_THREAD", "MATCHER_CHANGE_SCORE", "MATCHER_SET_SCORE",
  "MATCHER_ADD_TO_ADDRESSBOOK", "MATCHER_STOP", "MATCHER_HIDE",
  "MATCHER_IGNORE", "MATCHER_WATCH", "MATCHER_SPAM", "MATCHER_NOT_SPAM",
  "MATCHER_HAS_ATTACHMENT", "MATCHER_HAS_NO_ATTACHMENT", "MATCHER_SIGNED",
  "MATCHER_NOT_SIGNED", "MATCHER_TAG", "MATCHER_NOT_TAG",
  "MATCHER_SET_TAG", "MATCHER_UNSET_TAG", "MATCHER_TAGGED",
  "MATCHER_NOT_TAGGED", "MATCHER_CLEAR_TAGS", "MATCHER_ENABLED",
  "MATCHER_DISABLED", "MATCHER_RULENAME", "MATCHER_ACCOUNT",
  "MATCHER_STRING", "MATCHER_SECTION", "MATCHER_INTEGER", "$accept",
  "file", "$@1", "file_line_list", "file_line", "$@2",
  "section_notification", "instruction", "enabled", "name", "account",
  "filtering", "filtering_action_list", "filtering_action_b", "match_type",
  "condition", "condition_list", "bool_op", "one_condition", "$@3", "$@4",
  "$@5", "$@6", "filtering_action", "$@7", 0
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
     365,   366,   367,   368
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,   114,   116,   115,   117,   117,   118,   119,   118,   118,
     120,   121,   121,   121,   121,   121,   121,   121,   121,   121,
     121,   121,   121,   122,   122,   123,   124,   125,   126,   126,
     127,   128,   128,   128,   128,   129,   130,   130,   131,   131,
     132,   132,   132,   132,   132,   132,   132,   132,   132,   132,
     132,   132,   132,   132,   132,   132,   132,   132,   132,   132,
     132,   132,   132,   132,   132,   132,   132,   132,   132,   132,
     132,   132,   132,   132,   132,   132,   132,   132,   132,   132,
     132,   132,   132,   132,   132,   132,   132,   132,   132,   132,
     132,   132,   132,   132,   132,   132,   132,   133,   132,   134,
     132,   132,   132,   135,   132,   136,   132,   132,   132,   132,
     132,   132,   132,   137,   137,   137,   137,   137,   137,   137,
     137,   137,   137,   137,   137,   137,   137,   137,   137,   137,
     137,   137,   137,   137,   137,   137,   137,   137,   138,   137,
     137
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     2,     1,     1,     0,     2,     2,
       2,     6,     5,     5,     4,     4,     3,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     2,     1,     2,     1,
       1,     1,     1,     1,     1,     1,     3,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     2,     2,     1,     1,     1,     1,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     1,     1,     2,     2,     3,     3,     3,     3,     3,
       3,     2,     2,     2,     2,     2,     2,     0,     5,     0,
       5,     3,     3,     0,     5,     0,     5,     3,     3,     3,
       3,     2,     2,     2,     2,     2,     2,     1,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     3,     3,
       3,     2,     2,     2,     2,     1,     1,     1,     0,     4,
       1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,     0,     0,     1,     0,     0,     3,     0,     0,     6,
       9,    10,     4,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   119,   120,   121,   122,   123,     0,   124,   125,     0,
     126,   127,     0,    22,     0,     0,     0,     0,     0,     0,
      53,    54,    61,    62,     0,     0,    65,    66,    67,    68,
       0,     0,     0,   140,   135,   136,   137,    55,    56,    57,
      58,    59,    60,     0,     0,     0,     0,    81,    82,   117,
      23,    24,     0,     0,     8,    17,    19,    18,    21,    29,
      20,    35,    37,    30,    31,    32,    33,    34,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    83,    84,
       0,     0,     0,     0,     0,     0,    91,    92,    97,    99,
       0,     0,     0,     0,     0,     0,   111,   112,   133,   114,
     103,   105,   118,   113,     0,     0,   131,    93,     0,    94,
      95,    96,    63,    64,   132,   134,   138,     0,     0,   115,
     116,    25,    26,     0,     0,    28,    39,    38,     0,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    85,
      86,    87,    88,    89,    90,     0,     0,   101,   102,   107,
     108,   109,   110,     0,     0,   128,   129,   130,     0,    79,
      80,     0,     0,    16,    27,    36,     0,     0,     0,     0,
     139,     0,    14,    15,    98,   100,   104,   106,    12,    13,
      11
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,     6,     7,     8,     9,   114,   115,   116,
     117,   223,   224,   119,   128,   120,   121,   188,   122,   205,
     206,   213,   214,   123,   218
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -214
static const yytype_int16 yypact[] =
{
    -214,    10,   179,  -214,   -51,   -41,  -214,    28,   289,  -214,
    -214,  -214,  -214,  -214,  -214,  -214,  -214,  -214,  -214,  -214,
    -214,  -214,  -214,  -214,  -214,  -214,    96,    96,    96,    96,
      96,    96,    96,    96,    96,    96,   -39,   -38,    96,    96,
      96,    96,    96,    96,   -31,   -16,   -13,    35,    96,    96,
      96,    96,    96,    96,    36,    37,    26,    38,    39,    40,
      41,  -214,  -214,  -214,  -214,  -214,    42,  -214,  -214,    43,
    -214,  -214,    44,  -214,    45,    46,    47,    48,    49,    50,
    -214,  -214,  -214,  -214,    51,    52,  -214,  -214,  -214,  -214,
      53,    54,    57,  -214,  -214,  -214,  -214,  -214,  -214,  -214,
    -214,  -214,  -214,    96,    96,    58,    59,  -214,  -214,  -214,
    -214,  -214,    60,    61,  -214,    63,   546,  -214,  -214,   598,
    -214,   -50,  -214,  -214,  -214,  -214,  -214,  -214,    62,    64,
      65,    66,    70,   114,   115,   116,   117,   122,  -214,  -214,
     137,   138,   224,   225,   226,   227,  -214,  -214,  -214,  -214,
     232,   247,   248,   333,   334,   335,  -214,  -214,  -214,  -214,
    -214,  -214,  -214,  -214,   336,   337,  -214,  -214,   340,  -214,
    -214,  -214,  -214,  -214,  -214,  -214,  -214,   341,   342,  -214,
    -214,  -214,  -214,   397,   598,  -214,  -214,  -214,   546,  -214,
    -214,  -214,  -214,  -214,  -214,  -214,  -214,  -214,  -214,  -214,
    -214,  -214,  -214,  -214,  -214,    96,    96,  -214,  -214,  -214,
    -214,  -214,  -214,   100,   101,  -214,  -214,  -214,   343,  -214,
    -214,   546,   598,   222,  -214,  -214,   344,   345,   346,   347,
    -214,   598,   391,  -214,  -214,  -214,  -214,  -214,   392,  -214,
    -214
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -214,  -214,  -214,   454,  -214,  -214,  -214,  -214,  -214,   348,
     279,  -213,    22,  -214,   -27,   327,  -214,  -214,   276,  -214,
    -214,  -214,  -214,  -214,  -214
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -8
static const yytype_int16 yytable[] =
{
     129,   130,   131,   132,   133,   134,   135,   136,   137,   232,
       3,   140,   141,   142,   143,   144,   145,    10,   238,   186,
     187,   150,   151,   152,   153,   154,   155,    11,    -5,     4,
     118,    -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,
      -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,
      -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,
      -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,
      -7,    -7,    -7,    -7,   138,   139,   177,   178,    -7,    -7,
      -7,    -7,   146,    -7,    -7,    -7,    -7,    -7,    -7,    -7,
      -7,    -7,    -7,    -7,    -7,    -7,    -7,   147,   148,    -7,
      -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,
      -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,
      -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,
      -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,   158,
       5,   185,   124,   125,   126,   127,   149,   156,   157,   159,
     160,   161,   162,   163,   228,   229,   164,   165,   166,   167,
     168,   169,   170,   171,   172,   173,   174,   175,   176,   179,
     180,   181,   112,   189,   182,   190,   191,   192,   226,   227,
       4,   193,    -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,
      -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,
      -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,
      -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,
      -7,    -7,    -7,    -7,    -7,   194,   195,   196,   197,    -7,
      -7,    -7,    -7,   198,    -7,    -7,    -7,    -7,    -7,    -7,
      -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,   199,   200,
      -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,
      -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,
      -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,
      -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,    -7,
     233,     5,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,   201,   202,   203,   204,    56,
      57,    58,    59,   207,    60,    61,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,   208,   209,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    97,    98,    99,   100,   101,   102,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,   184,   210,   211,   212,   215,   216,    58,
      59,   217,   219,   220,   230,   234,   235,   236,   237,   239,
     240,    12,   221,   183,   225,     0,     0,     0,     0,    75,
       0,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,     0,     0,     0,     0,     0,     0,
       0,    97,    98,    99,   100,   101,   102,   103,   104,     0,
       0,   107,   108,     0,     0,     0,     0,   113,     0,     0,
     222,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   231,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,    58,    59,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,     0,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,     0,     0,     0,     0,     0,     0,     0,
      97,    98,    99,   100,   101,   102,   103,   104,    56,    57,
     107,   108,     0,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,    74,
       0,    76,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    90,    91,    92,    93,    94,
      95,    96,     0,     0,     0,     0,     0,     0,     0,     0,
     105,   106,     0,     0,   109
};

static const yytype_int16 yycheck[] =
{
      27,    28,    29,    30,    31,    32,    33,    34,    35,   222,
       0,    38,    39,    40,    41,    42,    43,    68,   231,    69,
      70,    48,    49,    50,    51,    52,    53,    68,     0,     1,
       8,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,   113,   113,   103,   104,    50,    51,
      52,    53,   113,    55,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,   113,   111,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     102,   103,   104,   105,   106,   107,   108,   109,   110,   113,
     112,   119,    46,    47,    48,    49,   111,   111,   111,   111,
     111,   111,   111,   111,    54,    54,   113,   113,   113,   113,
     113,   113,   113,   113,   113,   113,   113,   113,   111,   111,
     111,   111,   109,   111,   113,   111,   111,   111,   205,   206,
       1,   111,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,   111,   111,   111,   111,    50,
      51,    52,    53,   111,    55,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,   111,   111,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,   110,
      68,   112,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,   111,   111,   111,   111,    50,
      51,    52,    53,   111,    55,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,   111,   111,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,   110,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,   116,   111,   111,   111,   111,   111,    52,
      53,   111,   111,   111,   111,   111,   111,   111,   111,    68,
      68,     7,   183,   115,   188,    -1,    -1,    -1,    -1,    72,
      -1,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    94,    95,    96,    97,    98,    99,   100,   101,    -1,
      -1,   104,   105,    -1,    -1,    -1,    -1,   110,    -1,    -1,
     183,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   221,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    -1,    -1,    -1,    -1,    -1,    -1,    52,    53,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      94,    95,    96,    97,    98,    99,   100,   101,    50,    51,
     104,   105,    -1,    55,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    -1,    -1,    -1,    71,
      -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    87,    88,    89,    90,    91,
      92,    93,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     102,   103,    -1,    -1,   106
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,   115,   116,     0,     1,   112,   117,   118,   119,   120,
      68,    68,   117,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    50,    51,    52,    53,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    71,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,   103,   104,   105,   106,
     107,   108,   109,   110,   121,   122,   123,   124,   126,   127,
     129,   130,   132,   137,    46,    47,    48,    49,   128,   128,
     128,   128,   128,   128,   128,   128,   128,   128,   113,   113,
     128,   128,   128,   128,   128,   128,   113,   113,   111,   111,
     128,   128,   128,   128,   128,   128,   111,   111,   113,   111,
     111,   111,   111,   111,   113,   113,   113,   113,   113,   113,
     113,   113,   113,   113,   113,   113,   111,   128,   128,   111,
     111,   111,   113,   123,   129,   126,    69,    70,   131,   111,
     111,   111,   111,   111,   111,   111,   111,   111,   111,   111,
     111,   111,   111,   111,   111,   133,   134,   111,   111,   111,
     111,   111,   111,   135,   136,   111,   111,   111,   138,   111,
     111,   124,   129,   125,   126,   132,   128,   128,    54,    54,
     111,   129,   125,    68,   111,   111,   111,   111,   125,    68,
      68
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
#line 364 "matcher_parser_parse.y"
    {
	if (matcher_parse_op == MATCHER_PARSE_FILE) {
		prefs_filtering = &pre_global_processing;
	}
}
    break;

  case 7:

/* Line 1455 of yacc.c  */
#line 380 "matcher_parser_parse.y"
    { action_list = NULL; }
    break;

  case 9:

/* Line 1455 of yacc.c  */
#line 383 "matcher_parser_parse.y"
    {
	yyerrok;
}
    break;

  case 10:

/* Line 1455 of yacc.c  */
#line 389 "matcher_parser_parse.y"
    {
	gchar *folder = (yyvsp[(1) - (2)].str);
	FolderItem *item = NULL;

	if (matcher_parse_op == MATCHER_PARSE_FILE) {
                enable_compatibility = 0;
		if (!strcmp(folder, "global")) {
                        /* backward compatibility */
                        enable_compatibility = 1;
                }
		else if (!strcmp(folder, "preglobal")) {
			prefs_filtering = &pre_global_processing;
                }
		else if (!strcmp(folder, "postglobal")) {
			prefs_filtering = &post_global_processing;
                }
		else if (!strcmp(folder, "filtering")) {
                        prefs_filtering = &filtering_rules;
		}
                else {
			item = folder_find_item_from_identifier(folder);
			if (item != NULL) {
				prefs_filtering = &item->prefs->processing;
			} else {
				prefs_filtering = NULL;
			}
		}
	}
}
    break;

  case 16:

/* Line 1455 of yacc.c  */
#line 427 "matcher_parser_parse.y"
    {
	if (matcher_parse_op == MATCHER_PARSE_NO_EOL)
		YYACCEPT;
	else {
		matcher_parsererror("parse error [no eol]");
		YYERROR;
	}
}
    break;

  case 17:

/* Line 1455 of yacc.c  */
#line 436 "matcher_parser_parse.y"
    {
	if (matcher_parse_op == MATCHER_PARSE_ENABLED)
		YYACCEPT;
	else {
		matcher_parsererror("parse error [enabled]");
		YYERROR;
	}
}
    break;

  case 18:

/* Line 1455 of yacc.c  */
#line 445 "matcher_parser_parse.y"
    {
	if (matcher_parse_op == MATCHER_PARSE_ACCOUNT)
		YYACCEPT;
	else {
		matcher_parsererror("parse error [account]");
		YYERROR;
	}
}
    break;

  case 19:

/* Line 1455 of yacc.c  */
#line 454 "matcher_parser_parse.y"
    {
	if (matcher_parse_op == MATCHER_PARSE_NAME)
		YYACCEPT;
	else {
		matcher_parsererror("parse error [name]");
		YYERROR;
	}
}
    break;

  case 20:

/* Line 1455 of yacc.c  */
#line 463 "matcher_parser_parse.y"
    {
	if (matcher_parse_op == MATCHER_PARSE_CONDITION)
		YYACCEPT;
	else {
		matcher_parsererror("parse error [condition]");
		YYERROR;
	}
}
    break;

  case 21:

/* Line 1455 of yacc.c  */
#line 472 "matcher_parser_parse.y"
    {
	if (matcher_parse_op == MATCHER_PARSE_FILTERING_ACTION)
		YYACCEPT;
	else {
		matcher_parsererror("parse error [filtering action]");
		YYERROR;
	}
}
    break;

  case 23:

/* Line 1455 of yacc.c  */
#line 485 "matcher_parser_parse.y"
    {
	enabled = TRUE;
}
    break;

  case 24:

/* Line 1455 of yacc.c  */
#line 489 "matcher_parser_parse.y"
    {
	enabled = FALSE;
}
    break;

  case 25:

/* Line 1455 of yacc.c  */
#line 496 "matcher_parser_parse.y"
    {
	name = g_strdup((yyvsp[(2) - (2)].str));
}
    break;

  case 26:

/* Line 1455 of yacc.c  */
#line 503 "matcher_parser_parse.y"
    {
	account_id = strtol((yyvsp[(2) - (2)].str), NULL, 10);
}
    break;

  case 27:

/* Line 1455 of yacc.c  */
#line 510 "matcher_parser_parse.y"
    {
	filtering = filteringprop_new(enabled, name, account_id, cond, action_list);
	enabled = TRUE;
	account_id = 0;
	g_free(name);
	name = NULL;
        if (enable_compatibility) {
                prefs_filtering = &filtering_rules;
                if (action_list != NULL) {
                        FilteringAction * first_action;
                        
                        first_action = action_list->data;
                        
                        if (first_action->type == MATCHACTION_CHANGE_SCORE)
                                prefs_filtering = &pre_global_processing;
                }
        }
        
	cond = NULL;
	action_list = NULL;
        
	if ((matcher_parse_op == MATCHER_PARSE_FILE) &&
            (prefs_filtering != NULL)) {
		*prefs_filtering = g_slist_append(*prefs_filtering,
						  filtering);
		filtering = NULL;
	}
}
    break;

  case 30:

/* Line 1455 of yacc.c  */
#line 547 "matcher_parser_parse.y"
    {
        action_list = g_slist_append(action_list, action);
        action = NULL;
}
    break;

  case 31:

/* Line 1455 of yacc.c  */
#line 555 "matcher_parser_parse.y"
    {
	match_type = MATCHTYPE_MATCHCASE;
}
    break;

  case 32:

/* Line 1455 of yacc.c  */
#line 559 "matcher_parser_parse.y"
    {
	match_type = MATCHTYPE_MATCH;
}
    break;

  case 33:

/* Line 1455 of yacc.c  */
#line 563 "matcher_parser_parse.y"
    {
	match_type = MATCHTYPE_REGEXPCASE;
}
    break;

  case 34:

/* Line 1455 of yacc.c  */
#line 567 "matcher_parser_parse.y"
    {
	match_type = MATCHTYPE_REGEXP;
}
    break;

  case 35:

/* Line 1455 of yacc.c  */
#line 574 "matcher_parser_parse.y"
    {
	cond = matcherlist_new(matchers_list, (bool_op == MATCHERBOOL_AND));
	matchers_list = NULL;
}
    break;

  case 36:

/* Line 1455 of yacc.c  */
#line 582 "matcher_parser_parse.y"
    {
	matchers_list = g_slist_append(matchers_list, prop);
}
    break;

  case 37:

/* Line 1455 of yacc.c  */
#line 586 "matcher_parser_parse.y"
    {
	matchers_list = NULL;
	matchers_list = g_slist_append(matchers_list, prop);
}
    break;

  case 38:

/* Line 1455 of yacc.c  */
#line 594 "matcher_parser_parse.y"
    {
	bool_op = MATCHERBOOL_AND;
}
    break;

  case 39:

/* Line 1455 of yacc.c  */
#line 598 "matcher_parser_parse.y"
    {
	bool_op = MATCHERBOOL_OR;
}
    break;

  case 40:

/* Line 1455 of yacc.c  */
#line 605 "matcher_parser_parse.y"
    {
	gint criteria = 0;

	criteria = MATCHCRITERIA_ALL;
	prop = matcherprop_new(criteria, NULL, 0, NULL, 0);
}
    break;

  case 41:

/* Line 1455 of yacc.c  */
#line 612 "matcher_parser_parse.y"
    {
	gint criteria = 0;

	criteria = MATCHCRITERIA_UNREAD;
	prop = matcherprop_new(criteria, NULL, 0, NULL, 0);
}
    break;

  case 42:

/* Line 1455 of yacc.c  */
#line 619 "matcher_parser_parse.y"
    {
	gint criteria = 0;

	criteria = MATCHCRITERIA_NOT_UNREAD;
	prop = matcherprop_new(criteria, NULL, 0, NULL, 0);
}
    break;

  case 43:

/* Line 1455 of yacc.c  */
#line 626 "matcher_parser_parse.y"
    {
	gint criteria = 0;

	criteria = MATCHCRITERIA_NEW;
	prop = matcherprop_new(criteria, NULL, 0, NULL, 0);
}
    break;

  case 44:

/* Line 1455 of yacc.c  */
#line 633 "matcher_parser_parse.y"
    {
	gint criteria = 0;

	criteria = MATCHCRITERIA_NOT_NEW;
	prop = matcherprop_new(criteria, NULL, 0, NULL, 0);
}
    break;

  case 45:

/* Line 1455 of yacc.c  */
#line 640 "matcher_parser_parse.y"
    {
	gint criteria = 0;

	criteria = MATCHCRITERIA_MARKED;
	prop = matcherprop_new(criteria, NULL, 0, NULL, 0);
}
    break;

  case 46:

/* Line 1455 of yacc.c  */
#line 647 "matcher_parser_parse.y"
    {
	gint criteria = 0;

	criteria = MATCHCRITERIA_NOT_MARKED;
	prop = matcherprop_new(criteria, NULL, 0, NULL, 0);
}
    break;

  case 47:

/* Line 1455 of yacc.c  */
#line 654 "matcher_parser_parse.y"
    {
	gint criteria = 0;

	criteria = MATCHCRITERIA_DELETED;
	prop = matcherprop_new(criteria, NULL, 0, NULL, 0);
}
    break;

  case 48:

/* Line 1455 of yacc.c  */
#line 661 "matcher_parser_parse.y"
    {
	gint criteria = 0;

	criteria = MATCHCRITERIA_NOT_DELETED;
	prop = matcherprop_new(criteria, NULL, 0, NULL, 0);
}
    break;

  case 49:

/* Line 1455 of yacc.c  */
#line 668 "matcher_parser_parse.y"
    {
	gint criteria = 0;

	criteria = MATCHCRITERIA_REPLIED;
	prop = matcherprop_new(criteria, NULL, 0, NULL, 0);
}
    break;

  case 50:

/* Line 1455 of yacc.c  */
#line 675 "matcher_parser_parse.y"
    {
	gint criteria = 0;

	criteria = MATCHCRITERIA_NOT_REPLIED;
	prop = matcherprop_new(criteria, NULL, 0, NULL, 0);
}
    break;

  case 51:

/* Line 1455 of yacc.c  */
#line 682 "matcher_parser_parse.y"
    {
	gint criteria = 0;

	criteria = MATCHCRITERIA_FORWARDED;
	prop = matcherprop_new(criteria, NULL, 0, NULL, 0);
}
    break;

  case 52:

/* Line 1455 of yacc.c  */
#line 689 "matcher_parser_parse.y"
    {
	gint criteria = 0;

	criteria = MATCHCRITERIA_NOT_FORWARDED;
	prop = matcherprop_new(criteria, NULL, 0, NULL, 0);
}
    break;

  case 53:

/* Line 1455 of yacc.c  */
#line 696 "matcher_parser_parse.y"
    {
	gint criteria = 0;

	criteria = MATCHCRITERIA_LOCKED;
	prop = matcherprop_new(criteria, NULL, 0, NULL, 0);
}
    break;

  case 54:

/* Line 1455 of yacc.c  */
#line 703 "matcher_parser_parse.y"
    {
	gint criteria = 0;

	criteria = MATCHCRITERIA_NOT_LOCKED;
	prop = matcherprop_new(criteria, NULL, 0, NULL, 0);
}
    break;

  case 55:

/* Line 1455 of yacc.c  */
#line 710 "matcher_parser_parse.y"
    {
	gint criteria = 0;

	criteria = MATCHCRITERIA_SPAM;
	prop = matcherprop_new(criteria, NULL, 0, NULL, 0);
}
    break;

  case 56:

/* Line 1455 of yacc.c  */
#line 717 "matcher_parser_parse.y"
    {
	gint criteria = 0;

	criteria = MATCHCRITERIA_NOT_SPAM;
	prop = matcherprop_new(criteria, NULL, 0, NULL, 0);
}
    break;

  case 57:

/* Line 1455 of yacc.c  */
#line 724 "matcher_parser_parse.y"
    {
	gint criteria = 0;

	criteria = MATCHCRITERIA_HAS_ATTACHMENT;
	prop = matcherprop_new(criteria, NULL, 0, NULL, 0);
}
    break;

  case 58:

/* Line 1455 of yacc.c  */
#line 731 "matcher_parser_parse.y"
    {
	gint criteria = 0;

	criteria = MATCHCRITERIA_HAS_NO_ATTACHMENT;
	prop = matcherprop_new(criteria, NULL, 0, NULL, 0);
}
    break;

  case 59:

/* Line 1455 of yacc.c  */
#line 738 "matcher_parser_parse.y"
    {
	gint criteria = 0;

	criteria = MATCHCRITERIA_SIGNED;
	prop = matcherprop_new(criteria, NULL, 0, NULL, 0);
}
    break;

  case 60:

/* Line 1455 of yacc.c  */
#line 745 "matcher_parser_parse.y"
    {
	gint criteria = 0;

	criteria = MATCHCRITERIA_NOT_SIGNED;
	prop = matcherprop_new(criteria, NULL, 0, NULL, 0);
}
    break;

  case 61:

/* Line 1455 of yacc.c  */
#line 752 "matcher_parser_parse.y"
    {
	gint criteria = 0;

	criteria = MATCHCRITERIA_PARTIAL;
	prop = matcherprop_new(criteria, NULL, 0, NULL, 0);
}
    break;

  case 62:

/* Line 1455 of yacc.c  */
#line 759 "matcher_parser_parse.y"
    {
	gint criteria = 0;

	criteria = MATCHCRITERIA_NOT_PARTIAL;
	prop = matcherprop_new(criteria, NULL, 0, NULL, 0);
}
    break;

  case 63:

/* Line 1455 of yacc.c  */
#line 766 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gint value = 0;

	criteria = MATCHCRITERIA_COLORLABEL;
	value = strtol((yyvsp[(2) - (2)].str), NULL, 10);
	if (value < 0) value = 0;
	else if (value > COLORLABELS) value = COLORLABELS;
	prop = matcherprop_new(criteria, NULL, 0, NULL, value);
}
    break;

  case 64:

/* Line 1455 of yacc.c  */
#line 777 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gint value = 0;

	criteria = MATCHCRITERIA_NOT_COLORLABEL;
	value = strtol((yyvsp[(2) - (2)].str), NULL, 0);
	if (value < 0) value = 0;
	else if (value > COLORLABELS) value = COLORLABELS;
	prop = matcherprop_new(criteria, NULL, 0, NULL, value);
}
    break;

  case 65:

/* Line 1455 of yacc.c  */
#line 788 "matcher_parser_parse.y"
    {
	gint criteria = 0;

	criteria = MATCHCRITERIA_IGNORE_THREAD;
	prop = matcherprop_new(criteria, NULL, 0, NULL, 0);
}
    break;

  case 66:

/* Line 1455 of yacc.c  */
#line 795 "matcher_parser_parse.y"
    {
	gint criteria = 0;

	criteria = MATCHCRITERIA_NOT_IGNORE_THREAD;
	prop = matcherprop_new(criteria, NULL, 0, NULL, 0);
}
    break;

  case 67:

/* Line 1455 of yacc.c  */
#line 802 "matcher_parser_parse.y"
    {
	gint criteria = 0;

	criteria = MATCHCRITERIA_WATCH_THREAD;
	prop = matcherprop_new(criteria, NULL, 0, NULL, 0);
}
    break;

  case 68:

/* Line 1455 of yacc.c  */
#line 809 "matcher_parser_parse.y"
    {
	gint criteria = 0;

	criteria = MATCHCRITERIA_NOT_WATCH_THREAD;
	prop = matcherprop_new(criteria, NULL, 0, NULL, 0);
}
    break;

  case 69:

/* Line 1455 of yacc.c  */
#line 816 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gchar *expr = NULL;

	criteria = MATCHCRITERIA_SUBJECT;
	expr = (yyvsp[(3) - (3)].str);
	prop = matcherprop_new(criteria, NULL, match_type, expr, 0);
}
    break;

  case 70:

/* Line 1455 of yacc.c  */
#line 825 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gchar *expr = NULL;

	criteria = MATCHCRITERIA_NOT_SUBJECT;
	expr = (yyvsp[(3) - (3)].str);
	prop = matcherprop_new(criteria, NULL, match_type, expr, 0);
}
    break;

  case 71:

/* Line 1455 of yacc.c  */
#line 834 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gchar *expr = NULL;

	criteria = MATCHCRITERIA_FROM;
	expr = (yyvsp[(3) - (3)].str);
	prop = matcherprop_new(criteria, NULL, match_type, expr, 0);
}
    break;

  case 72:

/* Line 1455 of yacc.c  */
#line 843 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gchar *expr = NULL;

	criteria = MATCHCRITERIA_NOT_FROM;
	expr = (yyvsp[(3) - (3)].str);
	prop = matcherprop_new(criteria, NULL, match_type, expr, 0);
}
    break;

  case 73:

/* Line 1455 of yacc.c  */
#line 852 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gchar *expr = NULL;

	criteria = MATCHCRITERIA_TO;
	expr = (yyvsp[(3) - (3)].str);
	prop = matcherprop_new(criteria, NULL, match_type, expr, 0);
}
    break;

  case 74:

/* Line 1455 of yacc.c  */
#line 861 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gchar *expr = NULL;

	criteria = MATCHCRITERIA_NOT_TO;
	expr = (yyvsp[(3) - (3)].str);
	prop = matcherprop_new(criteria, NULL, match_type, expr, 0);
}
    break;

  case 75:

/* Line 1455 of yacc.c  */
#line 870 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gchar *expr = NULL;

	criteria = MATCHCRITERIA_CC;
	expr = (yyvsp[(3) - (3)].str);
	prop = matcherprop_new(criteria, NULL, match_type, expr, 0);
}
    break;

  case 76:

/* Line 1455 of yacc.c  */
#line 879 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gchar *expr = NULL;

	criteria = MATCHCRITERIA_NOT_CC;
	expr = (yyvsp[(3) - (3)].str);
	prop = matcherprop_new(criteria, NULL, match_type, expr, 0);
}
    break;

  case 77:

/* Line 1455 of yacc.c  */
#line 888 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gchar *expr = NULL;

	criteria = MATCHCRITERIA_TO_OR_CC;
	expr = (yyvsp[(3) - (3)].str);
	prop = matcherprop_new(criteria, NULL, match_type, expr, 0);
}
    break;

  case 78:

/* Line 1455 of yacc.c  */
#line 897 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gchar *expr = NULL;

	criteria = MATCHCRITERIA_NOT_TO_AND_NOT_CC;
	expr = (yyvsp[(3) - (3)].str);
	prop = matcherprop_new(criteria, NULL, match_type, expr, 0);
}
    break;

  case 79:

/* Line 1455 of yacc.c  */
#line 906 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gchar *expr = NULL;

	criteria = MATCHCRITERIA_TAG;
	expr = (yyvsp[(3) - (3)].str);
	prop = matcherprop_new(criteria, NULL, match_type, expr, 0);
}
    break;

  case 80:

/* Line 1455 of yacc.c  */
#line 915 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gchar *expr = NULL;

	criteria = MATCHCRITERIA_NOT_TAG;
	expr = (yyvsp[(3) - (3)].str);
	prop = matcherprop_new(criteria, NULL, match_type, expr, 0);
}
    break;

  case 81:

/* Line 1455 of yacc.c  */
#line 924 "matcher_parser_parse.y"
    {
	gint criteria = 0;

	criteria = MATCHCRITERIA_TAGGED;
	prop = matcherprop_new(criteria, NULL, 0, NULL, 0);
}
    break;

  case 82:

/* Line 1455 of yacc.c  */
#line 931 "matcher_parser_parse.y"
    {
	gint criteria = 0;

	criteria = MATCHCRITERIA_NOT_TAGGED;
	prop = matcherprop_new(criteria, NULL, 0, NULL, 0);
}
    break;

  case 83:

/* Line 1455 of yacc.c  */
#line 938 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gint value = 0;

	criteria = MATCHCRITERIA_AGE_GREATER;
	value = strtol((yyvsp[(2) - (2)].str), NULL, 0);
	prop = matcherprop_new(criteria, NULL, 0, NULL, value);
}
    break;

  case 84:

/* Line 1455 of yacc.c  */
#line 947 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gint value = 0;

	criteria = MATCHCRITERIA_AGE_LOWER;
	value = strtol((yyvsp[(2) - (2)].str), NULL, 0);
	prop = matcherprop_new(criteria, NULL, 0, NULL, value);
}
    break;

  case 85:

/* Line 1455 of yacc.c  */
#line 956 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gchar *expr = NULL;

	criteria = MATCHCRITERIA_NEWSGROUPS;
	expr = (yyvsp[(3) - (3)].str);
	prop = matcherprop_new(criteria, NULL, match_type, expr, 0);
}
    break;

  case 86:

/* Line 1455 of yacc.c  */
#line 965 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gchar *expr = NULL;

	criteria = MATCHCRITERIA_NOT_NEWSGROUPS;
	expr = (yyvsp[(3) - (3)].str);
	prop = matcherprop_new(criteria, NULL, match_type, expr, 0);
}
    break;

  case 87:

/* Line 1455 of yacc.c  */
#line 974 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gchar *expr = NULL;

	criteria = MATCHCRITERIA_INREPLYTO;
	expr = (yyvsp[(3) - (3)].str);
	prop = matcherprop_new(criteria, NULL, match_type, expr, 0);
}
    break;

  case 88:

/* Line 1455 of yacc.c  */
#line 983 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gchar *expr = NULL;

	criteria = MATCHCRITERIA_NOT_INREPLYTO;
	expr = (yyvsp[(3) - (3)].str);
	prop = matcherprop_new(criteria, NULL, match_type, expr, 0);
}
    break;

  case 89:

/* Line 1455 of yacc.c  */
#line 992 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gchar *expr = NULL;

	criteria = MATCHCRITERIA_REFERENCES;
	expr = (yyvsp[(3) - (3)].str);
	prop = matcherprop_new(criteria, NULL, match_type, expr, 0);
}
    break;

  case 90:

/* Line 1455 of yacc.c  */
#line 1001 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gchar *expr = NULL;

	criteria = MATCHCRITERIA_NOT_REFERENCES;
	expr = (yyvsp[(3) - (3)].str);
	prop = matcherprop_new(criteria, NULL, match_type, expr, 0);
}
    break;

  case 91:

/* Line 1455 of yacc.c  */
#line 1010 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gint value = 0;

	criteria = MATCHCRITERIA_SCORE_GREATER;
	value = strtol((yyvsp[(2) - (2)].str), NULL, 0);
	prop = matcherprop_new(criteria, NULL, 0, NULL, value);
}
    break;

  case 92:

/* Line 1455 of yacc.c  */
#line 1019 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gint value = 0;

	criteria = MATCHCRITERIA_SCORE_LOWER;
	value = strtol((yyvsp[(2) - (2)].str), NULL, 0);
	prop = matcherprop_new(criteria, NULL, 0, NULL, value);
}
    break;

  case 93:

/* Line 1455 of yacc.c  */
#line 1028 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gint value = 0;

	criteria = MATCHCRITERIA_SCORE_EQUAL;
	value = strtol((yyvsp[(2) - (2)].str), NULL, 0);
	prop = matcherprop_new(criteria, NULL, 0, NULL, value);
}
    break;

  case 94:

/* Line 1455 of yacc.c  */
#line 1037 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gint value    = 0;
	criteria = MATCHCRITERIA_SIZE_GREATER;
	value = strtol((yyvsp[(2) - (2)].str), NULL, 0);
	prop = matcherprop_new(criteria, NULL, 0, NULL, value);
}
    break;

  case 95:

/* Line 1455 of yacc.c  */
#line 1045 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gint value    = 0;
	criteria = MATCHCRITERIA_SIZE_SMALLER;
	value = strtol((yyvsp[(2) - (2)].str), NULL, 0);
	prop = matcherprop_new(criteria, NULL, 0, NULL, value);
}
    break;

  case 96:

/* Line 1455 of yacc.c  */
#line 1053 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gint value    = 0;
	criteria = MATCHCRITERIA_SIZE_EQUAL;
	value = strtol((yyvsp[(2) - (2)].str), NULL, 0);
	prop = matcherprop_new(criteria, NULL, 0, NULL, value);
}
    break;

  case 97:

/* Line 1455 of yacc.c  */
#line 1061 "matcher_parser_parse.y"
    {
	header = g_strdup((yyvsp[(2) - (2)].str));
}
    break;

  case 98:

/* Line 1455 of yacc.c  */
#line 1064 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gchar *expr = NULL;
	matcher_is_fast = FALSE;
	criteria = MATCHCRITERIA_HEADER;
	expr = (yyvsp[(2) - (5)].str);
	prop = matcherprop_new(criteria, header, match_type, expr, 0);
	g_free(header);
}
    break;

  case 99:

/* Line 1455 of yacc.c  */
#line 1074 "matcher_parser_parse.y"
    {
	header = g_strdup((yyvsp[(2) - (2)].str));
}
    break;

  case 100:

/* Line 1455 of yacc.c  */
#line 1077 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gchar *expr = NULL;
	matcher_is_fast = FALSE;
	criteria = MATCHCRITERIA_NOT_HEADER;
	expr = (yyvsp[(2) - (5)].str);
	prop = matcherprop_new(criteria, header, match_type, expr, 0);
	g_free(header);
}
    break;

  case 101:

/* Line 1455 of yacc.c  */
#line 1087 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gchar *expr = NULL;
	matcher_is_fast = FALSE;
	criteria = MATCHCRITERIA_HEADERS_PART;
	expr = (yyvsp[(3) - (3)].str);
	prop = matcherprop_new(criteria, NULL, match_type, expr, 0);
}
    break;

  case 102:

/* Line 1455 of yacc.c  */
#line 1096 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gchar *expr = NULL;
	matcher_is_fast = FALSE;
	criteria = MATCHCRITERIA_NOT_HEADERS_PART;
	expr = (yyvsp[(3) - (3)].str);
	prop = matcherprop_new(criteria, NULL, match_type, expr, 0);
}
    break;

  case 103:

/* Line 1455 of yacc.c  */
#line 1105 "matcher_parser_parse.y"
    {
	header = g_strdup((yyvsp[(2) - (2)].str));
}
    break;

  case 104:

/* Line 1455 of yacc.c  */
#line 1108 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gchar *expr = NULL;

	criteria = MATCHCRITERIA_FOUND_IN_ADDRESSBOOK;
	expr = (yyvsp[(2) - (5)].str);
	prop = matcherprop_new(criteria, header, match_type, expr, 0);
	g_free(header);
}
    break;

  case 105:

/* Line 1455 of yacc.c  */
#line 1118 "matcher_parser_parse.y"
    {
	header = g_strdup((yyvsp[(2) - (2)].str));
}
    break;

  case 106:

/* Line 1455 of yacc.c  */
#line 1121 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gchar *expr = NULL;

	criteria = MATCHCRITERIA_NOT_FOUND_IN_ADDRESSBOOK;
	expr = (yyvsp[(2) - (5)].str);
	prop = matcherprop_new(criteria, header, match_type, expr, 0);
	g_free(header);
}
    break;

  case 107:

/* Line 1455 of yacc.c  */
#line 1131 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gchar *expr = NULL;
	matcher_is_fast = FALSE;
	criteria = MATCHCRITERIA_MESSAGE;
	expr = (yyvsp[(3) - (3)].str);
	prop = matcherprop_new(criteria, NULL, match_type, expr, 0);
}
    break;

  case 108:

/* Line 1455 of yacc.c  */
#line 1140 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gchar *expr = NULL;
	matcher_is_fast = FALSE;
	criteria = MATCHCRITERIA_NOT_MESSAGE;
	expr = (yyvsp[(3) - (3)].str);
	prop = matcherprop_new(criteria, NULL, match_type, expr, 0);
}
    break;

  case 109:

/* Line 1455 of yacc.c  */
#line 1149 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gchar *expr = NULL;
	matcher_is_fast = FALSE;
	criteria = MATCHCRITERIA_BODY_PART;
	expr = (yyvsp[(3) - (3)].str);
	prop = matcherprop_new(criteria, NULL, match_type, expr, 0);
}
    break;

  case 110:

/* Line 1455 of yacc.c  */
#line 1158 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gchar *expr = NULL;
	matcher_is_fast = FALSE;
	criteria = MATCHCRITERIA_NOT_BODY_PART;
	expr = (yyvsp[(3) - (3)].str);
	prop = matcherprop_new(criteria, NULL, match_type, expr, 0);
}
    break;

  case 111:

/* Line 1455 of yacc.c  */
#line 1167 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gchar *expr = NULL;
	matcher_is_fast = FALSE;
	criteria = MATCHCRITERIA_TEST;
	expr = (yyvsp[(2) - (2)].str);
	prop = matcherprop_new(criteria, NULL, MATCHTYPE_MATCH, expr, 0);
}
    break;

  case 112:

/* Line 1455 of yacc.c  */
#line 1176 "matcher_parser_parse.y"
    {
	gint criteria = 0;
	gchar *expr = NULL;
	matcher_is_fast = FALSE;
	criteria = MATCHCRITERIA_NOT_TEST;
	expr = (yyvsp[(2) - (2)].str);
	prop = matcherprop_new(criteria, NULL, MATCHTYPE_MATCH, expr, 0);
}
    break;

  case 113:

/* Line 1455 of yacc.c  */
#line 1188 "matcher_parser_parse.y"
    {
	gchar *cmd = NULL;
	gint action_type = 0;

	action_type = MATCHACTION_EXECUTE;
	cmd = (yyvsp[(2) - (2)].str);
	action = filteringaction_new(action_type, 0, cmd, 0, 0, NULL);
}
    break;

  case 114:

/* Line 1455 of yacc.c  */
#line 1197 "matcher_parser_parse.y"
    {
	gchar *destination = NULL;
	gint action_type = 0;

	action_type = MATCHACTION_MOVE;
	destination = (yyvsp[(2) - (2)].str);
	action = filteringaction_new(action_type, 0, destination, 0, 0, NULL);
}
    break;

  case 115:

/* Line 1455 of yacc.c  */
#line 1206 "matcher_parser_parse.y"
    {
	gchar *destination = NULL;
	gint action_type = 0;

	action_type = MATCHACTION_SET_TAG;
	destination = (yyvsp[(2) - (2)].str);
	action = filteringaction_new(action_type, 0, destination, 0, 0, NULL);
}
    break;

  case 116:

/* Line 1455 of yacc.c  */
#line 1215 "matcher_parser_parse.y"
    {
	gchar *destination = NULL;
	gint action_type = 0;

	action_type = MATCHACTION_UNSET_TAG;
	destination = (yyvsp[(2) - (2)].str);
	action = filteringaction_new(action_type, 0, destination, 0, 0, NULL);
}
    break;

  case 117:

/* Line 1455 of yacc.c  */
#line 1224 "matcher_parser_parse.y"
    {
	gint action_type = 0;

	action_type = MATCHACTION_CLEAR_TAGS;
	action = filteringaction_new(action_type, 0, NULL, 0, 0, NULL);
}
    break;

  case 118:

/* Line 1455 of yacc.c  */
#line 1231 "matcher_parser_parse.y"
    {
	gchar *destination = NULL;
	gint action_type = 0;

	action_type = MATCHACTION_COPY;
	destination = (yyvsp[(2) - (2)].str);
	action = filteringaction_new(action_type, 0, destination, 0, 0, NULL);
}
    break;

  case 119:

/* Line 1455 of yacc.c  */
#line 1240 "matcher_parser_parse.y"
    {
	gint action_type = 0;

	action_type = MATCHACTION_DELETE;
	action = filteringaction_new(action_type, 0, NULL, 0, 0, NULL);
}
    break;

  case 120:

/* Line 1455 of yacc.c  */
#line 1247 "matcher_parser_parse.y"
    {
	gint action_type = 0;

	action_type = MATCHACTION_MARK;
	action = filteringaction_new(action_type, 0, NULL, 0, 0, NULL);
}
    break;

  case 121:

/* Line 1455 of yacc.c  */
#line 1254 "matcher_parser_parse.y"
    {
	gint action_type = 0;

	action_type = MATCHACTION_UNMARK;
	action = filteringaction_new(action_type, 0, NULL, 0, 0, NULL);
}
    break;

  case 122:

/* Line 1455 of yacc.c  */
#line 1261 "matcher_parser_parse.y"
    {
	gint action_type = 0;

	action_type = MATCHACTION_LOCK;
	action = filteringaction_new(action_type, 0, NULL, 0, 0, NULL);
}
    break;

  case 123:

/* Line 1455 of yacc.c  */
#line 1268 "matcher_parser_parse.y"
    {
	gint action_type = 0;

	action_type = MATCHACTION_UNLOCK;
	action = filteringaction_new(action_type, 0, NULL, 0, 0, NULL);
}
    break;

  case 124:

/* Line 1455 of yacc.c  */
#line 1275 "matcher_parser_parse.y"
    {
	gint action_type = 0;

	action_type = MATCHACTION_MARK_AS_READ;
	action = filteringaction_new(action_type, 0, NULL, 0, 0, NULL);
}
    break;

  case 125:

/* Line 1455 of yacc.c  */
#line 1282 "matcher_parser_parse.y"
    {
	gint action_type = 0;

	action_type = MATCHACTION_MARK_AS_UNREAD;
	action = filteringaction_new(action_type, 0, NULL, 0, 0, NULL);
}
    break;

  case 126:

/* Line 1455 of yacc.c  */
#line 1289 "matcher_parser_parse.y"
    {
	gint action_type = 0;

	action_type = MATCHACTION_MARK_AS_SPAM;
	action = filteringaction_new(action_type, 0, NULL, 0, 0, NULL);
}
    break;

  case 127:

/* Line 1455 of yacc.c  */
#line 1296 "matcher_parser_parse.y"
    {
	gint action_type = 0;

	action_type = MATCHACTION_MARK_AS_HAM;
	action = filteringaction_new(action_type, 0, NULL, 0, 0, NULL);
}
    break;

  case 128:

/* Line 1455 of yacc.c  */
#line 1303 "matcher_parser_parse.y"
    {
	gchar *destination = NULL;
	gint action_type = 0;
	gint account_id = 0;

	action_type = MATCHACTION_FORWARD;
	account_id = strtol((yyvsp[(2) - (3)].str), NULL, 10);
	destination = (yyvsp[(3) - (3)].str);
	action = filteringaction_new(action_type,
            account_id, destination, 0, 0, NULL);
}
    break;

  case 129:

/* Line 1455 of yacc.c  */
#line 1315 "matcher_parser_parse.y"
    {
	gchar *destination = NULL;
	gint action_type = 0;
	gint account_id = 0;

	action_type = MATCHACTION_FORWARD_AS_ATTACHMENT;
	account_id = strtol((yyvsp[(2) - (3)].str), NULL, 10);
	destination = (yyvsp[(3) - (3)].str);
	action = filteringaction_new(action_type,
            account_id, destination, 0, 0, NULL);
}
    break;

  case 130:

/* Line 1455 of yacc.c  */
#line 1327 "matcher_parser_parse.y"
    {
	gchar *destination = NULL;
	gint action_type = 0;
	gint account_id = 0;

	action_type = MATCHACTION_REDIRECT;
	account_id = strtol((yyvsp[(2) - (3)].str), NULL, 10);
	destination = (yyvsp[(3) - (3)].str);
	action = filteringaction_new(action_type,
            account_id, destination, 0, 0, NULL);
}
    break;

  case 131:

/* Line 1455 of yacc.c  */
#line 1339 "matcher_parser_parse.y"
    {
	gint action_type = 0;
	gint color = 0;

	action_type = MATCHACTION_COLOR;
	color = strtol((yyvsp[(2) - (2)].str), NULL, 10);
	action = filteringaction_new(action_type, 0, NULL, color, 0, NULL);
}
    break;

  case 132:

/* Line 1455 of yacc.c  */
#line 1348 "matcher_parser_parse.y"
    {
        gint score = 0;
        
        score = strtol((yyvsp[(2) - (2)].str), NULL, 10);
	action = filteringaction_new(MATCHACTION_CHANGE_SCORE, 0,
				     NULL, 0, score, NULL);
}
    break;

  case 133:

/* Line 1455 of yacc.c  */
#line 1357 "matcher_parser_parse.y"
    {
        gint score = 0;
        
        score = strtol((yyvsp[(2) - (2)].str), NULL, 10);
	action = filteringaction_new(MATCHACTION_CHANGE_SCORE, 0,
				     NULL, 0, score, NULL);
}
    break;

  case 134:

/* Line 1455 of yacc.c  */
#line 1365 "matcher_parser_parse.y"
    {
        gint score = 0;
        
        score = strtol((yyvsp[(2) - (2)].str), NULL, 10);
	action = filteringaction_new(MATCHACTION_SET_SCORE, 0,
				     NULL, 0, score, NULL);
}
    break;

  case 135:

/* Line 1455 of yacc.c  */
#line 1373 "matcher_parser_parse.y"
    {
	action = filteringaction_new(MATCHACTION_HIDE, 0, NULL, 0, 0, NULL);
}
    break;

  case 136:

/* Line 1455 of yacc.c  */
#line 1377 "matcher_parser_parse.y"
    {
	action = filteringaction_new(MATCHACTION_IGNORE, 0, NULL, 0, 0, NULL);
}
    break;

  case 137:

/* Line 1455 of yacc.c  */
#line 1381 "matcher_parser_parse.y"
    {
	action = filteringaction_new(MATCHACTION_WATCH, 0, NULL, 0, 0, NULL);
}
    break;

  case 138:

/* Line 1455 of yacc.c  */
#line 1385 "matcher_parser_parse.y"
    {
	header = g_strdup((yyvsp[(2) - (2)].str));
}
    break;

  case 139:

/* Line 1455 of yacc.c  */
#line 1388 "matcher_parser_parse.y"
    {
	gchar *addressbook = NULL;
	gint action_type = 0;

	action_type = MATCHACTION_ADD_TO_ADDRESSBOOK;
	addressbook = (yyvsp[(2) - (4)].str);
	action = filteringaction_new(action_type, 0, addressbook, 0, 0, header);
	g_free(header);
}
    break;

  case 140:

/* Line 1455 of yacc.c  */
#line 1398 "matcher_parser_parse.y"
    {
	action = filteringaction_new(MATCHACTION_STOP, 0, NULL, 0, 0, NULL);
}
    break;



/* Line 1455 of yacc.c  */
#line 3825 "matcher_parser_parse.c"
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



