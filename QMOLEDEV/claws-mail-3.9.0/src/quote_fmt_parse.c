/* A Bison parser, made by GNU Bison 2.5.  */

/* Bison implementation for Yacc-like parsers in C
   
      Copyright (C) 1984, 1989-1990, 2000-2011 Free Software Foundation, Inc.
   
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
#define YYBISON_VERSION "2.5"

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

/* Line 268 of yacc.c  */
#line 20 "quote_fmt_parse.y"


#include "defs.h"

#include <glib.h>
#include <glib/gi18n.h>

#include <ctype.h>

#include "procmsg.h"
#include "procmime.h"
#include "utils.h"
#include "codeconv.h"
#include "procheader.h"
#include "addr_compl.h"
#include "gtk/inputdialog.h"

#include "quote_fmt.h"
#include "quote_fmt_lex.h"
#include "account.h"

/* decl */
/*
flex quote_fmt.l
bison -p quote_fmt quote_fmt.y
*/

int yylex(void);

static MsgInfo *msginfo = NULL;
static PrefsAccount *account = NULL;
#ifdef USE_ENCHANT
static gchar default_dictionary[BUFFSIZE];
#endif
static gboolean *visible = NULL;
static gboolean dry_run = FALSE;
static gint maxsize = 0;
static gint stacksize = 0;
static GHashTable *var_table = NULL;
static GList *attachments = NULL;

typedef struct st_buffer
{
	gchar *buffer;
	gint bufsize;
	gint bufmax;
} st_buffer;

static struct st_buffer main_expr = { NULL, 0, 0 };
static struct st_buffer sub_expr = { NULL, 0, 0 };
static struct st_buffer* current = NULL;

static const gchar *quote_str = NULL;
static const gchar *body = NULL;
static gint error = 0;

static gint cursor_pos = -1;

extern int quote_fmt_firsttime;
extern int line;
extern int escaped_string;

static void add_visibility(gboolean val)
{
	stacksize++;
	if (maxsize < stacksize) {
		maxsize += 128;
		visible = g_realloc(visible, maxsize * sizeof(gboolean));
		if (visible == NULL)
			maxsize = 0;
	}

	visible[stacksize - 1] = val;
}

static void remove_visibility(void)
{
	stacksize--;
	if (stacksize < 0) {
		g_warning("Error: visibility stack underflow\n");
		stacksize = 0;
	}
}

static void add_buffer(const gchar *s)
{
	gint len;

	if (s == NULL)
		return;

	len = strlen(s);
	if (current->bufsize + len + 1 > current->bufmax) {
		if (current->bufmax == 0)
			current->bufmax = 128;
		while (current->bufsize + len + 1 > current->bufmax)
			current->bufmax *= 2;
		current->buffer = g_realloc(current->buffer, current->bufmax);
	}
	strcpy(current->buffer + current->bufsize, s);
	current->bufsize += len;
}

static void clear_buffer(void)
{
	if (current->buffer)
		*current->buffer = '\0';
	else
		/* force to an empty string, as buffer should not be left unallocated */
		add_buffer("");
	current->bufsize = 0;
}

gchar *quote_fmt_get_buffer(void)
{
	if (current != &main_expr)
		g_warning("Error: parser still in sub-expr mode\n");

	if (error != 0)
		return NULL;
	else
		return current->buffer;
}

GList *quote_fmt_get_attachments_list(void)
{
	return attachments;
}

gint quote_fmt_get_line(void)
{
	return line;
}

gint quote_fmt_get_cursor_pos(void)
{
	return cursor_pos;	
}

#define INSERT(buf) \
	if (stacksize != 0 && visible[stacksize - 1])\
		add_buffer(buf); \

#define INSERT_CHARACTER(chr) \
	if (stacksize != 0 && visible[stacksize - 1]) { \
		gchar tmp[2]; \
		tmp[0] = (chr); \
		tmp[1] = '\0'; \
		add_buffer(tmp); \
	}

void quote_fmt_reset_vartable(void)
{
	if (var_table) {
		g_hash_table_destroy(var_table);
		var_table = NULL;
	}
	if (attachments) {
		GList *cur = attachments;
		while (cur) {
			g_free(cur->data);
			cur = g_list_next(cur);
		}
		g_list_free(attachments);
		attachments = NULL;
	}
}

#ifdef USE_ENCHANT
void quote_fmt_init(MsgInfo *info, const gchar *my_quote_str,
		    const gchar *my_body, gboolean my_dry_run,
			PrefsAccount *compose_account,
			gboolean string_is_escaped,
			GtkAspell *compose_gtkaspell)
#else
void quote_fmt_init(MsgInfo *info, const gchar *my_quote_str,
		    const gchar *my_body, gboolean my_dry_run,
			PrefsAccount *compose_account,
			gboolean string_is_escaped)
#endif
{
	quote_str = my_quote_str;
	body = my_body;
	msginfo = info;
	account = compose_account;
#ifdef USE_ENCHANT
	gchar *dict = gtkaspell_get_default_dictionary(compose_gtkaspell);
	if (dict)
		strncpy2(default_dictionary, dict, sizeof(default_dictionary));
	else
		*default_dictionary = '\0';
#endif
	dry_run = my_dry_run;
	stacksize = 0;
	add_visibility(TRUE);
	main_expr.bufmax = 0;
	sub_expr.bufmax = 0;
	current = &main_expr;
	clear_buffer();
	error = 0;
	line = 1;
	escaped_string = string_is_escaped;

	if (!var_table)
		var_table = g_hash_table_new_full(g_str_hash, g_str_equal, 
				g_free, g_free);

        /*
         * force LEX initialization
         */
	quote_fmt_firsttime = 1;
	cursor_pos = -1;
}

void quote_fmterror(char *str)
{
	g_warning("Error: %s at line %d\n", str, line);
	error = 1;
}

int quote_fmtwrap(void)
{
	return 1;
}

static int isseparator(int ch)
{
	return g_ascii_isspace(ch) || ch == '.' || ch == '-';
}

static void quote_fmt_show_date(const MsgInfo *msginfo, const gchar *format)
{
	char  result[100];
	char *rptr;
	char  zone[6];
	struct tm lt;
	const char *fptr;
	const char *zptr;

	if (!msginfo->date)
		return;
	
	/* 
	 * ALF - GNU C's strftime() has a nice format specifier 
	 * for time zone offset (%z). Non-standard however, so 
	 * emulate it.
	 */

#define RLEFT (sizeof result) - (rptr - result)	
#define STR_SIZE(x) (sizeof (x) - 1)

	zone[0] = 0;

	if (procheader_date_parse_to_tm(msginfo->date, &lt, zone)) {
		/*
		 * break up format string in tiny bits delimited by valid %z's and 
		 * feed it to strftime(). don't forget that '%%z' mean literal '%z'.
		 */
		for (rptr = result, fptr = format; fptr && *fptr && rptr < &result[sizeof result - 1];) {
			int	    perc;
			const char *p;
			char	   *tmp;
			
			if (NULL != (zptr = strstr(fptr, "%z"))) {
				/*
				 * count nr. of prepended percent chars
				 */
				for (perc = 0, p = zptr; p && p >= format && *p == '%'; p--, perc++)
					;
				/*
				 * feed to strftime()
				 */
				tmp = g_strndup(fptr, zptr - fptr + (perc % 2 ? 0 : STR_SIZE("%z")));
				if (tmp) {
					rptr += strftime(rptr, RLEFT, tmp, &lt);
					g_free(tmp);
				}
				/*
				 * append time zone offset
				 */
				if (zone[0] && perc % 2) 
					rptr += g_snprintf(rptr, RLEFT, "%s", zone);
				fptr = zptr + STR_SIZE("%z");
			} else {
				rptr += strftime(rptr, RLEFT, fptr, &lt);
				fptr  = NULL;
			}
		}
		
		if (g_utf8_validate(result, -1, NULL)) {
			INSERT(result);
		} else {
			gchar *utf = conv_codeset_strdup(result, 
				conv_get_locale_charset_str_no_utf8(),
				CS_INTERNAL);
			if (utf == NULL || 
			    !g_utf8_validate(utf, -1, NULL)) {
				g_free(utf);
				utf = g_malloc(strlen(result)*2+1);
				conv_localetodisp(utf, 
					strlen(result)*2+1, result);
			}
			if (g_utf8_validate(utf, -1, NULL)) {
				INSERT(utf);
			}
			g_free(utf);
		}
	}
#undef STR_SIZE			
#undef RLEFT			
}		

static void quote_fmt_show_first_name(const MsgInfo *msginfo)
{
	guchar *p;
	gchar *str;

	if (!msginfo->fromname)
		return;	
	
	p = (guchar*)strchr(msginfo->fromname, ',');
	if (p != NULL) {
		/* fromname is like "Duck, Donald" */
		p++;
		while (*p && isspace(*p)) p++;
		str = alloca(strlen((char *)p) + 1);
		if (str != NULL) {
			strcpy(str, (char *)p);
			INSERT(str);
		}
	} else {
		/* fromname is like "Donald Duck" */
		str = alloca(strlen(msginfo->fromname) + 1);
		if (str != NULL) {
			strcpy(str, msginfo->fromname);
			p = (guchar *)str;
			while (*p && !isspace(*p)) p++;
			*p = '\0';
			INSERT(str);
		}
	}
}

static void quote_fmt_show_last_name(const MsgInfo *msginfo)
{
	gchar *p;
	gchar *str;

	/* This probably won't work together very well with Middle
           names and the like - thth */
	if (!msginfo->fromname) 
		return;

	str = alloca(strlen(msginfo->fromname) + 1);
	if (str != NULL) {
		strcpy(str, msginfo->fromname);
		p = strchr(str, ',');
		if (p != NULL) {
			/* fromname is like "Duck, Donald" */
			*p = '\0';
			INSERT(str);
		} else {
			/* fromname is like "Donald Duck" */
			p = str;
			while (*p && !isspace(*p)) p++;
			if (*p) {
			    /* We found a space. Get first 
			     none-space char and insert
			     rest of string from there. */
			    while (*p && isspace(*p)) p++;
			    if (*p) {
				INSERT(p);
			    } else {
				/* If there is no none-space 
				 char, just insert whole 
				 fromname. */
				INSERT(str);
			    }
			} else {
			    /* If there is no space, just 
			     insert whole fromname. */
			    INSERT(str);
			}
		}
	}
}

static void quote_fmt_show_sender_initial(const MsgInfo *msginfo)
{
#define MAX_SENDER_INITIAL 20
	gchar tmp[MAX_SENDER_INITIAL];
	guchar *p;
	gchar *cur;
	gint len = 0;

	if (!msginfo->fromname) 
		return;

	p = (guchar *)msginfo->fromname;
	cur = tmp;
	while (*p) {
		if (*p && g_utf8_validate((gchar *)p, 1, NULL)) {
			*cur = toupper(*p);
				cur++;
			len++;
			if (len >= MAX_SENDER_INITIAL - 1)
				break;
		} else
			break;
		while (*p && !isseparator(*p)) p++;
		while (*p && isseparator(*p)) p++;
	}
	*cur = '\0';
	INSERT(tmp);
}

static void quote_fmt_show_msg(MsgInfo *msginfo, const gchar *body,
			       gboolean quoted, gboolean signature,
			       const gchar *quote_str)
{
	gchar buf[BUFFSIZE];
	FILE *fp;

	if (!(msginfo->folder || body))
		return;

	if (body)
		fp = str_open_as_stream(body);
	else {
		if (MSG_IS_ENCRYPTED(msginfo->flags))
			fp = procmime_get_first_encrypted_text_content(msginfo);
		else
			fp = procmime_get_first_text_content(msginfo);
	}

	if (fp == NULL)
		g_warning("Can't get text part\n");
	else {
		while (fgets(buf, sizeof(buf), fp) != NULL) {
			strcrchomp(buf);
			
			if (!signature && strncmp(buf, "-- \n", 4) == 0)
				break;
		
			if (quoted && quote_str)
				INSERT(quote_str);
			
			INSERT(buf);
		}
		fclose(fp);
	}
}

static void quote_fmt_insert_file(const gchar *filename)
{
	FILE *file;
	char buffer[256];
	
	if ((file = g_fopen(filename, "rb")) != NULL) {
		while (fgets(buffer, sizeof(buffer), file)) {
			INSERT(buffer);
		}
		fclose(file);
	}

}

static void quote_fmt_insert_program_output(const gchar *progname)
{
	FILE *file;
	char buffer[256];

	if ((file = popen(progname, "r")) != NULL) {
		while (fgets(buffer, sizeof(buffer), file)) {
			INSERT(buffer);
		}
		pclose(file);
	}
}

static void quote_fmt_insert_user_input(const gchar *varname)
{
	gchar *buf = NULL;
	gchar *text = NULL;
	
	if (dry_run) 
		return;

	if ((text = g_hash_table_lookup(var_table, varname)) == NULL) {
		buf = g_strdup_printf(_("Enter text to replace '%s'"), varname);
		text = input_dialog(_("Enter variable"), buf, "");
		g_free(buf);
		if (!text)
			return;
		g_hash_table_insert(var_table, g_strdup(varname), g_strdup(text));
	} else {
		/* don't free the one in hashtable at the end */
		text = g_strdup(text);
	}

	if (!text)
		return;
	INSERT(text);
	g_free(text);
}

static void quote_fmt_attach_file(const gchar *filename)
{
	attachments = g_list_append(attachments, g_strdup(filename));
}

static gchar *quote_fmt_complete_address(const gchar *addr)
{
	gint count;
	gchar *res, *tmp, *email_addr;
	gchar **split;

	debug_print("quote_fmt_complete_address: %s\n", addr);
	if (addr == NULL)
		return NULL;

	/* if addr is a list of message, try the 1st element only */
	split = g_strsplit(addr, ",", -1);
	if (!split || !split[0] || *split[0] == '\0') {
		g_strfreev(split);
		return NULL;
	}

	Xstrdup_a(email_addr, split[0], return NULL);
	extract_address(email_addr);
	if (!*email_addr) {
		g_strfreev(split);
		return NULL;
	}

	res = NULL;
	start_address_completion(NULL);
	if (1 < (count = complete_address(email_addr))) {
		tmp = get_complete_address(1);
		res = procheader_get_fromname(tmp);
		g_free(tmp);
	}
	end_address_completion();
	g_strfreev(split);

	debug_print("quote_fmt_complete_address: matched %s\n", res);
	return res;
}



/* Line 268 of yacc.c  */
#line 623 "quote_fmt_parse.c"

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
     SHOW_NEWSGROUPS = 258,
     SHOW_DATE = 259,
     SHOW_FROM = 260,
     SHOW_FULLNAME = 261,
     SHOW_FIRST_NAME = 262,
     SHOW_LAST_NAME = 263,
     SHOW_SENDER_INITIAL = 264,
     SHOW_SUBJECT = 265,
     SHOW_TO = 266,
     SHOW_MESSAGEID = 267,
     SHOW_PERCENT = 268,
     SHOW_CC = 269,
     SHOW_REFERENCES = 270,
     SHOW_MESSAGE = 271,
     SHOW_QUOTED_MESSAGE = 272,
     SHOW_BACKSLASH = 273,
     SHOW_TAB = 274,
     SHOW_MAIL_ADDRESS = 275,
     SHOW_QUOTED_MESSAGE_NO_SIGNATURE = 276,
     SHOW_MESSAGE_NO_SIGNATURE = 277,
     SHOW_EOL = 278,
     SHOW_QUESTION_MARK = 279,
     SHOW_EXCLAMATION_MARK = 280,
     SHOW_PIPE = 281,
     SHOW_OPARENT = 282,
     SHOW_CPARENT = 283,
     SHOW_ACCOUNT_FULL_NAME = 284,
     SHOW_ACCOUNT_MAIL_ADDRESS = 285,
     SHOW_ACCOUNT_NAME = 286,
     SHOW_ACCOUNT_ORGANIZATION = 287,
     SHOW_ACCOUNT_DICT = 288,
     SHOW_ACCOUNT_SIG = 289,
     SHOW_ACCOUNT_SIGPATH = 290,
     SHOW_DICT = 291,
     SHOW_TAGS = 292,
     SHOW_ADDRESSBOOK_COMPLETION_FOR_CC = 293,
     SHOW_ADDRESSBOOK_COMPLETION_FOR_FROM = 294,
     SHOW_ADDRESSBOOK_COMPLETION_FOR_TO = 295,
     QUERY_DATE = 296,
     QUERY_FROM = 297,
     QUERY_FULLNAME = 298,
     QUERY_SUBJECT = 299,
     QUERY_TO = 300,
     QUERY_NEWSGROUPS = 301,
     QUERY_MESSAGEID = 302,
     QUERY_CC = 303,
     QUERY_REFERENCES = 304,
     QUERY_ACCOUNT_FULL_NAME = 305,
     QUERY_ACCOUNT_ORGANIZATION = 306,
     QUERY_ACCOUNT_DICT = 307,
     QUERY_ACCOUNT_SIG = 308,
     QUERY_ACCOUNT_SIGPATH = 309,
     QUERY_DICT = 310,
     QUERY_CC_FOUND_IN_ADDRESSBOOK = 311,
     QUERY_FROM_FOUND_IN_ADDRESSBOOK = 312,
     QUERY_TO_FOUND_IN_ADDRESSBOOK = 313,
     QUERY_NOT_DATE = 314,
     QUERY_NOT_FROM = 315,
     QUERY_NOT_FULLNAME = 316,
     QUERY_NOT_SUBJECT = 317,
     QUERY_NOT_TO = 318,
     QUERY_NOT_NEWSGROUPS = 319,
     QUERY_NOT_MESSAGEID = 320,
     QUERY_NOT_CC = 321,
     QUERY_NOT_REFERENCES = 322,
     QUERY_NOT_ACCOUNT_FULL_NAME = 323,
     QUERY_NOT_ACCOUNT_ORGANIZATION = 324,
     QUERY_NOT_ACCOUNT_DICT = 325,
     QUERY_NOT_ACCOUNT_SIG = 326,
     QUERY_NOT_ACCOUNT_SIGPATH = 327,
     QUERY_NOT_DICT = 328,
     QUERY_NOT_CC_FOUND_IN_ADDRESSBOOK = 329,
     QUERY_NOT_FROM_FOUND_IN_ADDRESSBOOK = 330,
     QUERY_NOT_TO_FOUND_IN_ADDRESSBOOK = 331,
     INSERT_FILE = 332,
     INSERT_PROGRAMOUTPUT = 333,
     INSERT_USERINPUT = 334,
     ATTACH_FILE = 335,
     OPARENT = 336,
     CPARENT = 337,
     CHARACTER = 338,
     SHOW_DATE_EXPR = 339,
     SET_CURSOR_POS = 340
   };
#endif
/* Tokens.  */
#define SHOW_NEWSGROUPS 258
#define SHOW_DATE 259
#define SHOW_FROM 260
#define SHOW_FULLNAME 261
#define SHOW_FIRST_NAME 262
#define SHOW_LAST_NAME 263
#define SHOW_SENDER_INITIAL 264
#define SHOW_SUBJECT 265
#define SHOW_TO 266
#define SHOW_MESSAGEID 267
#define SHOW_PERCENT 268
#define SHOW_CC 269
#define SHOW_REFERENCES 270
#define SHOW_MESSAGE 271
#define SHOW_QUOTED_MESSAGE 272
#define SHOW_BACKSLASH 273
#define SHOW_TAB 274
#define SHOW_MAIL_ADDRESS 275
#define SHOW_QUOTED_MESSAGE_NO_SIGNATURE 276
#define SHOW_MESSAGE_NO_SIGNATURE 277
#define SHOW_EOL 278
#define SHOW_QUESTION_MARK 279
#define SHOW_EXCLAMATION_MARK 280
#define SHOW_PIPE 281
#define SHOW_OPARENT 282
#define SHOW_CPARENT 283
#define SHOW_ACCOUNT_FULL_NAME 284
#define SHOW_ACCOUNT_MAIL_ADDRESS 285
#define SHOW_ACCOUNT_NAME 286
#define SHOW_ACCOUNT_ORGANIZATION 287
#define SHOW_ACCOUNT_DICT 288
#define SHOW_ACCOUNT_SIG 289
#define SHOW_ACCOUNT_SIGPATH 290
#define SHOW_DICT 291
#define SHOW_TAGS 292
#define SHOW_ADDRESSBOOK_COMPLETION_FOR_CC 293
#define SHOW_ADDRESSBOOK_COMPLETION_FOR_FROM 294
#define SHOW_ADDRESSBOOK_COMPLETION_FOR_TO 295
#define QUERY_DATE 296
#define QUERY_FROM 297
#define QUERY_FULLNAME 298
#define QUERY_SUBJECT 299
#define QUERY_TO 300
#define QUERY_NEWSGROUPS 301
#define QUERY_MESSAGEID 302
#define QUERY_CC 303
#define QUERY_REFERENCES 304
#define QUERY_ACCOUNT_FULL_NAME 305
#define QUERY_ACCOUNT_ORGANIZATION 306
#define QUERY_ACCOUNT_DICT 307
#define QUERY_ACCOUNT_SIG 308
#define QUERY_ACCOUNT_SIGPATH 309
#define QUERY_DICT 310
#define QUERY_CC_FOUND_IN_ADDRESSBOOK 311
#define QUERY_FROM_FOUND_IN_ADDRESSBOOK 312
#define QUERY_TO_FOUND_IN_ADDRESSBOOK 313
#define QUERY_NOT_DATE 314
#define QUERY_NOT_FROM 315
#define QUERY_NOT_FULLNAME 316
#define QUERY_NOT_SUBJECT 317
#define QUERY_NOT_TO 318
#define QUERY_NOT_NEWSGROUPS 319
#define QUERY_NOT_MESSAGEID 320
#define QUERY_NOT_CC 321
#define QUERY_NOT_REFERENCES 322
#define QUERY_NOT_ACCOUNT_FULL_NAME 323
#define QUERY_NOT_ACCOUNT_ORGANIZATION 324
#define QUERY_NOT_ACCOUNT_DICT 325
#define QUERY_NOT_ACCOUNT_SIG 326
#define QUERY_NOT_ACCOUNT_SIGPATH 327
#define QUERY_NOT_DICT 328
#define QUERY_NOT_CC_FOUND_IN_ADDRESSBOOK 329
#define QUERY_NOT_FROM_FOUND_IN_ADDRESSBOOK 330
#define QUERY_NOT_TO_FOUND_IN_ADDRESSBOOK 331
#define INSERT_FILE 332
#define INSERT_PROGRAMOUTPUT 333
#define INSERT_USERINPUT 334
#define ATTACH_FILE 335
#define OPARENT 336
#define CPARENT 337
#define CHARACTER 338
#define SHOW_DATE_EXPR 339
#define SET_CURSOR_POS 340




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 293 of yacc.c  */
#line 571 "quote_fmt_parse.y"

	char chr;
	char str[256];



/* Line 293 of yacc.c  */
#line 836 "quote_fmt_parse.c"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */


/* Line 343 of yacc.c  */
#line 848 "quote_fmt_parse.c"

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
# if defined YYENABLE_NLS && YYENABLE_NLS
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
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
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
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
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

# define YYCOPY_NEEDED 1

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

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
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
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  133
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   256

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  86
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  54
/* YYNRULES -- Number of rules.  */
#define YYNRULES  137
/* YYNRULES -- Number of states.  */
#define YYNSTATES  262

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   340

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
      85
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     5,     7,    10,    12,    15,    17,    19,
      21,    23,    25,    27,    29,    31,    33,    35,    38,    40,
      45,    47,    49,    51,    53,    55,    57,    59,    61,    63,
      65,    67,    69,    71,    73,    75,    77,    79,    81,    83,
      85,    87,    89,    91,    93,    95,    97,    99,   101,   103,
     105,   107,   109,   111,   113,   115,   117,   119,   121,   122,
     128,   129,   135,   136,   142,   143,   149,   150,   156,   157,
     163,   164,   170,   171,   177,   178,   184,   185,   191,   192,
     198,   199,   205,   206,   212,   213,   219,   220,   226,   227,
     233,   234,   240,   241,   247,   248,   254,   255,   261,   262,
     268,   269,   275,   276,   282,   283,   289,   290,   296,   297,
     303,   304,   310,   311,   317,   318,   324,   325,   331,   332,
     338,   339,   345,   346,   352,   353,   359,   360,   366,   367,
     373,   374,   380,   381,   387,   388,   394,   395
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
      87,     0,    -1,    89,    -1,    90,    -1,    91,    89,    -1,
      91,    -1,    92,    90,    -1,    92,    -1,    92,    -1,    96,
      -1,   115,    -1,   134,    -1,   138,    -1,    95,    -1,    93,
      -1,    83,    -1,    83,    -1,    94,    83,    -1,     3,    -1,
      84,    81,    94,    82,    -1,     4,    -1,     5,    -1,    20,
      -1,     6,    -1,     7,    -1,     8,    -1,     9,    -1,    10,
      -1,    11,    -1,    12,    -1,    13,    -1,    14,    -1,    15,
      -1,    16,    -1,    17,    -1,    22,    -1,    21,    -1,    29,
      -1,    30,    -1,    31,    -1,    32,    -1,    34,    -1,    35,
      -1,    33,    -1,    36,    -1,    37,    -1,    18,    -1,    19,
      -1,    23,    -1,    24,    -1,    25,    -1,    26,    -1,    27,
      -1,    28,    -1,    85,    -1,    38,    -1,    39,    -1,    40,
      -1,    -1,    41,    97,    81,    87,    82,    -1,    -1,    42,
      98,    81,    87,    82,    -1,    -1,    43,    99,    81,    87,
      82,    -1,    -1,    44,   100,    81,    87,    82,    -1,    -1,
      45,   101,    81,    87,    82,    -1,    -1,    46,   102,    81,
      87,    82,    -1,    -1,    47,   103,    81,    87,    82,    -1,
      -1,    48,   104,    81,    87,    82,    -1,    -1,    49,   105,
      81,    87,    82,    -1,    -1,    50,   106,    81,    87,    82,
      -1,    -1,    51,   107,    81,    87,    82,    -1,    -1,    53,
     108,    81,    87,    82,    -1,    -1,    54,   109,    81,    87,
      82,    -1,    -1,    52,   110,    81,    87,    82,    -1,    -1,
      55,   111,    81,    87,    82,    -1,    -1,    56,   112,    81,
      87,    82,    -1,    -1,    57,   113,    81,    87,    82,    -1,
      -1,    58,   114,    81,    87,    82,    -1,    -1,    59,   116,
      81,    87,    82,    -1,    -1,    60,   117,    81,    87,    82,
      -1,    -1,    61,   118,    81,    87,    82,    -1,    -1,    62,
     119,    81,    87,    82,    -1,    -1,    63,   120,    81,    87,
      82,    -1,    -1,    64,   121,    81,    87,    82,    -1,    -1,
      65,   122,    81,    87,    82,    -1,    -1,    66,   123,    81,
      87,    82,    -1,    -1,    67,   124,    81,    87,    82,    -1,
      -1,    68,   125,    81,    87,    82,    -1,    -1,    69,   126,
      81,    87,    82,    -1,    -1,    71,   127,    81,    87,    82,
      -1,    -1,    72,   128,    81,    87,    82,    -1,    -1,    70,
     129,    81,    87,    82,    -1,    -1,    73,   130,    81,    87,
      82,    -1,    -1,    74,   131,    81,    87,    82,    -1,    -1,
      75,   132,    81,    87,    82,    -1,    -1,    76,   133,    81,
      87,    82,    -1,    -1,    77,   135,    81,    88,    82,    -1,
      -1,    78,   136,    81,    88,    82,    -1,    -1,    79,   137,
      81,    88,    82,    -1,    -1,    80,   139,    81,    88,    82,
      -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   627,   627,   630,   633,   634,   637,   638,   641,   642,
     643,   644,   645,   648,   649,   655,   659,   664,   678,   683,
     687,   692,   697,   706,   711,   715,   719,   723,   728,   733,
     738,   742,   747,   756,   760,   764,   768,   772,   777,   782,
     787,   792,   798,   803,   813,   819,   827,   831,   835,   839,
     843,   847,   851,   855,   859,   866,   874,   882,   893,   892,
     901,   900,   909,   908,   917,   916,   925,   924,   933,   932,
     941,   940,   949,   948,   957,   956,   972,   971,   980,   979,
     988,   987,   998,   997,  1007,  1006,  1020,  1019,  1032,  1031,
    1042,  1041,  1052,  1051,  1064,  1063,  1072,  1071,  1080,  1079,
    1088,  1087,  1096,  1095,  1104,  1103,  1112,  1111,  1120,  1119,
    1128,  1127,  1143,  1142,  1151,  1150,  1159,  1158,  1169,  1168,
    1178,  1177,  1191,  1190,  1203,  1202,  1213,  1212,  1223,  1222,
    1235,  1234,  1247,  1246,  1259,  1258,  1273,  1272
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "SHOW_NEWSGROUPS", "SHOW_DATE",
  "SHOW_FROM", "SHOW_FULLNAME", "SHOW_FIRST_NAME", "SHOW_LAST_NAME",
  "SHOW_SENDER_INITIAL", "SHOW_SUBJECT", "SHOW_TO", "SHOW_MESSAGEID",
  "SHOW_PERCENT", "SHOW_CC", "SHOW_REFERENCES", "SHOW_MESSAGE",
  "SHOW_QUOTED_MESSAGE", "SHOW_BACKSLASH", "SHOW_TAB", "SHOW_MAIL_ADDRESS",
  "SHOW_QUOTED_MESSAGE_NO_SIGNATURE", "SHOW_MESSAGE_NO_SIGNATURE",
  "SHOW_EOL", "SHOW_QUESTION_MARK", "SHOW_EXCLAMATION_MARK", "SHOW_PIPE",
  "SHOW_OPARENT", "SHOW_CPARENT", "SHOW_ACCOUNT_FULL_NAME",
  "SHOW_ACCOUNT_MAIL_ADDRESS", "SHOW_ACCOUNT_NAME",
  "SHOW_ACCOUNT_ORGANIZATION", "SHOW_ACCOUNT_DICT", "SHOW_ACCOUNT_SIG",
  "SHOW_ACCOUNT_SIGPATH", "SHOW_DICT", "SHOW_TAGS",
  "SHOW_ADDRESSBOOK_COMPLETION_FOR_CC",
  "SHOW_ADDRESSBOOK_COMPLETION_FOR_FROM",
  "SHOW_ADDRESSBOOK_COMPLETION_FOR_TO", "QUERY_DATE", "QUERY_FROM",
  "QUERY_FULLNAME", "QUERY_SUBJECT", "QUERY_TO", "QUERY_NEWSGROUPS",
  "QUERY_MESSAGEID", "QUERY_CC", "QUERY_REFERENCES",
  "QUERY_ACCOUNT_FULL_NAME", "QUERY_ACCOUNT_ORGANIZATION",
  "QUERY_ACCOUNT_DICT", "QUERY_ACCOUNT_SIG", "QUERY_ACCOUNT_SIGPATH",
  "QUERY_DICT", "QUERY_CC_FOUND_IN_ADDRESSBOOK",
  "QUERY_FROM_FOUND_IN_ADDRESSBOOK", "QUERY_TO_FOUND_IN_ADDRESSBOOK",
  "QUERY_NOT_DATE", "QUERY_NOT_FROM", "QUERY_NOT_FULLNAME",
  "QUERY_NOT_SUBJECT", "QUERY_NOT_TO", "QUERY_NOT_NEWSGROUPS",
  "QUERY_NOT_MESSAGEID", "QUERY_NOT_CC", "QUERY_NOT_REFERENCES",
  "QUERY_NOT_ACCOUNT_FULL_NAME", "QUERY_NOT_ACCOUNT_ORGANIZATION",
  "QUERY_NOT_ACCOUNT_DICT", "QUERY_NOT_ACCOUNT_SIG",
  "QUERY_NOT_ACCOUNT_SIGPATH", "QUERY_NOT_DICT",
  "QUERY_NOT_CC_FOUND_IN_ADDRESSBOOK",
  "QUERY_NOT_FROM_FOUND_IN_ADDRESSBOOK",
  "QUERY_NOT_TO_FOUND_IN_ADDRESSBOOK", "INSERT_FILE",
  "INSERT_PROGRAMOUTPUT", "INSERT_USERINPUT", "ATTACH_FILE", "OPARENT",
  "CPARENT", "CHARACTER", "SHOW_DATE_EXPR", "SET_CURSOR_POS", "$accept",
  "quote_fmt", "sub_expr", "character_or_special_or_insert_or_query_list",
  "character_or_special_list", "character_or_special_or_insert_or_query",
  "character_or_special", "character", "string", "special", "query", "$@1",
  "$@2", "$@3", "$@4", "$@5", "$@6", "$@7", "$@8", "$@9", "$@10", "$@11",
  "$@12", "$@13", "$@14", "$@15", "$@16", "$@17", "$@18", "query_not",
  "$@19", "$@20", "$@21", "$@22", "$@23", "$@24", "$@25", "$@26", "$@27",
  "$@28", "$@29", "$@30", "$@31", "$@32", "$@33", "$@34", "$@35", "$@36",
  "insert", "$@37", "$@38", "$@39", "attach", "$@40", 0
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
     335,   336,   337,   338,   339,   340
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    86,    87,    88,    89,    89,    90,    90,    91,    91,
      91,    91,    91,    92,    92,    93,    94,    94,    95,    95,
      95,    95,    95,    95,    95,    95,    95,    95,    95,    95,
      95,    95,    95,    95,    95,    95,    95,    95,    95,    95,
      95,    95,    95,    95,    95,    95,    95,    95,    95,    95,
      95,    95,    95,    95,    95,    95,    95,    95,    97,    96,
      98,    96,    99,    96,   100,    96,   101,    96,   102,    96,
     103,    96,   104,    96,   105,    96,   106,    96,   107,    96,
     108,    96,   109,    96,   110,    96,   111,    96,   112,    96,
     113,    96,   114,    96,   116,   115,   117,   115,   118,   115,
     119,   115,   120,   115,   121,   115,   122,   115,   123,   115,
     124,   115,   125,   115,   126,   115,   127,   115,   128,   115,
     129,   115,   130,   115,   131,   115,   132,   115,   133,   115,
     135,   134,   136,   134,   137,   134,   139,   138
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     1,     2,     1,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     2,     1,     4,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     0,     5,
       0,     5,     0,     5,     0,     5,     0,     5,     0,     5,
       0,     5,     0,     5,     0,     5,     0,     5,     0,     5,
       0,     5,     0,     5,     0,     5,     0,     5,     0,     5,
       0,     5,     0,     5,     0,     5,     0,     5,     0,     5,
       0,     5,     0,     5,     0,     5,     0,     5,     0,     5,
       0,     5,     0,     5,     0,     5,     0,     5,     0,     5,
       0,     5,     0,     5,     0,     5,     0,     5,     0,     5,
       0,     5,     0,     5,     0,     5,     0,     5
};

/* YYDEFACT[STATE-NAME] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,    18,    20,    21,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    46,    47,    22,    36,
      35,    48,    49,    50,    51,    52,    53,    37,    38,    39,
      40,    43,    41,    42,    44,    45,    55,    56,    57,    58,
      60,    62,    64,    66,    68,    70,    72,    74,    76,    78,
      84,    80,    82,    86,    88,    90,    92,    94,    96,    98,
     100,   102,   104,   106,   108,   110,   112,   114,   120,   116,
     118,   122,   124,   126,   128,   130,   132,   134,   136,    15,
       0,    54,     0,     2,     5,     8,    14,    13,     9,    10,
      11,    12,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     1,     4,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    16,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     3,     7,     0,     0,     0,    19,
      17,    59,    61,    63,    65,    67,    69,    71,    73,    75,
      77,    79,    85,    81,    83,    87,    89,    91,    93,    95,
      97,    99,   101,   103,   105,   107,   109,   111,   113,   115,
     121,   117,   119,   123,   125,   127,   129,   131,     6,   133,
     135,   137
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    82,   213,    83,   214,    84,    85,    86,   176,    87,
      88,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,   104,   105,   103,   106,   107,   108,   109,    89,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   122,   123,   121,   124,   125,   126,   127,    90,   128,
     129,   130,    91,   131
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -50
static const yytype_int16 yypact[] =
{
      -3,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,
     -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,
     -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,
     -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,
     -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,
     -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,
     -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,
     -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,
      40,   -50,   162,   -50,    -3,   -50,   -50,   -50,   -50,   -50,
     -50,   -50,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
     127,   128,   129,   -50,   -50,    -3,    -3,    -3,    -3,    -3,
      -3,    -3,    -3,    -3,    -3,    -3,    -3,    -3,    -3,    -3,
      -3,    -3,    -3,    -3,    -3,    -3,    -3,    -3,    -3,    -3,
      -3,    -3,    -3,    -3,    -3,    -3,    -3,    -3,    -3,    -3,
      -3,    80,    80,    80,    80,   -50,    -4,   131,   132,   133,
     134,   135,   136,   137,   138,   139,   140,   141,   142,   143,
     144,   145,   146,   147,   148,   149,   150,   151,   152,   153,
     154,   155,   156,   157,   158,   159,   160,   161,   163,   164,
     165,   166,   167,   168,   -50,    80,   169,   170,   171,   -50,
     -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,
     -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,
     -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,
     -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,
     -50,   -50
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -50,    -9,    -5,   172,    29,   -50,   -49,   -50,   -50,   -50,
     -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,
     -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,
     -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,
     -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,   -50,
     -50,   -50,   -50,   -50
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_uint16 yytable[] =
{
       1,     2,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,   219,   220,
      79,    80,    81,     1,     2,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,   132,   215,   215,   215,   215,   177,   178,   179,   180,
     181,   182,   183,   184,   185,   186,   187,   188,   189,   190,
     191,   192,   193,   194,   195,   196,   197,   198,   199,   200,
     201,   202,   203,   204,   205,   206,   207,   208,   209,   210,
     211,   212,   133,    79,    80,    81,   215,   216,   217,   218,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147,   148,   149,   150,   151,   152,   153,   154,
     155,   156,   157,   158,   159,   160,   161,   162,   163,   164,
     165,   166,   167,   168,   169,   170,   171,   172,   173,   174,
       0,     0,   175,   221,   222,   223,   224,   225,   226,   227,
     228,   229,   230,   231,   232,   233,   234,   235,   236,   237,
     238,   239,   240,   241,   242,   243,   244,   245,   246,   247,
     248,   249,   250,   251,   258,   252,   253,   254,   255,   256,
     257,   259,   260,   261,     0,     0,   134
};

#define yypact_value_is_default(yystate) \
  ((yystate) == (-50))

#define yytable_value_is_error(yytable_value) \
  YYID (0)

static const yytype_int16 yycheck[] =
{
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    82,    83,
      83,    84,    85,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    81,   171,   172,   173,   174,   135,   136,   137,   138,
     139,   140,   141,   142,   143,   144,   145,   146,   147,   148,
     149,   150,   151,   152,   153,   154,   155,   156,   157,   158,
     159,   160,   161,   162,   163,   164,   165,   166,   167,   168,
     169,   170,     0,    83,    84,    85,   215,   172,   173,   174,
      81,    81,    81,    81,    81,    81,    81,    81,    81,    81,
      81,    81,    81,    81,    81,    81,    81,    81,    81,    81,
      81,    81,    81,    81,    81,    81,    81,    81,    81,    81,
      81,    81,    81,    81,    81,    81,    81,    81,    81,    81,
      -1,    -1,    83,    82,    82,    82,    82,    82,    82,    82,
      82,    82,    82,    82,    82,    82,    82,    82,    82,    82,
      82,    82,    82,    82,    82,    82,    82,    82,    82,    82,
      82,    82,    82,    82,   215,    82,    82,    82,    82,    82,
      82,    82,    82,    82,    -1,    -1,    84
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    83,
      84,    85,    87,    89,    91,    92,    93,    95,    96,   115,
     134,   138,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   110,   108,   109,   111,   112,   113,   114,
     116,   117,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   129,   127,   128,   130,   131,   132,   133,   135,   136,
     137,   139,    81,     0,    89,    81,    81,    81,    81,    81,
      81,    81,    81,    81,    81,    81,    81,    81,    81,    81,
      81,    81,    81,    81,    81,    81,    81,    81,    81,    81,
      81,    81,    81,    81,    81,    81,    81,    81,    81,    81,
      81,    81,    81,    81,    81,    83,    94,    87,    87,    87,
      87,    87,    87,    87,    87,    87,    87,    87,    87,    87,
      87,    87,    87,    87,    87,    87,    87,    87,    87,    87,
      87,    87,    87,    87,    87,    87,    87,    87,    87,    87,
      87,    87,    87,    88,    90,    92,    88,    88,    88,    82,
      83,    82,    82,    82,    82,    82,    82,    82,    82,    82,
      82,    82,    82,    82,    82,    82,    82,    82,    82,    82,
      82,    82,    82,    82,    82,    82,    82,    82,    82,    82,
      82,    82,    82,    82,    82,    82,    82,    82,    90,    82,
      82,    82
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
   Once GCC version 2 has supplanted version 1, this can go.  However,
   YYFAIL appears to be in use.  Nevertheless, it is formally deprecated
   in Bison 2.4.2's NEWS entry, where a plan to phase it out is
   discussed.  */

#define YYFAIL		goto yyerrlab
#if defined YYFAIL
  /* This is here to suppress warnings from the GCC cpp's
     -Wunused-macros.  Normally we don't worry about that warning, but
     some users do, and we want to make it easy for users to remove
     YYFAIL uses, which will produce warnings from Bison 2.5.  */
#endif

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
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


/* This macro is provided for backward compatibility. */

#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
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

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (0, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  YYSIZE_T yysize1;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = 0;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - Assume YYFAIL is not used.  It's too flawed to consider.  See
       <http://lists.gnu.org/archive/html/bison-patches/2009-12/msg00024.html>
       for details.  YYERROR is fine as it does not invoke this
       function.
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                yysize1 = yysize + yytnamerr (0, yytname[yyx]);
                if (! (yysize <= yysize1
                       && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                  return 2;
                yysize = yysize1;
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  yysize1 = yysize + yystrlen (yyformat);
  if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
    return 2;
  yysize = yysize1;

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
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
  if (yypact_value_is_default (yyn))
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
      if (yytable_value_is_error (yyn))
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
        case 14:

/* Line 1806 of yacc.c  */
#line 650 "quote_fmt_parse.y"
    {
		INSERT_CHARACTER((yyvsp[(1) - (1)].chr));
	}
    break;

  case 16:

/* Line 1806 of yacc.c  */
#line 660 "quote_fmt_parse.y"
    {
		(yyval.str)[0] = (yyvsp[(1) - (1)].chr);
		(yyval.str)[1] = '\0';
	}
    break;

  case 17:

/* Line 1806 of yacc.c  */
#line 665 "quote_fmt_parse.y"
    {
		size_t len;
		
		strncpy((yyval.str), (yyvsp[(1) - (2)].str), sizeof((yyval.str)));
		(yyval.str)[sizeof((yyval.str)) - 1] = '\0';
		len = strlen((yyval.str));
		if (len + 1 < sizeof((yyval.str))) {
			(yyval.str)[len + 1] = '\0';
			(yyval.str)[len] = (yyvsp[(2) - (2)].chr);
		}
	}
    break;

  case 18:

/* Line 1806 of yacc.c  */
#line 679 "quote_fmt_parse.y"
    {
		if (msginfo->newsgroups)
			INSERT(msginfo->newsgroups);
	}
    break;

  case 19:

/* Line 1806 of yacc.c  */
#line 684 "quote_fmt_parse.y"
    {
		quote_fmt_show_date(msginfo, (yyvsp[(3) - (4)].str));
	}
    break;

  case 20:

/* Line 1806 of yacc.c  */
#line 688 "quote_fmt_parse.y"
    {
		if (msginfo->date)
			INSERT(msginfo->date);
	}
    break;

  case 21:

/* Line 1806 of yacc.c  */
#line 693 "quote_fmt_parse.y"
    {
		if (msginfo->from)
			INSERT(msginfo->from);
	}
    break;

  case 22:

/* Line 1806 of yacc.c  */
#line 698 "quote_fmt_parse.y"
    {
		if (msginfo->from) {
			gchar *stripped_address = g_strdup(msginfo->from);
			extract_address(stripped_address);
			INSERT(stripped_address);
			g_free(stripped_address);
		}
	}
    break;

  case 23:

/* Line 1806 of yacc.c  */
#line 707 "quote_fmt_parse.y"
    {
		if (msginfo->fromname)
			INSERT(msginfo->fromname);
	}
    break;

  case 24:

/* Line 1806 of yacc.c  */
#line 712 "quote_fmt_parse.y"
    {
		quote_fmt_show_first_name(msginfo);
	}
    break;

  case 25:

/* Line 1806 of yacc.c  */
#line 716 "quote_fmt_parse.y"
    {
		quote_fmt_show_last_name(msginfo);
	}
    break;

  case 26:

/* Line 1806 of yacc.c  */
#line 720 "quote_fmt_parse.y"
    {
		quote_fmt_show_sender_initial(msginfo);
	}
    break;

  case 27:

/* Line 1806 of yacc.c  */
#line 724 "quote_fmt_parse.y"
    {
		if (msginfo->subject)
			INSERT(msginfo->subject);
	}
    break;

  case 28:

/* Line 1806 of yacc.c  */
#line 729 "quote_fmt_parse.y"
    {
		if (msginfo->to)
			INSERT(msginfo->to);
	}
    break;

  case 29:

/* Line 1806 of yacc.c  */
#line 734 "quote_fmt_parse.y"
    {
		if (msginfo->msgid)
			INSERT(msginfo->msgid);
	}
    break;

  case 30:

/* Line 1806 of yacc.c  */
#line 739 "quote_fmt_parse.y"
    {
		INSERT("%");
	}
    break;

  case 31:

/* Line 1806 of yacc.c  */
#line 743 "quote_fmt_parse.y"
    {
		if (msginfo->cc)
			INSERT(msginfo->cc);
	}
    break;

  case 32:

/* Line 1806 of yacc.c  */
#line 748 "quote_fmt_parse.y"
    {
		GSList *item;

		INSERT(msginfo->inreplyto);
		for (item = msginfo->references; item != NULL; item = g_slist_next(item))
			if (item->data)
				INSERT(item->data);
	}
    break;

  case 33:

/* Line 1806 of yacc.c  */
#line 757 "quote_fmt_parse.y"
    {
		quote_fmt_show_msg(msginfo, body, FALSE, TRUE, quote_str);
	}
    break;

  case 34:

/* Line 1806 of yacc.c  */
#line 761 "quote_fmt_parse.y"
    {
		quote_fmt_show_msg(msginfo, body, TRUE, TRUE, quote_str);
	}
    break;

  case 35:

/* Line 1806 of yacc.c  */
#line 765 "quote_fmt_parse.y"
    {
		quote_fmt_show_msg(msginfo, body, FALSE, FALSE, quote_str);
	}
    break;

  case 36:

/* Line 1806 of yacc.c  */
#line 769 "quote_fmt_parse.y"
    {
		quote_fmt_show_msg(msginfo, body, TRUE, FALSE, quote_str);
	}
    break;

  case 37:

/* Line 1806 of yacc.c  */
#line 773 "quote_fmt_parse.y"
    {
		if (account && account->name)
			INSERT(account->name);
	}
    break;

  case 38:

/* Line 1806 of yacc.c  */
#line 778 "quote_fmt_parse.y"
    {
		if (account && account->address)
			INSERT(account->address);
	}
    break;

  case 39:

/* Line 1806 of yacc.c  */
#line 783 "quote_fmt_parse.y"
    {
		if (account && account->account_name)
			INSERT(account->account_name);
	}
    break;

  case 40:

/* Line 1806 of yacc.c  */
#line 788 "quote_fmt_parse.y"
    {
		if (account && account->organization)
			INSERT(account->organization);
	}
    break;

  case 41:

/* Line 1806 of yacc.c  */
#line 793 "quote_fmt_parse.y"
    {
		gchar *str = account_get_signature_str(account);
		INSERT(str);
		g_free(str);
	}
    break;

  case 42:

/* Line 1806 of yacc.c  */
#line 799 "quote_fmt_parse.y"
    {
		if (account && account->sig_path)
			INSERT(account->sig_path);
	}
    break;

  case 43:

/* Line 1806 of yacc.c  */
#line 804 "quote_fmt_parse.y"
    {
#ifdef USE_ENCHANT
		if (account && account->enable_default_dictionary) {
			gchar *dictname = g_path_get_basename(account->default_dictionary);
			INSERT(dictname);
			g_free(dictname);
		}
#endif
	}
    break;

  case 44:

/* Line 1806 of yacc.c  */
#line 814 "quote_fmt_parse.y"
    {
#ifdef USE_ENCHANT
		INSERT(default_dictionary);
#endif
	}
    break;

  case 45:

/* Line 1806 of yacc.c  */
#line 820 "quote_fmt_parse.y"
    {
		gchar *tags = procmsg_msginfo_get_tags_str(msginfo);
		if (tags) {
			INSERT(tags);
		}
		g_free(tags);
	}
    break;

  case 46:

/* Line 1806 of yacc.c  */
#line 828 "quote_fmt_parse.y"
    {
		INSERT("\\");
	}
    break;

  case 47:

/* Line 1806 of yacc.c  */
#line 832 "quote_fmt_parse.y"
    {
		INSERT("\t");
	}
    break;

  case 48:

/* Line 1806 of yacc.c  */
#line 836 "quote_fmt_parse.y"
    {
		INSERT("\n");
	}
    break;

  case 49:

/* Line 1806 of yacc.c  */
#line 840 "quote_fmt_parse.y"
    {
		INSERT("?");
	}
    break;

  case 50:

/* Line 1806 of yacc.c  */
#line 844 "quote_fmt_parse.y"
    {
		INSERT("!");
	}
    break;

  case 51:

/* Line 1806 of yacc.c  */
#line 848 "quote_fmt_parse.y"
    {
		INSERT("|");
	}
    break;

  case 52:

/* Line 1806 of yacc.c  */
#line 852 "quote_fmt_parse.y"
    {
		INSERT("{");
	}
    break;

  case 53:

/* Line 1806 of yacc.c  */
#line 856 "quote_fmt_parse.y"
    {
		INSERT("}");
	}
    break;

  case 54:

/* Line 1806 of yacc.c  */
#line 860 "quote_fmt_parse.y"
    {
		if (current->buffer)
			cursor_pos = g_utf8_strlen(current->buffer, -1);
		else
			cursor_pos = 0;
	}
    break;

  case 55:

/* Line 1806 of yacc.c  */
#line 867 "quote_fmt_parse.y"
    {
		gchar *tmp = quote_fmt_complete_address(msginfo->cc);
		if (tmp) {
			INSERT(tmp);
			g_free(tmp);
		}
	}
    break;

  case 56:

/* Line 1806 of yacc.c  */
#line 875 "quote_fmt_parse.y"
    {
		gchar *tmp = quote_fmt_complete_address(msginfo->from);
		if (tmp) {
			INSERT(tmp);
			g_free(tmp);
		}
	}
    break;

  case 57:

/* Line 1806 of yacc.c  */
#line 883 "quote_fmt_parse.y"
    {
		gchar *tmp = quote_fmt_complete_address(msginfo->to);
		if (tmp) {
			INSERT(tmp);
			g_free(tmp);
		}
	}
    break;

  case 58:

/* Line 1806 of yacc.c  */
#line 893 "quote_fmt_parse.y"
    {
		add_visibility(msginfo->date != NULL);
	}
    break;

  case 59:

/* Line 1806 of yacc.c  */
#line 897 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 60:

/* Line 1806 of yacc.c  */
#line 901 "quote_fmt_parse.y"
    {
		add_visibility(msginfo->from != NULL);
	}
    break;

  case 61:

/* Line 1806 of yacc.c  */
#line 905 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 62:

/* Line 1806 of yacc.c  */
#line 909 "quote_fmt_parse.y"
    {
		add_visibility(msginfo->fromname != NULL);
	}
    break;

  case 63:

/* Line 1806 of yacc.c  */
#line 913 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 64:

/* Line 1806 of yacc.c  */
#line 917 "quote_fmt_parse.y"
    {
		add_visibility(msginfo->subject != NULL);
	}
    break;

  case 65:

/* Line 1806 of yacc.c  */
#line 921 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 66:

/* Line 1806 of yacc.c  */
#line 925 "quote_fmt_parse.y"
    {
		add_visibility(msginfo->to != NULL);
	}
    break;

  case 67:

/* Line 1806 of yacc.c  */
#line 929 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 68:

/* Line 1806 of yacc.c  */
#line 933 "quote_fmt_parse.y"
    {
		add_visibility(msginfo->newsgroups != NULL);
	}
    break;

  case 69:

/* Line 1806 of yacc.c  */
#line 937 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 70:

/* Line 1806 of yacc.c  */
#line 941 "quote_fmt_parse.y"
    {
		add_visibility(msginfo->msgid != NULL);
	}
    break;

  case 71:

/* Line 1806 of yacc.c  */
#line 945 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 72:

/* Line 1806 of yacc.c  */
#line 949 "quote_fmt_parse.y"
    {
		add_visibility(msginfo->cc != NULL);
	}
    break;

  case 73:

/* Line 1806 of yacc.c  */
#line 953 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 74:

/* Line 1806 of yacc.c  */
#line 957 "quote_fmt_parse.y"
    {
		gboolean found;
		GSList *item;

		found = (msginfo->inreplyto != NULL);
		for (item = msginfo->references; found == FALSE && item != NULL; item = g_slist_next(item))
			if (item->data)
				found = TRUE;
		add_visibility(found == TRUE);
	}
    break;

  case 75:

/* Line 1806 of yacc.c  */
#line 968 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 76:

/* Line 1806 of yacc.c  */
#line 972 "quote_fmt_parse.y"
    {
		add_visibility(account != NULL && account->name != NULL);
	}
    break;

  case 77:

/* Line 1806 of yacc.c  */
#line 976 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 78:

/* Line 1806 of yacc.c  */
#line 980 "quote_fmt_parse.y"
    {
		add_visibility(account != NULL && account->organization != NULL);
	}
    break;

  case 79:

/* Line 1806 of yacc.c  */
#line 984 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 80:

/* Line 1806 of yacc.c  */
#line 988 "quote_fmt_parse.y"
    {
		gchar *str = account_get_signature_str(account);
		add_visibility(str != NULL && * str != '\0');
		g_free(str);
	}
    break;

  case 81:

/* Line 1806 of yacc.c  */
#line 994 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 82:

/* Line 1806 of yacc.c  */
#line 998 "quote_fmt_parse.y"
    {
		add_visibility(account != NULL && account->sig_path != NULL
				&& *account->sig_path != '\0');
	}
    break;

  case 83:

/* Line 1806 of yacc.c  */
#line 1003 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 84:

/* Line 1806 of yacc.c  */
#line 1007 "quote_fmt_parse.y"
    {
#ifdef USE_ENCHANT
		add_visibility(account != NULL && account->enable_default_dictionary == TRUE &&
				account->default_dictionary != NULL && *account->default_dictionary != '\0');
#else
		add_visibility(FALSE);
#endif
	}
    break;

  case 85:

/* Line 1806 of yacc.c  */
#line 1016 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 86:

/* Line 1806 of yacc.c  */
#line 1020 "quote_fmt_parse.y"
    {
#ifdef USE_ENCHANT
		add_visibility(*default_dictionary != '\0');
#else
		add_visibility(FALSE);
#endif
	}
    break;

  case 87:

/* Line 1806 of yacc.c  */
#line 1028 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 88:

/* Line 1806 of yacc.c  */
#line 1032 "quote_fmt_parse.y"
    {
		gchar *tmp = quote_fmt_complete_address(msginfo->cc);
		add_visibility(tmp != NULL && *tmp != '\0');
		g_free(tmp);
	}
    break;

  case 89:

/* Line 1806 of yacc.c  */
#line 1038 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 90:

/* Line 1806 of yacc.c  */
#line 1042 "quote_fmt_parse.y"
    {
		gchar *tmp = quote_fmt_complete_address(msginfo->from);
		add_visibility(tmp != NULL && *tmp != '\0');
		g_free(tmp);
	}
    break;

  case 91:

/* Line 1806 of yacc.c  */
#line 1048 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 92:

/* Line 1806 of yacc.c  */
#line 1052 "quote_fmt_parse.y"
    {
		gchar *tmp = quote_fmt_complete_address(msginfo->to);
		add_visibility(tmp != NULL && *tmp != '\0');
		g_free(tmp);
	}
    break;

  case 93:

/* Line 1806 of yacc.c  */
#line 1058 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 94:

/* Line 1806 of yacc.c  */
#line 1064 "quote_fmt_parse.y"
    {
		add_visibility(msginfo->date == NULL);
	}
    break;

  case 95:

/* Line 1806 of yacc.c  */
#line 1068 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 96:

/* Line 1806 of yacc.c  */
#line 1072 "quote_fmt_parse.y"
    {
		add_visibility(msginfo->from == NULL);
	}
    break;

  case 97:

/* Line 1806 of yacc.c  */
#line 1076 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 98:

/* Line 1806 of yacc.c  */
#line 1080 "quote_fmt_parse.y"
    {
		add_visibility(msginfo->fromname == NULL);
	}
    break;

  case 99:

/* Line 1806 of yacc.c  */
#line 1084 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 100:

/* Line 1806 of yacc.c  */
#line 1088 "quote_fmt_parse.y"
    {
		add_visibility(msginfo->subject == NULL);
	}
    break;

  case 101:

/* Line 1806 of yacc.c  */
#line 1092 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 102:

/* Line 1806 of yacc.c  */
#line 1096 "quote_fmt_parse.y"
    {
		add_visibility(msginfo->to == NULL);
	}
    break;

  case 103:

/* Line 1806 of yacc.c  */
#line 1100 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 104:

/* Line 1806 of yacc.c  */
#line 1104 "quote_fmt_parse.y"
    {
		add_visibility(msginfo->newsgroups == NULL);
	}
    break;

  case 105:

/* Line 1806 of yacc.c  */
#line 1108 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 106:

/* Line 1806 of yacc.c  */
#line 1112 "quote_fmt_parse.y"
    {
		add_visibility(msginfo->msgid == NULL);
	}
    break;

  case 107:

/* Line 1806 of yacc.c  */
#line 1116 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 108:

/* Line 1806 of yacc.c  */
#line 1120 "quote_fmt_parse.y"
    {
		add_visibility(msginfo->cc == NULL);
	}
    break;

  case 109:

/* Line 1806 of yacc.c  */
#line 1124 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 110:

/* Line 1806 of yacc.c  */
#line 1128 "quote_fmt_parse.y"
    {
		gboolean found;
		GSList *item;

		found = (msginfo->inreplyto != NULL);
		for (item = msginfo->references; found == FALSE && item != NULL; item = g_slist_next(item))
			if (item->data)
				found = TRUE;
		add_visibility(found == FALSE);
	}
    break;

  case 111:

/* Line 1806 of yacc.c  */
#line 1139 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 112:

/* Line 1806 of yacc.c  */
#line 1143 "quote_fmt_parse.y"
    {
		add_visibility(account == NULL || account->name == NULL);
	}
    break;

  case 113:

/* Line 1806 of yacc.c  */
#line 1147 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 114:

/* Line 1806 of yacc.c  */
#line 1151 "quote_fmt_parse.y"
    {
		add_visibility(account == NULL || account->organization == NULL);
	}
    break;

  case 115:

/* Line 1806 of yacc.c  */
#line 1155 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 116:

/* Line 1806 of yacc.c  */
#line 1159 "quote_fmt_parse.y"
    {
		gchar *str = account_get_signature_str(account);
		add_visibility(str == NULL || *str == '\0');
		g_free(str);
	}
    break;

  case 117:

/* Line 1806 of yacc.c  */
#line 1165 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 118:

/* Line 1806 of yacc.c  */
#line 1169 "quote_fmt_parse.y"
    {
		add_visibility(account == NULL || account->sig_path == NULL
				|| *account->sig_path == '\0');
	}
    break;

  case 119:

/* Line 1806 of yacc.c  */
#line 1174 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 120:

/* Line 1806 of yacc.c  */
#line 1178 "quote_fmt_parse.y"
    {
#ifdef USE_ENCHANT
		add_visibility(account == NULL || account->enable_default_dictionary == FALSE
				|| *account->default_dictionary == '\0');
#else
		add_visibility(FALSE);
#endif
	}
    break;

  case 121:

/* Line 1806 of yacc.c  */
#line 1187 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 122:

/* Line 1806 of yacc.c  */
#line 1191 "quote_fmt_parse.y"
    {
#ifdef USE_ENCHANT
		add_visibility(*default_dictionary == '\0');
#else
		add_visibility(FALSE);
#endif
	}
    break;

  case 123:

/* Line 1806 of yacc.c  */
#line 1199 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 124:

/* Line 1806 of yacc.c  */
#line 1203 "quote_fmt_parse.y"
    {
		gchar *tmp = quote_fmt_complete_address(msginfo->cc);
		add_visibility(tmp == NULL || *tmp == '\0');
		g_free(tmp);
	}
    break;

  case 125:

/* Line 1806 of yacc.c  */
#line 1209 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 126:

/* Line 1806 of yacc.c  */
#line 1213 "quote_fmt_parse.y"
    {
		gchar *tmp = quote_fmt_complete_address(msginfo->from);
		add_visibility(tmp == NULL || *tmp == '\0');
		g_free(tmp);
	}
    break;

  case 127:

/* Line 1806 of yacc.c  */
#line 1219 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 128:

/* Line 1806 of yacc.c  */
#line 1223 "quote_fmt_parse.y"
    {
		gchar *tmp = quote_fmt_complete_address(msginfo->to);
		add_visibility(tmp == NULL || *tmp == '\0');
		g_free(tmp);
	}
    break;

  case 129:

/* Line 1806 of yacc.c  */
#line 1229 "quote_fmt_parse.y"
    {
		remove_visibility();
	}
    break;

  case 130:

/* Line 1806 of yacc.c  */
#line 1235 "quote_fmt_parse.y"
    {
		current = &sub_expr;
		clear_buffer();
	}
    break;

  case 131:

/* Line 1806 of yacc.c  */
#line 1240 "quote_fmt_parse.y"
    {
		current = &main_expr;
		if (!dry_run) {
			quote_fmt_insert_file(sub_expr.buffer);
		}
	}
    break;

  case 132:

/* Line 1806 of yacc.c  */
#line 1247 "quote_fmt_parse.y"
    {
		current = &sub_expr;
		clear_buffer();
	}
    break;

  case 133:

/* Line 1806 of yacc.c  */
#line 1252 "quote_fmt_parse.y"
    {
		current = &main_expr;
		if (!dry_run) {
			quote_fmt_insert_program_output(sub_expr.buffer);
		}
	}
    break;

  case 134:

/* Line 1806 of yacc.c  */
#line 1259 "quote_fmt_parse.y"
    {
		current = &sub_expr;
		clear_buffer();
	}
    break;

  case 135:

/* Line 1806 of yacc.c  */
#line 1264 "quote_fmt_parse.y"
    {
		current = &main_expr;
		if (!dry_run) {
			quote_fmt_insert_user_input(sub_expr.buffer);
		}
	}
    break;

  case 136:

/* Line 1806 of yacc.c  */
#line 1273 "quote_fmt_parse.y"
    {
		current = &sub_expr;
		clear_buffer();
	}
    break;

  case 137:

/* Line 1806 of yacc.c  */
#line 1278 "quote_fmt_parse.y"
    {
		current = &main_expr;
		if (!dry_run) {
			quote_fmt_attach_file(sub_expr.buffer);
		}
	}
    break;



/* Line 1806 of yacc.c  */
#line 3575 "quote_fmt_parse.c"
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
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
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
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
      if (!yypact_value_is_default (yyn))
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
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
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



