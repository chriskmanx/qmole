/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2007 Hiroyuki Yamamoto and the Claws Mail Team
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

%{

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

%}

%union {
	char chr;
	char str[256];
}

/* tokens SHOW */
%token SHOW_NEWSGROUPS
%token SHOW_DATE SHOW_FROM SHOW_FULLNAME SHOW_FIRST_NAME SHOW_LAST_NAME
%token SHOW_SENDER_INITIAL SHOW_SUBJECT SHOW_TO SHOW_MESSAGEID
%token SHOW_PERCENT SHOW_CC SHOW_REFERENCES SHOW_MESSAGE
%token SHOW_QUOTED_MESSAGE SHOW_BACKSLASH SHOW_TAB SHOW_MAIL_ADDRESS
%token SHOW_QUOTED_MESSAGE_NO_SIGNATURE SHOW_MESSAGE_NO_SIGNATURE
%token SHOW_EOL SHOW_QUESTION_MARK SHOW_EXCLAMATION_MARK SHOW_PIPE SHOW_OPARENT SHOW_CPARENT
%token SHOW_ACCOUNT_FULL_NAME SHOW_ACCOUNT_MAIL_ADDRESS SHOW_ACCOUNT_NAME SHOW_ACCOUNT_ORGANIZATION
%token SHOW_ACCOUNT_DICT SHOW_ACCOUNT_SIG SHOW_ACCOUNT_SIGPATH
%token SHOW_DICT SHOW_TAGS
%token SHOW_ADDRESSBOOK_COMPLETION_FOR_CC
%token SHOW_ADDRESSBOOK_COMPLETION_FOR_FROM
%token SHOW_ADDRESSBOOK_COMPLETION_FOR_TO
/* tokens QUERY */
%token QUERY_DATE QUERY_FROM
%token QUERY_FULLNAME QUERY_SUBJECT QUERY_TO QUERY_NEWSGROUPS
%token QUERY_MESSAGEID QUERY_CC QUERY_REFERENCES
%token QUERY_ACCOUNT_FULL_NAME QUERY_ACCOUNT_ORGANIZATION QUERY_ACCOUNT_DICT
%token QUERY_ACCOUNT_SIG QUERY_ACCOUNT_SIGPATH
%token QUERY_DICT
%token QUERY_CC_FOUND_IN_ADDRESSBOOK
%token QUERY_FROM_FOUND_IN_ADDRESSBOOK
%token QUERY_TO_FOUND_IN_ADDRESSBOOK
/* tokens QUERY_NOT */
%token QUERY_NOT_DATE QUERY_NOT_FROM
%token QUERY_NOT_FULLNAME QUERY_NOT_SUBJECT QUERY_NOT_TO QUERY_NOT_NEWSGROUPS
%token QUERY_NOT_MESSAGEID QUERY_NOT_CC QUERY_NOT_REFERENCES
%token QUERY_NOT_ACCOUNT_FULL_NAME QUERY_NOT_ACCOUNT_ORGANIZATION QUERY_NOT_ACCOUNT_DICT
%token QUERY_NOT_ACCOUNT_SIG QUERY_NOT_ACCOUNT_SIGPATH
%token QUERY_NOT_DICT
%token QUERY_NOT_CC_FOUND_IN_ADDRESSBOOK
%token QUERY_NOT_FROM_FOUND_IN_ADDRESSBOOK
%token QUERY_NOT_TO_FOUND_IN_ADDRESSBOOK
/* other tokens */
%token INSERT_FILE INSERT_PROGRAMOUTPUT INSERT_USERINPUT
%token ATTACH_FILE
%token OPARENT CPARENT
%token CHARACTER
%token SHOW_DATE_EXPR
%token SET_CURSOR_POS

%start quote_fmt

%token <chr> CHARACTER
%type <chr> character
%type <str> string

%%

quote_fmt:
	character_or_special_or_insert_or_query_list ;

sub_expr:
	character_or_special_list ;

character_or_special_or_insert_or_query_list:
	character_or_special_or_insert_or_query character_or_special_or_insert_or_query_list
	| character_or_special_or_insert_or_query ;

character_or_special_list:
	character_or_special character_or_special_list
	| character_or_special ;

character_or_special_or_insert_or_query:
	character_or_special
	| query
	| query_not
	| insert
	| attach ;

character_or_special:
	special
	| character
	{
		INSERT_CHARACTER($1);
	};

character:
	CHARACTER
	;

string:
	CHARACTER
	{
		$$[0] = $1;
		$$[1] = '\0';
	}
	| string CHARACTER
	{
		size_t len;
		
		strncpy($$, $1, sizeof($$));
		$$[sizeof($$) - 1] = '\0';
		len = strlen($$);
		if (len + 1 < sizeof($$)) {
			$$[len + 1] = '\0';
			$$[len] = $2;
		}
	};

special:
	SHOW_NEWSGROUPS
	{
		if (msginfo->newsgroups)
			INSERT(msginfo->newsgroups);
	}
	| SHOW_DATE_EXPR OPARENT string CPARENT
	{
		quote_fmt_show_date(msginfo, $3);
	}
	| SHOW_DATE
	{
		if (msginfo->date)
			INSERT(msginfo->date);
	}
	| SHOW_FROM
	{
		if (msginfo->from)
			INSERT(msginfo->from);
	}
	| SHOW_MAIL_ADDRESS
	{
		if (msginfo->from) {
			gchar *stripped_address = g_strdup(msginfo->from);
			extract_address(stripped_address);
			INSERT(stripped_address);
			g_free(stripped_address);
		}
	}
	| SHOW_FULLNAME
	{
		if (msginfo->fromname)
			INSERT(msginfo->fromname);
	}
	| SHOW_FIRST_NAME
	{
		quote_fmt_show_first_name(msginfo);
	}
	| SHOW_LAST_NAME
    {
		quote_fmt_show_last_name(msginfo);
	}
	| SHOW_SENDER_INITIAL
	{
		quote_fmt_show_sender_initial(msginfo);
	}
	| SHOW_SUBJECT
	{
		if (msginfo->subject)
			INSERT(msginfo->subject);
	}
	| SHOW_TO
	{
		if (msginfo->to)
			INSERT(msginfo->to);
	}
	| SHOW_MESSAGEID
	{
		if (msginfo->msgid)
			INSERT(msginfo->msgid);
	}
	| SHOW_PERCENT
	{
		INSERT("%");
	}
	| SHOW_CC
	{
		if (msginfo->cc)
			INSERT(msginfo->cc);
	}
	| SHOW_REFERENCES
	{
		GSList *item;

		INSERT(msginfo->inreplyto);
		for (item = msginfo->references; item != NULL; item = g_slist_next(item))
			if (item->data)
				INSERT(item->data);
	}
	| SHOW_MESSAGE
	{
		quote_fmt_show_msg(msginfo, body, FALSE, TRUE, quote_str);
	}
	| SHOW_QUOTED_MESSAGE
	{
		quote_fmt_show_msg(msginfo, body, TRUE, TRUE, quote_str);
	}
	| SHOW_MESSAGE_NO_SIGNATURE
	{
		quote_fmt_show_msg(msginfo, body, FALSE, FALSE, quote_str);
	}
	| SHOW_QUOTED_MESSAGE_NO_SIGNATURE
	{
		quote_fmt_show_msg(msginfo, body, TRUE, FALSE, quote_str);
	}
	| SHOW_ACCOUNT_FULL_NAME
	{
		if (account && account->name)
			INSERT(account->name);
	}
	| SHOW_ACCOUNT_MAIL_ADDRESS
	{
		if (account && account->address)
			INSERT(account->address);
	}
	| SHOW_ACCOUNT_NAME
	{
		if (account && account->account_name)
			INSERT(account->account_name);
	}
	| SHOW_ACCOUNT_ORGANIZATION
	{
		if (account && account->organization)
			INSERT(account->organization);
	}
	| SHOW_ACCOUNT_SIG
	{
		gchar *str = account_get_signature_str(account);
		INSERT(str);
		g_free(str);
	}
	| SHOW_ACCOUNT_SIGPATH
	{
		if (account && account->sig_path)
			INSERT(account->sig_path);
	}
	| SHOW_ACCOUNT_DICT
	{
#ifdef USE_ENCHANT
		if (account && account->enable_default_dictionary) {
			gchar *dictname = g_path_get_basename(account->default_dictionary);
			INSERT(dictname);
			g_free(dictname);
		}
#endif
	}
	| SHOW_DICT
	{
#ifdef USE_ENCHANT
		INSERT(default_dictionary);
#endif
	}
	| SHOW_TAGS
	{
		gchar *tags = procmsg_msginfo_get_tags_str(msginfo);
		if (tags) {
			INSERT(tags);
		}
		g_free(tags);
	}
	| SHOW_BACKSLASH
	{
		INSERT("\\");
	}
	| SHOW_TAB
	{
		INSERT("\t");
	}
	| SHOW_EOL
	{
		INSERT("\n");
	}
	| SHOW_QUESTION_MARK
	{
		INSERT("?");
	}
	| SHOW_EXCLAMATION_MARK
	{
		INSERT("!");
	}
	| SHOW_PIPE
	{
		INSERT("|");
	}
	| SHOW_OPARENT
	{
		INSERT("{");
	}
	| SHOW_CPARENT
	{
		INSERT("}");
	}
	| SET_CURSOR_POS
	{
		if (current->buffer)
			cursor_pos = g_utf8_strlen(current->buffer, -1);
		else
			cursor_pos = 0;
	}
	| SHOW_ADDRESSBOOK_COMPLETION_FOR_CC
	{
		gchar *tmp = quote_fmt_complete_address(msginfo->cc);
		if (tmp) {
			INSERT(tmp);
			g_free(tmp);
		}
	}
	| SHOW_ADDRESSBOOK_COMPLETION_FOR_FROM
	{
		gchar *tmp = quote_fmt_complete_address(msginfo->from);
		if (tmp) {
			INSERT(tmp);
			g_free(tmp);
		}
	}
	| SHOW_ADDRESSBOOK_COMPLETION_FOR_TO
	{
		gchar *tmp = quote_fmt_complete_address(msginfo->to);
		if (tmp) {
			INSERT(tmp);
			g_free(tmp);
		}
	};

query:
	QUERY_DATE
	{
		add_visibility(msginfo->date != NULL);
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_FROM
	{
		add_visibility(msginfo->from != NULL);
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_FULLNAME
	{
		add_visibility(msginfo->fromname != NULL);
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_SUBJECT
	{
		add_visibility(msginfo->subject != NULL);
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_TO
	{
		add_visibility(msginfo->to != NULL);
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_NEWSGROUPS
	{
		add_visibility(msginfo->newsgroups != NULL);
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_MESSAGEID
	{
		add_visibility(msginfo->msgid != NULL);
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_CC
	{
		add_visibility(msginfo->cc != NULL);
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_REFERENCES
	{
		gboolean found;
		GSList *item;

		found = (msginfo->inreplyto != NULL);
		for (item = msginfo->references; found == FALSE && item != NULL; item = g_slist_next(item))
			if (item->data)
				found = TRUE;
		add_visibility(found == TRUE);
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_ACCOUNT_FULL_NAME
	{
		add_visibility(account != NULL && account->name != NULL);
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_ACCOUNT_ORGANIZATION
	{
		add_visibility(account != NULL && account->organization != NULL);
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_ACCOUNT_SIG
	{
		gchar *str = account_get_signature_str(account);
		add_visibility(str != NULL && * str != '\0');
		g_free(str);
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_ACCOUNT_SIGPATH
	{
		add_visibility(account != NULL && account->sig_path != NULL
				&& *account->sig_path != '\0');
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_ACCOUNT_DICT
	{
#ifdef USE_ENCHANT
		add_visibility(account != NULL && account->enable_default_dictionary == TRUE &&
				account->default_dictionary != NULL && *account->default_dictionary != '\0');
#else
		add_visibility(FALSE);
#endif
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_DICT
	{
#ifdef USE_ENCHANT
		add_visibility(*default_dictionary != '\0');
#else
		add_visibility(FALSE);
#endif
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_CC_FOUND_IN_ADDRESSBOOK
	{
		gchar *tmp = quote_fmt_complete_address(msginfo->cc);
		add_visibility(tmp != NULL && *tmp != '\0');
		g_free(tmp);
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_FROM_FOUND_IN_ADDRESSBOOK
	{
		gchar *tmp = quote_fmt_complete_address(msginfo->from);
		add_visibility(tmp != NULL && *tmp != '\0');
		g_free(tmp);
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_TO_FOUND_IN_ADDRESSBOOK
	{
		gchar *tmp = quote_fmt_complete_address(msginfo->to);
		add_visibility(tmp != NULL && *tmp != '\0');
		g_free(tmp);
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	};

query_not:
	QUERY_NOT_DATE
	{
		add_visibility(msginfo->date == NULL);
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_NOT_FROM
	{
		add_visibility(msginfo->from == NULL);
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_NOT_FULLNAME
	{
		add_visibility(msginfo->fromname == NULL);
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_NOT_SUBJECT
	{
		add_visibility(msginfo->subject == NULL);
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_NOT_TO
	{
		add_visibility(msginfo->to == NULL);
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_NOT_NEWSGROUPS
	{
		add_visibility(msginfo->newsgroups == NULL);
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_NOT_MESSAGEID
	{
		add_visibility(msginfo->msgid == NULL);
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_NOT_CC
	{
		add_visibility(msginfo->cc == NULL);
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_NOT_REFERENCES
	{
		gboolean found;
		GSList *item;

		found = (msginfo->inreplyto != NULL);
		for (item = msginfo->references; found == FALSE && item != NULL; item = g_slist_next(item))
			if (item->data)
				found = TRUE;
		add_visibility(found == FALSE);
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_NOT_ACCOUNT_FULL_NAME
	{
		add_visibility(account == NULL || account->name == NULL);
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_NOT_ACCOUNT_ORGANIZATION
	{
		add_visibility(account == NULL || account->organization == NULL);
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_NOT_ACCOUNT_SIG
	{
		gchar *str = account_get_signature_str(account);
		add_visibility(str == NULL || *str == '\0');
		g_free(str);
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_NOT_ACCOUNT_SIGPATH
	{
		add_visibility(account == NULL || account->sig_path == NULL
				|| *account->sig_path == '\0');
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_NOT_ACCOUNT_DICT
	{
#ifdef USE_ENCHANT
		add_visibility(account == NULL || account->enable_default_dictionary == FALSE
				|| *account->default_dictionary == '\0');
#else
		add_visibility(FALSE);
#endif
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_NOT_DICT
	{
#ifdef USE_ENCHANT
		add_visibility(*default_dictionary == '\0');
#else
		add_visibility(FALSE);
#endif
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_NOT_CC_FOUND_IN_ADDRESSBOOK
	{
		gchar *tmp = quote_fmt_complete_address(msginfo->cc);
		add_visibility(tmp == NULL || *tmp == '\0');
		g_free(tmp);
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_NOT_FROM_FOUND_IN_ADDRESSBOOK
	{
		gchar *tmp = quote_fmt_complete_address(msginfo->from);
		add_visibility(tmp == NULL || *tmp == '\0');
		g_free(tmp);
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	}
	| QUERY_NOT_TO_FOUND_IN_ADDRESSBOOK
	{
		gchar *tmp = quote_fmt_complete_address(msginfo->to);
		add_visibility(tmp == NULL || *tmp == '\0');
		g_free(tmp);
	}
	OPARENT quote_fmt CPARENT
	{
		remove_visibility();
	};

insert:
	INSERT_FILE
	{
		current = &sub_expr;
		clear_buffer();
	}
	OPARENT sub_expr CPARENT
	{
		current = &main_expr;
		if (!dry_run) {
			quote_fmt_insert_file(sub_expr.buffer);
		}
	}
	| INSERT_PROGRAMOUTPUT
	{
		current = &sub_expr;
		clear_buffer();
	}
	OPARENT sub_expr CPARENT
	{
		current = &main_expr;
		if (!dry_run) {
			quote_fmt_insert_program_output(sub_expr.buffer);
		}
	}
	| INSERT_USERINPUT
	{
		current = &sub_expr;
		clear_buffer();
	}
	OPARENT sub_expr CPARENT
	{
		current = &main_expr;
		if (!dry_run) {
			quote_fmt_insert_user_input(sub_expr.buffer);
		}
	};

attach:
	ATTACH_FILE
	{
		current = &sub_expr;
		clear_buffer();
	}
	OPARENT sub_expr CPARENT
	{
		current = &main_expr;
		if (!dry_run) {
			quote_fmt_attach_file(sub_expr.buffer);
		}
	};
