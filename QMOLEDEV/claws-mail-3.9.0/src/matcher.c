/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2002-2012 by the Claws Mail Team and Hiroyuki Yamamoto
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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#include "claws-features.h"
#endif

#include <glib.h>
#include <glib/gi18n.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>

#ifdef USE_PTHREAD
#include <pthread.h>
#endif

#include "defs.h"
#include "utils.h"
#include "procheader.h"
#include "matcher.h"
#include "matcher_parser.h"
#include "prefs_gtk.h"
#include "addr_compl.h"
#include "codeconv.h"
#include "quoted-printable.h"
#include "claws.h"
#include <ctype.h>
#include "prefs_common.h"
#include "log.h"
#include "tags.h"
#include "folder_item_prefs.h"
#include "procmsg.h"

/*!
 *\brief	Keyword lookup element
 */
struct _MatchParser {
	gint id;		/*!< keyword id */ 
	gchar *str;		/*!< keyword */
};
typedef struct _MatchParser MatchParser;

/*!
 *\brief	Table with strings and ids used by the lexer and
 *		the parser. New keywords can be added here.
 */
static const MatchParser matchparser_tab[] = {
	/* msginfo flags */
	{MATCHCRITERIA_ALL, "all"},
	{MATCHCRITERIA_UNREAD, "unread"},
	{MATCHCRITERIA_NOT_UNREAD, "~unread"},
	{MATCHCRITERIA_NEW, "new"},
	{MATCHCRITERIA_NOT_NEW, "~new"},
	{MATCHCRITERIA_MARKED, "marked"},
	{MATCHCRITERIA_NOT_MARKED, "~marked"},
	{MATCHCRITERIA_DELETED, "deleted"},
	{MATCHCRITERIA_NOT_DELETED, "~deleted"},
	{MATCHCRITERIA_REPLIED, "replied"},
	{MATCHCRITERIA_NOT_REPLIED, "~replied"},
	{MATCHCRITERIA_FORWARDED, "forwarded"},
	{MATCHCRITERIA_NOT_FORWARDED, "~forwarded"},
	{MATCHCRITERIA_LOCKED, "locked"},
	{MATCHCRITERIA_NOT_LOCKED, "~locked"},
	{MATCHCRITERIA_COLORLABEL, "colorlabel"},
	{MATCHCRITERIA_NOT_COLORLABEL, "~colorlabel"},
	{MATCHCRITERIA_IGNORE_THREAD, "ignore_thread"},
	{MATCHCRITERIA_NOT_IGNORE_THREAD, "~ignore_thread"},
	{MATCHCRITERIA_WATCH_THREAD, "watch_thread"},
	{MATCHCRITERIA_NOT_WATCH_THREAD, "~watch_thread"},
	{MATCHCRITERIA_SPAM, "spam"},
	{MATCHCRITERIA_NOT_SPAM, "~spam"},
	{MATCHCRITERIA_HAS_ATTACHMENT, "has_attachment"},
	{MATCHCRITERIA_HAS_NO_ATTACHMENT, "~has_attachment"},
	{MATCHCRITERIA_SIGNED, "signed"},
	{MATCHCRITERIA_NOT_SIGNED, "~signed"},

	/* msginfo headers */
	{MATCHCRITERIA_SUBJECT, "subject"},
	{MATCHCRITERIA_NOT_SUBJECT, "~subject"},
	{MATCHCRITERIA_FROM, "from"},
	{MATCHCRITERIA_NOT_FROM, "~from"},
	{MATCHCRITERIA_TO, "to"},
	{MATCHCRITERIA_NOT_TO, "~to"},
	{MATCHCRITERIA_CC, "cc"},
	{MATCHCRITERIA_NOT_CC, "~cc"},
	{MATCHCRITERIA_TO_OR_CC, "to_or_cc"},
	{MATCHCRITERIA_NOT_TO_AND_NOT_CC, "~to_or_cc"},
	{MATCHCRITERIA_TAG, "tag"},
	{MATCHCRITERIA_NOT_TAG, "~tag"},
	{MATCHCRITERIA_TAGGED, "tagged"},
	{MATCHCRITERIA_NOT_TAGGED, "~tagged"},
	{MATCHCRITERIA_AGE_GREATER, "age_greater"},
	{MATCHCRITERIA_AGE_LOWER, "age_lower"},
	{MATCHCRITERIA_NEWSGROUPS, "newsgroups"},
	{MATCHCRITERIA_NOT_NEWSGROUPS, "~newsgroups"},
	{MATCHCRITERIA_INREPLYTO, "inreplyto"},
	{MATCHCRITERIA_NOT_INREPLYTO, "~inreplyto"},
	{MATCHCRITERIA_REFERENCES, "references"},
	{MATCHCRITERIA_NOT_REFERENCES, "~references"},
	{MATCHCRITERIA_SCORE_GREATER, "score_greater"},
	{MATCHCRITERIA_SCORE_LOWER, "score_lower"},
	{MATCHCRITERIA_SCORE_EQUAL, "score_equal"},
	{MATCHCRITERIA_PARTIAL, "partial"},
	{MATCHCRITERIA_NOT_PARTIAL, "~partial"},
	{MATCHCRITERIA_FOUND_IN_ADDRESSBOOK, "found_in_addressbook"},
	{MATCHCRITERIA_NOT_FOUND_IN_ADDRESSBOOK, "~found_in_addressbook"},

	{MATCHCRITERIA_SIZE_GREATER, "size_greater"},
	{MATCHCRITERIA_SIZE_SMALLER, "size_smaller"},
	{MATCHCRITERIA_SIZE_EQUAL,   "size_equal"},

	/* content have to be read */
	{MATCHCRITERIA_HEADER, "header"},
	{MATCHCRITERIA_NOT_HEADER, "~header"},
	{MATCHCRITERIA_HEADERS_PART, "headers_part"},
	{MATCHCRITERIA_NOT_HEADERS_PART, "~headers_part"},
	{MATCHCRITERIA_MESSAGE, "message"},
	{MATCHCRITERIA_NOT_MESSAGE, "~message"},
	{MATCHCRITERIA_BODY_PART, "body_part"},
	{MATCHCRITERIA_NOT_BODY_PART, "~body_part"},
	{MATCHCRITERIA_TEST, "test"},
	{MATCHCRITERIA_NOT_TEST, "~test"},

	/* match type */
	{MATCHTYPE_MATCHCASE, "matchcase"},
	{MATCHTYPE_MATCH, "match"},
	{MATCHTYPE_REGEXPCASE, "regexpcase"},
	{MATCHTYPE_REGEXP, "regexp"},

	/* actions */
	{MATCHACTION_SCORE, "score"},    /* for backward compatibility */
	{MATCHACTION_MOVE, "move"},
	{MATCHACTION_COPY, "copy"},
	{MATCHACTION_DELETE, "delete"},
	{MATCHACTION_MARK, "mark"},
	{MATCHACTION_UNMARK, "unmark"},
	{MATCHACTION_LOCK, "lock"},
	{MATCHACTION_UNLOCK, "unlock"},
	{MATCHACTION_MARK_AS_READ, "mark_as_read"},
	{MATCHACTION_MARK_AS_UNREAD, "mark_as_unread"},
	{MATCHACTION_MARK_AS_SPAM, "mark_as_spam"},
	{MATCHACTION_MARK_AS_HAM, "mark_as_ham"},
	{MATCHACTION_FORWARD, "forward"},
	{MATCHACTION_FORWARD_AS_ATTACHMENT, "forward_as_attachment"},
	{MATCHACTION_EXECUTE, "execute"},
	{MATCHACTION_COLOR, "color"},
	{MATCHACTION_REDIRECT, "redirect"},
	{MATCHACTION_CHANGE_SCORE, "change_score"},
	{MATCHACTION_SET_SCORE, "set_score"},
	{MATCHACTION_STOP, "stop"},
	{MATCHACTION_HIDE, "hide"},
	{MATCHACTION_IGNORE, "ignore"},
	{MATCHACTION_WATCH, "watch"},
	{MATCHACTION_ADD_TO_ADDRESSBOOK, "add_to_addressbook"},
	{MATCHACTION_SET_TAG, "set_tag"},
	{MATCHACTION_UNSET_TAG, "unset_tag"},
	{MATCHACTION_CLEAR_TAGS, "clear_tags"},
};

enum {
	MATCH_ANY = 0,
	MATCH_ALL = 1,
	MATCH_ONE = 2
};

enum {
	CONTEXT_SUBJECT,
	CONTEXT_FROM,
	CONTEXT_TO,
	CONTEXT_CC,
	CONTEXT_NEWSGROUPS,
	CONTEXT_IN_REPLY_TO,
	CONTEXT_REFERENCES,
	CONTEXT_HEADER,
	CONTEXT_HEADER_LINE,
	CONTEXT_BODY_LINE,
	CONTEXT_TAG,
	N_CONTEXT_STRS
};

static gchar *context_str[N_CONTEXT_STRS];

void matcher_init(void)
{
	if (context_str[CONTEXT_SUBJECT] != NULL)
		return;

	context_str[CONTEXT_SUBJECT] = g_strdup_printf(_("%s header"), prefs_common_translated_header_name("Subject:"));
	context_str[CONTEXT_FROM] = g_strdup_printf(_("%s header"), prefs_common_translated_header_name("From:"));
	context_str[CONTEXT_TO] = g_strdup_printf(_("%s header"), prefs_common_translated_header_name("To:"));
	context_str[CONTEXT_CC] = g_strdup_printf(_("%s header"), prefs_common_translated_header_name("Cc:"));
	context_str[CONTEXT_NEWSGROUPS] = g_strdup_printf(_("%s header"), prefs_common_translated_header_name("Newsgroups:"));
	context_str[CONTEXT_IN_REPLY_TO] = g_strdup_printf(_("%s header"), prefs_common_translated_header_name("In-Reply-To:"));
	context_str[CONTEXT_REFERENCES] = g_strdup_printf(_("%s header"), prefs_common_translated_header_name("References:"));
	context_str[CONTEXT_HEADER] = g_strdup(_("header"));
	context_str[CONTEXT_HEADER_LINE] = g_strdup(_("header line"));
	context_str[CONTEXT_BODY_LINE] = g_strdup(_("body line"));
	context_str[CONTEXT_TAG]  = g_strdup(_("tag"));
}

void matcher_done(void)
{
	int i;
	for (i = 0; i < N_CONTEXT_STRS; i++) {
		g_free(context_str[i]);
		context_str[i] = NULL;
	}
}

extern gboolean debug_filtering_session;

/*!
 *\brief	Look up table with keywords defined in \sa matchparser_tab
 */
static GHashTable *matchparser_hashtab;

/*!
 *\brief	Translate keyword id to keyword string
 *
 *\param	id Id of keyword
 *
 *\return	const gchar * Keyword
 */
const gchar *get_matchparser_tab_str(gint id)
{
	gint i;

	for (i = 0; i < sizeof matchparser_tab / sizeof matchparser_tab[0]; i++) {
		if (matchparser_tab[i].id == id)
			return matchparser_tab[i].str;
	}
	return NULL;
}

/*!
 *\brief	Create keyword lookup table
 */
static void create_matchparser_hashtab(void)
{
	int i;
	
	if (matchparser_hashtab) return;
	matchparser_hashtab = g_hash_table_new(g_str_hash, g_str_equal);
	for (i = 0; i < sizeof matchparser_tab / sizeof matchparser_tab[0]; i++)
		g_hash_table_insert(matchparser_hashtab,
				    matchparser_tab[i].str,
				    (gpointer) &matchparser_tab[i]);
}

/*!
 *\brief	Return a keyword id from a keyword string
 *
 *\param	str Keyword string
 *
 *\return	gint Keyword id
 */
gint get_matchparser_tab_id(const gchar *str)
{
	MatchParser *res;

	if (NULL != (res = g_hash_table_lookup(matchparser_hashtab, str))) {
		return res->id;
	} else
		return -1;
}

/* **************** data structure allocation **************** */

/*!
 *\brief	Allocate a structure for a filtering / scoring
 *		"condition" (a matcher structure)
 *
 *\param	criteria Criteria ID (MATCHCRITERIA_XXXX)
 *\param	header Header string (if criteria is MATCHCRITERIA_HEADER
			or MATCHCRITERIA_FOUND_IN_ADDRESSBOOK)
 *\param	matchtype Type of action (MATCHTYPE_XXX)
 *\param	expr String value or expression to check
 *\param	value Integer value to check
 *
 *\return	MatcherProp * Pointer to newly allocated structure
 */
MatcherProp *matcherprop_new(gint criteria, const gchar *header,
			      gint matchtype, const gchar *expr,
			      int value)
{
	MatcherProp *prop;

 	prop = g_new0(MatcherProp, 1);
	prop->criteria = criteria;
	prop->header = header != NULL ? g_strdup(header) : NULL;

	prop->expr = expr != NULL ? g_strdup(expr) : NULL;

	prop->matchtype = matchtype;
	prop->preg = NULL;
	prop->value = value;
	prop->error = 0;

	return prop;
}

/*!
 *\brief	Free a matcher structure
 *
 *\param	prop Pointer to matcher structure allocated with
 *		#matcherprop_new
 */
void matcherprop_free(MatcherProp *prop)
{
	g_free(prop->expr);
	g_free(prop->header);
	if (prop->preg != NULL) {
		regfree(prop->preg);
		g_free(prop->preg);
	}
	g_free(prop);
}

/*!
 *\brief	Copy a matcher structure
 *
 *\param	src Matcher structure to copy
 *
 *\return	MatcherProp * Pointer to newly allocated matcher structure
 */
MatcherProp *matcherprop_copy(const MatcherProp *src)
{
	MatcherProp *prop = g_new0(MatcherProp, 1);
	
	prop->criteria = src->criteria;
	prop->header = src->header ? g_strdup(src->header) : NULL;
	prop->expr = src->expr ? g_strdup(src->expr) : NULL;
	prop->matchtype = src->matchtype;
	
	prop->preg = NULL; /* will be re-evaluated */
	prop->value = src->value;
	prop->error = src->error;	
	return prop;		
}

/* ************** match ******************************/

static gboolean match_with_addresses_in_addressbook
	(MatcherProp *prop, GSList *address_list, gint type,
	 gchar* folderpath, gint match)
{
	GSList *walk = NULL;
	gboolean found = FALSE;
	gchar *path = NULL;

	cm_return_val_if_fail(address_list != NULL, FALSE);

	debug_print("match_with_addresses_in_addressbook(%d, %s)\n",
				g_slist_length(address_list), folderpath?folderpath:"(null)");

	if (folderpath == NULL ||
		strcasecmp(folderpath, "Any") == 0 ||
		*folderpath == '\0')
		path = NULL;
	else
		path = folderpath;
	
	start_address_completion(path);

	for (walk = address_list; walk != NULL; walk = walk->next) {
		/* exact matching of email address */
		guint num_addr = complete_address(walk->data);
		found = FALSE;
		if (num_addr > 1) {
			/* skip first item (this is the search string itself) */
			int i = 1;
			for (; i < num_addr && !found; i++) {
				gchar *addr = get_complete_address(i);
				extract_address(addr);
				if (strcasecmp(addr, walk->data) == 0) {
					found = TRUE;

					/* debug output */
					if (debug_filtering_session
							&& prefs_common.filtering_debug_level >= FILTERING_DEBUG_LEVEL_HIGH) {
						log_print(LOG_DEBUG_FILTERING,
								"address [ %s ] matches\n",
								(gchar *)walk->data);
					}
				}
				g_free(addr);
			}
		}
		/* debug output */
		if (debug_filtering_session
				&& prefs_common.filtering_debug_level >= FILTERING_DEBUG_LEVEL_HIGH
				&& !found) {
			log_print(LOG_DEBUG_FILTERING,
					"address [ %s ] does NOT match\n",
					(gchar *)walk->data);
		}
		g_free(walk->data);

		if (match == MATCH_ALL) {
			/* if matching all addresses, stop if one doesn't match */
			if (!found) {
				/* debug output */
				if (debug_filtering_session
						&& prefs_common.filtering_debug_level >= FILTERING_DEBUG_LEVEL_HIGH) {
					log_print(LOG_DEBUG_FILTERING,
							"not all address match (matching all)\n");
				}
				break;
			}
		} else if (match == MATCH_ANY) {
			/* if matching any address, stop if one does match */
			if (found) {
				/* debug output */
				if (debug_filtering_session
						&& prefs_common.filtering_debug_level >= FILTERING_DEBUG_LEVEL_HIGH) {
					log_print(LOG_DEBUG_FILTERING,
							"at least one address matches (matching any)\n");
				}
				break;
			}
		}
		/* MATCH_ONE: there should be only one loop iteration */
	}

	end_address_completion();
	
	return found;
}

/*!
 *\brief	Find out if a string matches a condition
 *
 *\param	prop Matcher structure
 *\param	str String to check 
 *
 *\return	gboolean TRUE if str matches the condition in the 
 *		matcher structure
 */
static gboolean matcherprop_string_match(MatcherProp *prop, const gchar *str,
					 const gchar *debug_context)
{
	gchar *str1;
	gchar *down_expr;
	gboolean ret = FALSE;
	gboolean should_free = FALSE;
	if (str == NULL)
		return FALSE;

	if (prop->matchtype == MATCHTYPE_REGEXPCASE ||
	    prop->matchtype == MATCHTYPE_MATCHCASE) {
		str1 = g_utf8_casefold(str, -1);
		down_expr = g_utf8_casefold(prop->expr, -1);
		should_free = TRUE;
	} else {
		str1 = (gchar *)str;
		down_expr = (gchar *)prop->expr;
		should_free = FALSE;
	}

	switch (prop->matchtype) {
	case MATCHTYPE_REGEXPCASE:
	case MATCHTYPE_REGEXP:
#ifndef G_OS_WIN32
		if (!prop->preg && (prop->error == 0)) {
			prop->preg = g_new0(regex_t, 1);
			/* if regexp then don't use the escaped string */
			if (regcomp(prop->preg, down_expr,
				    REG_NOSUB | REG_EXTENDED
				    | ((prop->matchtype == MATCHTYPE_REGEXPCASE)
				    ? REG_ICASE : 0)) != 0) {
				prop->error = 1;
				g_free(prop->preg);
				prop->preg = NULL;
			}
		}
		if (prop->preg == NULL) {
			ret = FALSE;
			goto free_strs;
		}
		
		if (regexec(prop->preg, str1, 0, NULL, 0) == 0)
			ret = TRUE;
		else
			ret = FALSE;

		/* debug output */
		if (debug_filtering_session
				&& prefs_common.filtering_debug_level >= FILTERING_DEBUG_LEVEL_HIGH) {
			gchar *stripped = g_strdup(str);

			strretchomp(stripped);
			if (ret) {
				log_print(LOG_DEBUG_FILTERING,
						"%s value [ %s ] matches regular expression [ %s ] (%s)\n",
						debug_context, stripped, prop->expr,
						prop->matchtype == MATCHTYPE_REGEXP ? _("Case sensitive"):_("Case insensitive"));
			} else {
				log_print(LOG_DEBUG_FILTERING,
						"%s value [ %s ] does NOT match regular expression [ %s ] (%s)\n",
						debug_context, stripped, prop->expr,
						prop->matchtype == MATCHTYPE_REGEXP ? _("Case sensitive"):_("Case insensitive"));
			}
			g_free(stripped);
		}
		break;
#endif			
	case MATCHTYPE_MATCHCASE:
	case MATCHTYPE_MATCH:
		ret = (strstr(str1, down_expr) != NULL);

		/* debug output */
		if (debug_filtering_session
				&& prefs_common.filtering_debug_level >= FILTERING_DEBUG_LEVEL_HIGH) {
			gchar *stripped = g_strdup(str);

			strretchomp(stripped);
			if (ret) {
				log_print(LOG_DEBUG_FILTERING,
						"%s value [ %s ] contains [ %s ] (%s)\n",
						debug_context, stripped, prop->expr,
						prop->matchtype == MATCHTYPE_MATCH ? _("Case sensitive"):_("Case insensitive"));
			} else {
				log_print(LOG_DEBUG_FILTERING,
						"%s value [ %s ] does NOT contain [ %s ] (%s)\n",
						debug_context, stripped, prop->expr,
						prop->matchtype == MATCHTYPE_MATCH ? _("Case sensitive"):_("Case insensitive"));
			}
			g_free(stripped);
		}
		break;

	default:
		break;
	}
	
free_strs:
	if (should_free) {
		g_free(str1);
		g_free(down_expr);
	}
	return ret;
}

/*!
 *\brief	Find out if a tag matches a condition
 *
 *\param	prop Matcher structure
 *\param	msginfo message to check
 *
 *\return	gboolean TRUE if msginfo matches the condition in the 
 *		matcher structure
 */
static gboolean matcherprop_tag_match(MatcherProp *prop, MsgInfo *msginfo,
					 const gchar *debug_context)
{
	gboolean ret = FALSE;
	GSList *cur;

	if (msginfo == NULL || msginfo->tags == NULL)
		return FALSE;

	for (cur = msginfo->tags; cur; cur = cur->next) {
		const gchar *str = tags_get_tag(GPOINTER_TO_INT(cur->data));
		if (!str)
			continue;
		if (matcherprop_string_match(prop, str, debug_context)) {
			ret = TRUE;
			break;
		}
	}
	return ret;
}

/*!
 *\brief	Find out if the string-ed list matches a condition
 *
 *\param	prop Matcher structure
 *\param	list GSList of strings to check
 *
 *\return	gboolean TRUE if str matches the condition in the 
 *		matcher structure
 */
static gboolean matcherprop_list_match(MatcherProp *prop, const GSList *list,
const gchar *debug_context)
{
	const GSList *cur;

	for(cur = list; cur != NULL; cur = cur->next) {
		if (matcherprop_string_match(prop, (gchar *)cur->data, debug_context))
			return TRUE;
	}
	return FALSE;
}

static gboolean matcherprop_header_line_match(MatcherProp *prop, const gchar *hdr,
					      const gchar *str, const gchar *debug_context)
{
	gchar *line = NULL;
	gboolean res = FALSE;

	if (hdr == NULL || str == NULL)
		return FALSE;

	line = g_strdup_printf("%s %s", hdr, str);
	res = matcherprop_string_match(prop, line, debug_context);
	g_free(line);
       
	return res;
}

#ifdef USE_PTHREAD
typedef struct _thread_data {
	const gchar *cmd;
	gboolean done;
} thread_data;
#endif

#ifdef USE_PTHREAD
static void *matcher_test_thread(void *data)
{
	thread_data *td = (thread_data *)data;
	int result = -1;

	pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL);
	pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL);

	result = system(td->cmd);
	td->done = TRUE; /* let the caller thread join() */
	return GINT_TO_POINTER(result);
}
#endif

/*!
 *\brief	Execute a command defined in the matcher structure
 *
 *\param	prop Pointer to matcher structure
 *\param	info Pointer to message info structure
 *
 *\return	gboolean TRUE if command was executed succesfully
 */
static gboolean matcherprop_match_test(const MatcherProp *prop, 
					  MsgInfo *info)
{
	gchar *file;
	gchar *cmd;
	gint retval;
#ifdef USE_PTHREAD
	pthread_t pt;
	pthread_attr_t pta;
	thread_data *td = g_new0(thread_data, 1);
	void *res = NULL;
	time_t start_time = time(NULL);
#endif

	file = procmsg_get_message_file(info);
	if (file == NULL) {
#ifdef USE_PTHREAD
		g_free(td);
#endif
		return FALSE;
	}
	g_free(file);		

	cmd = matching_build_command(prop->expr, info);
	if (cmd == NULL) {
#ifdef USE_PTHREAD
		g_free(td);
#endif	
		return FALSE;
}

#ifdef USE_PTHREAD
	/* debug output */
	if (debug_filtering_session
			&& prefs_common.filtering_debug_level >= FILTERING_DEBUG_LEVEL_HIGH) {
		log_print(LOG_DEBUG_FILTERING,
				"starting threaded command [ %s ]\n",
				cmd);
	}

	td->cmd = cmd;
	td->done = FALSE;
	if (pthread_attr_init(&pta) != 0 ||
	    pthread_attr_setdetachstate(&pta, PTHREAD_CREATE_JOINABLE) != 0 ||
	    pthread_create(&pt, &pta, matcher_test_thread, td) != 0)
		retval = system(cmd);
	else {
		debug_print("waiting for test thread\n");
		while(!td->done) {
			/* don't let the interface freeze while waiting */
			claws_do_idle();
			if (time(NULL) - start_time > 30) {
				pthread_cancel(pt);
				td->done = TRUE;
				retval = -1;
			}
		}
		pthread_join(pt, &res);
		retval = GPOINTER_TO_INT(res);
		debug_print(" test thread returned %d\n", retval);
	}
	g_free(td);
#else
	/* debug output */
	if (debug_filtering_session
			&& prefs_common.filtering_debug_level >= FILTERING_DEBUG_LEVEL_HIGH) {
		log_print(LOG_DEBUG_FILTERING,
				"starting synchronous command [ %s ]\n",
				cmd);
	}

	retval = system(cmd);
#endif
	debug_print("Command exit code: %d\n", retval);

	/* debug output */
	if (debug_filtering_session
			&& prefs_common.filtering_debug_level >= FILTERING_DEBUG_LEVEL_HIGH) {
		log_print(LOG_DEBUG_FILTERING,
				"command returned [ %d ]\n",
				retval);
	}

	g_free(cmd);
	return (retval == 0);
}

/*!
 *\brief	Check if a message matches the condition in a matcher
 *		structure.
 *
 *\param	prop Pointer to matcher structure
 *\param	info Pointer to message info
 *
 *\return	gboolean TRUE if a match
 */
static gboolean matcherprop_match(MatcherProp *prop, 
				  MsgInfo *info)
{
	time_t t;

	switch(prop->criteria) {
	case MATCHCRITERIA_ALL:
		return TRUE;
	case MATCHCRITERIA_UNREAD:
		return MSG_IS_UNREAD(info->flags);
	case MATCHCRITERIA_NOT_UNREAD:
		return !MSG_IS_UNREAD(info->flags);
	case MATCHCRITERIA_NEW:
		return MSG_IS_NEW(info->flags);
	case MATCHCRITERIA_NOT_NEW:
		return !MSG_IS_NEW(info->flags);
	case MATCHCRITERIA_MARKED:
		return MSG_IS_MARKED(info->flags);
	case MATCHCRITERIA_NOT_MARKED:
		return !MSG_IS_MARKED(info->flags);
	case MATCHCRITERIA_DELETED:
		return MSG_IS_DELETED(info->flags);
	case MATCHCRITERIA_NOT_DELETED:
		return !MSG_IS_DELETED(info->flags);
	case MATCHCRITERIA_REPLIED:
		return MSG_IS_REPLIED(info->flags);
	case MATCHCRITERIA_NOT_REPLIED:
		return !MSG_IS_REPLIED(info->flags);
	case MATCHCRITERIA_FORWARDED:
		return MSG_IS_FORWARDED(info->flags);
	case MATCHCRITERIA_NOT_FORWARDED:
		return !MSG_IS_FORWARDED(info->flags);
	case MATCHCRITERIA_LOCKED:
		return MSG_IS_LOCKED(info->flags);
	case MATCHCRITERIA_NOT_LOCKED:
		return !MSG_IS_LOCKED(info->flags);
	case MATCHCRITERIA_SPAM:
		return MSG_IS_SPAM(info->flags);
	case MATCHCRITERIA_NOT_SPAM:
		return !MSG_IS_SPAM(info->flags);
	case MATCHCRITERIA_HAS_ATTACHMENT:
		return MSG_IS_WITH_ATTACHMENT(info->flags);
	case MATCHCRITERIA_HAS_NO_ATTACHMENT:
		return !MSG_IS_WITH_ATTACHMENT(info->flags);
	case MATCHCRITERIA_SIGNED:
		return MSG_IS_SIGNED(info->flags);
	case MATCHCRITERIA_NOT_SIGNED:
		return !MSG_IS_SIGNED(info->flags);
	case MATCHCRITERIA_COLORLABEL:
	{
		gint color = MSG_GET_COLORLABEL_VALUE(info->flags);
		gboolean ret = (color == prop->value);

		/* debug output */
		if (debug_filtering_session
				&& prefs_common.filtering_debug_level >= FILTERING_DEBUG_LEVEL_HIGH) {
			if (ret) {
				log_print(LOG_DEBUG_FILTERING,
						"message color value [ %d ] matches color value [ %d ]\n",
						color, prop->value);
			} else {
				log_print(LOG_DEBUG_FILTERING,
						"message color value [ %d ] does NOT match color value [ %d ]\n",
						color, prop->value);
			}
		}
		return ret;
	}
	case MATCHCRITERIA_NOT_COLORLABEL:
	{
		gint color = MSG_GET_COLORLABEL_VALUE(info->flags);
		gboolean ret = (color != prop->value);

		/* debug output */
		if (debug_filtering_session
				&& prefs_common.filtering_debug_level >= FILTERING_DEBUG_LEVEL_HIGH) {
			if (ret) {
				log_print(LOG_DEBUG_FILTERING,
						"message color value [ %d ] matches color value [ %d ]\n",
						color, prop->value);
			} else {
				log_print(LOG_DEBUG_FILTERING,
						"message color value [ %d ] does NOT match color value [ %d ]\n",
						color, prop->value);
			}
		}
		return ret;
	}
	case MATCHCRITERIA_IGNORE_THREAD:
		return MSG_IS_IGNORE_THREAD(info->flags);
	case MATCHCRITERIA_NOT_IGNORE_THREAD:
		return !MSG_IS_IGNORE_THREAD(info->flags);
	case MATCHCRITERIA_WATCH_THREAD:
		return MSG_IS_WATCH_THREAD(info->flags);
	case MATCHCRITERIA_NOT_WATCH_THREAD:
		return !MSG_IS_WATCH_THREAD(info->flags);
	case MATCHCRITERIA_SUBJECT:
		return matcherprop_string_match(prop, info->subject, context_str[CONTEXT_SUBJECT]);
	case MATCHCRITERIA_NOT_SUBJECT:
		return !matcherprop_string_match(prop, info->subject, context_str[CONTEXT_SUBJECT]);
	case MATCHCRITERIA_FROM:
		return matcherprop_string_match(prop, info->from, context_str[CONTEXT_FROM]);
	case MATCHCRITERIA_NOT_FROM:
		return !matcherprop_string_match(prop, info->from, context_str[CONTEXT_FROM]);
	case MATCHCRITERIA_TO:
		return matcherprop_string_match(prop, info->to, context_str[CONTEXT_TO]);
	case MATCHCRITERIA_NOT_TO:
		return !matcherprop_string_match(prop, info->to, context_str[CONTEXT_TO]);
	case MATCHCRITERIA_CC:
		return matcherprop_string_match(prop, info->cc, context_str[CONTEXT_CC]);
	case MATCHCRITERIA_NOT_CC:
		return !matcherprop_string_match(prop, info->cc, context_str[CONTEXT_CC]);
	case MATCHCRITERIA_TO_OR_CC:
		return matcherprop_string_match(prop, info->to, context_str[CONTEXT_TO])
		     || matcherprop_string_match(prop, info->cc, context_str[CONTEXT_CC]);
	case MATCHCRITERIA_NOT_TO_AND_NOT_CC:
		return !matcherprop_string_match(prop, info->to, context_str[CONTEXT_TO])
		     && !matcherprop_string_match(prop, info->cc, context_str[CONTEXT_CC]);
	case MATCHCRITERIA_TAG:
		return matcherprop_tag_match(prop, info, context_str[CONTEXT_TAG]);
	case MATCHCRITERIA_NOT_TAG:
		return !matcherprop_tag_match(prop, info, context_str[CONTEXT_TAG]);
	case MATCHCRITERIA_TAGGED:
		return info->tags != NULL;
	case MATCHCRITERIA_NOT_TAGGED:
		return info->tags == NULL;
	case MATCHCRITERIA_AGE_GREATER:
	{
		gboolean ret;
		gint age;

		t = time(NULL);
		age = ((t - info->date_t) / (60 * 60 * 24));
		ret = (age >= prop->value);

		/* debug output */
		if (debug_filtering_session
				&& prefs_common.filtering_debug_level >= FILTERING_DEBUG_LEVEL_HIGH) {
			if (ret) {
				log_print(LOG_DEBUG_FILTERING,
						"message age [ %d ] is greater than [ %d ]\n",
						age, prop->value);
			} else {
				log_print(LOG_DEBUG_FILTERING,
						"message age [ %d ] is not greater than [ %d ]\n",
						age, prop->value);
			}
		}
		return ret;
	}
	case MATCHCRITERIA_AGE_LOWER:
	{
		gboolean ret;
		gint age;

		t = time(NULL);
		age = ((t - info->date_t) / (60 * 60 * 24));
		ret = (age < prop->value);

		/* debug output */
		if (debug_filtering_session
				&& prefs_common.filtering_debug_level >= FILTERING_DEBUG_LEVEL_HIGH) {
			if (ret) {
				log_print(LOG_DEBUG_FILTERING,
						"message age [ %d ] is lower than [ %d ]\n",
						age, prop->value);
			} else {
				log_print(LOG_DEBUG_FILTERING,
						"message age [ %d ] is not lower than [ %d ]\n",
						age, prop->value);
			}
		}
		return ret;
	}
	case MATCHCRITERIA_SCORE_GREATER:
	{
		gboolean ret = (info->score > prop->value);

		/* debug output */
		if (debug_filtering_session
				&& prefs_common.filtering_debug_level >= FILTERING_DEBUG_LEVEL_HIGH) {
			if (ret) {
				log_print(LOG_DEBUG_FILTERING,
						"message score [ %d ] is greater than [ %d ]\n",
						info->score, prop->value);
			} else {
				log_print(LOG_DEBUG_FILTERING,
						"message score [ %d ] is not greater than [ %d ]\n",
						info->score, prop->value);
			}
		}
		return ret;
	}
	case MATCHCRITERIA_SCORE_LOWER:
	{
		gboolean ret = (info->score < prop->value);

		/* debug output */
		if (debug_filtering_session
				&& prefs_common.filtering_debug_level >= FILTERING_DEBUG_LEVEL_HIGH) {
			if (ret) {
				log_print(LOG_DEBUG_FILTERING,
						"message score [ %d ] is lower than [ %d ]\n",
						info->score, prop->value);
			} else {
				log_print(LOG_DEBUG_FILTERING,
						"message score [ %d ] is not lower than [ %d ]\n",
						info->score, prop->value);
			}
		}
		return ret;
	}
	case MATCHCRITERIA_SCORE_EQUAL:
	{
		gboolean ret = (info->score == prop->value);

		/* debug output */
		if (debug_filtering_session
				&& prefs_common.filtering_debug_level >= FILTERING_DEBUG_LEVEL_HIGH) {
			if (ret) {
				log_print(LOG_DEBUG_FILTERING,
						"message score [ %d ] is equal to [ %d ]\n",
						info->score, prop->value);
			} else {
				log_print(LOG_DEBUG_FILTERING,
						"message score [ %d ] is not equal to [ %d ]\n",
						info->score, prop->value);
			}
		}
		return ret;
	}
	case MATCHCRITERIA_SIZE_GREATER:
	{
		/* FIXME: info->size is a goffset */
		gboolean ret = (info->size > (goffset) prop->value);

		/* debug output */
		if (debug_filtering_session
				&& prefs_common.filtering_debug_level >= FILTERING_DEBUG_LEVEL_HIGH) {
			if (ret) {
				log_print(LOG_DEBUG_FILTERING,
						"message size is greater than [ %d ]\n",
						prop->value);
			} else {
				log_print(LOG_DEBUG_FILTERING,
						"message size is not greater than [ %d ]\n",
						prop->value);
			}
		}
		return ret;
	}
	case MATCHCRITERIA_SIZE_SMALLER:
	{
		/* FIXME: info->size is a goffset */
		gboolean ret = (info->size < (goffset) prop->value);

		/* debug output */
		if (debug_filtering_session
				&& prefs_common.filtering_debug_level >= FILTERING_DEBUG_LEVEL_HIGH) {
			if (ret) {
				log_print(LOG_DEBUG_FILTERING,
						"message size is smaller than [ %d ]\n",
						prop->value);
			} else {
				log_print(LOG_DEBUG_FILTERING,
						"message size is not smaller than [ %d ]\n",
						prop->value);
			}
		}
		return ret;
	}
	case MATCHCRITERIA_SIZE_EQUAL:
	{
		/* FIXME: info->size is a goffset */
		gboolean ret = (info->size == (goffset) prop->value);

		/* debug output */
		if (debug_filtering_session
				&& prefs_common.filtering_debug_level >= FILTERING_DEBUG_LEVEL_HIGH) {
			if (ret) {
				log_print(LOG_DEBUG_FILTERING,
						"message size is equal to [ %d ]\n",
						prop->value);
			} else {
				log_print(LOG_DEBUG_FILTERING,
						"message size is not equal to [ %d ]\n",
						prop->value);
			}
		}
		return ret;
	}
	case MATCHCRITERIA_PARTIAL:
	{
		/* FIXME: info->size is a goffset */
		gboolean ret = (info->total_size != 0 && info->size != (goffset)info->total_size);

		/* debug output */
		if (debug_filtering_session
				&& prefs_common.filtering_debug_level >= FILTERING_DEBUG_LEVEL_HIGH) {
			if (ret) {
				log_print(LOG_DEBUG_FILTERING,
						"message is partially downloaded, size is less than total size [ %d ])\n",
						info->total_size);
			} else {
				log_print(LOG_DEBUG_FILTERING,
						"message is not partially downloaded\n");
			}
		}
		return ret;
	}
	case MATCHCRITERIA_NOT_PARTIAL:
	{
		/* FIXME: info->size is a goffset */
		gboolean ret = (info->total_size == 0 || info->size == (goffset)info->total_size);

		/* debug output */
		if (debug_filtering_session
				&& prefs_common.filtering_debug_level >= FILTERING_DEBUG_LEVEL_HIGH) {
			if (ret) {
				log_print(LOG_DEBUG_FILTERING,
						"message is not partially downloaded\n");
			} else {
				log_print(LOG_DEBUG_FILTERING,
						"message is partially downloaded, size is less than total size [ %d ])\n",
						info->total_size);
			}
		}
		return ret;
	}
	case MATCHCRITERIA_NEWSGROUPS:
		return matcherprop_string_match(prop, info->newsgroups, context_str[CONTEXT_NEWSGROUPS]);
	case MATCHCRITERIA_NOT_NEWSGROUPS:
		return !matcherprop_string_match(prop, info->newsgroups, context_str[CONTEXT_NEWSGROUPS]);
	case MATCHCRITERIA_INREPLYTO:
		return matcherprop_string_match(prop, info->inreplyto, context_str[CONTEXT_IN_REPLY_TO]);
	case MATCHCRITERIA_NOT_INREPLYTO:
		return !matcherprop_string_match(prop, info->inreplyto, context_str[CONTEXT_IN_REPLY_TO]);
	case MATCHCRITERIA_REFERENCES:
		return matcherprop_list_match(prop, info->references, context_str[CONTEXT_REFERENCES]);
	case MATCHCRITERIA_NOT_REFERENCES:
		return !matcherprop_list_match(prop, info->references, context_str[CONTEXT_REFERENCES]);
	case MATCHCRITERIA_TEST:
		return matcherprop_match_test(prop, info);
	case MATCHCRITERIA_NOT_TEST:
		return !matcherprop_match_test(prop, info);
	default:
		return FALSE;
	}
}

/* ********************* MatcherList *************************** */

/*!
 *\brief	Create a new list of matchers 
 *
 *\param	matchers List of matcher structures
 *\param	bool_and Operator
 *
 *\return	MatcherList * New list
 */
MatcherList *matcherlist_new(GSList *matchers, gboolean bool_and)
{
	MatcherList *cond;

	cond = g_new0(MatcherList, 1);

	cond->matchers = matchers;
	cond->bool_and = bool_and;

	return cond;
}

/*!
 *\brief	Frees a list of matchers
 *
 *\param	cond List of matchers
 */
void matcherlist_free(MatcherList *cond)
{
	GSList *l;

	cm_return_if_fail(cond);
	for (l = cond->matchers ; l != NULL ; l = g_slist_next(l)) {
		matcherprop_free((MatcherProp *) l->data);
	}
	g_slist_free(cond->matchers);
	g_free(cond);
}

/*!
 *\brief	Skip all headers in a message file
 *
 *\param	fp Message file
 */
static void matcherlist_skip_headers(FILE *fp)
{
	gchar buf[BUFFSIZE];

	while (procheader_get_one_field(buf, sizeof(buf), fp, NULL) != -1)
		;
}

/*!
 *\brief	Check if a header matches a matcher condition
 *
 *\param	matcher Matcher structure to check header for
 *\param	buf Header name
 *
 *\return	boolean TRUE if matching header
 */
static gboolean matcherprop_match_one_header(MatcherProp *matcher,
					     gchar *buf)
{
	gboolean result = FALSE;
	Header *header = NULL;

	switch (matcher->criteria) {
	case MATCHCRITERIA_HEADER:
	case MATCHCRITERIA_NOT_HEADER:
		header = procheader_parse_header(buf);
		if (!header)
			return FALSE;
		if (procheader_headername_equal(header->name,
						matcher->header)) {
			if (matcher->criteria == MATCHCRITERIA_HEADER)
				result = matcherprop_string_match(matcher, header->body, context_str[CONTEXT_HEADER]);
			else
				result = !matcherprop_string_match(matcher, header->body, context_str[CONTEXT_HEADER]);
			procheader_header_free(header);
			return result;
		}
		else {
			procheader_header_free(header);
		}
		break;
	case MATCHCRITERIA_HEADERS_PART:
	case MATCHCRITERIA_MESSAGE:
		header = procheader_parse_header(buf);
		if (!header)
			return FALSE;
		result = matcherprop_header_line_match(matcher, 
			       header->name, header->body, context_str[CONTEXT_HEADER_LINE]);
		procheader_header_free(header);
		return result;
	case MATCHCRITERIA_NOT_HEADERS_PART:
	case MATCHCRITERIA_NOT_MESSAGE:
		header = procheader_parse_header(buf);
		if (!header)
			return FALSE;
		result = !matcherprop_header_line_match(matcher, 
			       header->name, header->body, context_str[CONTEXT_HEADER_LINE]);
		procheader_header_free(header);
		return result;
	case MATCHCRITERIA_FOUND_IN_ADDRESSBOOK:
	case MATCHCRITERIA_NOT_FOUND_IN_ADDRESSBOOK:
		{
			GSList *address_list = NULL;
			gint match = MATCH_ONE;
			gboolean found = FALSE;

			/* how many address headers are me trying to mach? */
			if (strcasecmp(matcher->header, "Any") == 0)
				match = MATCH_ANY;
			else if (strcasecmp(matcher->header, "All") == 0)
					match = MATCH_ALL;

			if (match == MATCH_ONE) {
				/* matching one address header exactly, is that the right one? */
				header = procheader_parse_header(buf);
				if (!header ||
						!procheader_headername_equal(header->name, matcher->header))
					return FALSE;
				address_list = address_list_append(address_list, header->body);
				if (address_list == NULL)
					return FALSE;

			} else {
				header = procheader_parse_header(buf);
				if (!header)
					return FALSE;
				/* address header is one of the headers we have to match when checking
				   for any address header or all address headers? */
				if (procheader_headername_equal(header->name, "From") ||
					 procheader_headername_equal(header->name, "To") ||
					 procheader_headername_equal(header->name, "Cc") ||
					 procheader_headername_equal(header->name, "Reply-To") ||
					 procheader_headername_equal(header->name, "Sender"))
					address_list = address_list_append(address_list, header->body);
				if (address_list == NULL)
					return FALSE;
			}

			found = match_with_addresses_in_addressbook
							(matcher, address_list, matcher->criteria,
							 matcher->expr, match);
			g_slist_free(address_list);

			if (matcher->criteria == MATCHCRITERIA_NOT_FOUND_IN_ADDRESSBOOK)
				return !found;
			else
				return found;
	}
	}

	return FALSE;
}

/*!
 *\brief	Check if the matcher structure wants headers to
 *		be matched
 *
 *\param	matcher Matcher structure
 *
 *\return	gboolean TRUE if the matcher structure describes
 *		a header match condition
 */
static gboolean matcherprop_criteria_headers(const MatcherProp *matcher)
{
	switch (matcher->criteria) {
	case MATCHCRITERIA_HEADER:
	case MATCHCRITERIA_NOT_HEADER:
	case MATCHCRITERIA_HEADERS_PART:
	case MATCHCRITERIA_NOT_HEADERS_PART:
	case MATCHCRITERIA_FOUND_IN_ADDRESSBOOK:
	case MATCHCRITERIA_NOT_FOUND_IN_ADDRESSBOOK:
		return TRUE;
	default:
		return FALSE;
	}
}

/*!
 *\brief	Check if the matcher structure wants the message
 *		to be matched (just perform an action on any
 *		message)
 *
 *\param	matcher Matcher structure
 *
 *\return	gboolean TRUE if matcher condition should match
 *		a message
 */
static gboolean matcherprop_criteria_message(MatcherProp *matcher)
{
	switch (matcher->criteria) {
	case MATCHCRITERIA_MESSAGE:
	case MATCHCRITERIA_NOT_MESSAGE:
		return TRUE;
	default:
		return FALSE;
	}
}

/*!
 *\brief	Check if a list of conditions matches one header in
 *		a message file.
 *
 *\param	matchers List of conditions
 *\param	fp Message file
 *
 *\return	gboolean TRUE if one of the headers is matched by
 *		the list of conditions.	
 */
static gboolean matcherlist_match_headers(MatcherList *matchers, FILE *fp)
{
	GSList *l;
	gchar buf[BUFFSIZE];

	while (procheader_get_one_field(buf, sizeof(buf), fp, NULL) != -1) {
		for (l = matchers->matchers ; l != NULL ; l = g_slist_next(l)) {
			MatcherProp *matcher = (MatcherProp *) l->data;
			gint match = MATCH_ANY;

			if (matcher->done)
				continue;

			/* determine the match range (all, any are our concern here) */
			if (matcher->criteria == MATCHCRITERIA_NOT_HEADERS_PART ||
			    matcher->criteria == MATCHCRITERIA_NOT_MESSAGE) {
				match = MATCH_ALL;

			} else if (matcher->criteria == MATCHCRITERIA_FOUND_IN_ADDRESSBOOK ||
			 		   matcher->criteria == MATCHCRITERIA_NOT_FOUND_IN_ADDRESSBOOK) {
				Header *header = NULL;

				/* address header is one of the headers we have to match when checking
				   for any address header or all address headers? */
				header = procheader_parse_header(buf);
				if (header &&
					(procheader_headername_equal(header->name, "From") ||
					 procheader_headername_equal(header->name, "To") ||
					 procheader_headername_equal(header->name, "Cc") ||
					 procheader_headername_equal(header->name, "Reply-To") ||
					 procheader_headername_equal(header->name, "Sender"))) {

					if (strcasecmp(matcher->header, "Any") == 0)
						match = MATCH_ANY;
					else if (strcasecmp(matcher->header, "All") == 0)
						match = MATCH_ALL;
					else
						match = MATCH_ONE;
				} else {
					/* further call to matcherprop_match_one_header() can't match
					   and it irrelevant, so: don't alter the match result */
					continue;
				}
			}

			/* ZERO line must NOT match for the rule to match.
			 */
			if (match == MATCH_ALL) {
				if (matcherprop_match_one_header(matcher, buf)) {
					matcher->result = TRUE;
				} else {
					matcher->result = FALSE;
					matcher->done = TRUE;
				}
			/* else, just one line matching is enough for the rule to match
			 */
			} else if (matcherprop_criteria_headers(matcher) ||
			           matcherprop_criteria_message(matcher)) {
				if (matcherprop_match_one_header(matcher, buf)) {
					matcher->result = TRUE;
					matcher->done = TRUE;
				}
			}
			
			/* if the rule matched and the matchers are OR, no need to
			 * check the others */
			if (matcher->result && matcher->done) {
				if (!matchers->bool_and)
					return TRUE;
			}
		}
	}

	return FALSE;
}

/*!
 *\brief	Check if a matcher wants to check the message body
 *
 *\param	matcher Matcher structure
 *
 *\return	gboolean TRUE if body must be matched.
 */
static gboolean matcherprop_criteria_body(const MatcherProp *matcher)
{
	switch (matcher->criteria) {
	case MATCHCRITERIA_BODY_PART:
	case MATCHCRITERIA_NOT_BODY_PART:
		return TRUE;
	default:
		return FALSE;
	}
}
	
static gboolean matcherlist_match_binary_content(MatcherList *matchers, MimeInfo *partinfo)
{
	FILE *outfp;
	gchar buf[BUFFSIZE];
	GSList *l;

	if (partinfo->type == MIMETYPE_TEXT)
		return FALSE;
	else
		outfp = procmime_get_binary_content(partinfo);

	if (!outfp)
		return FALSE;

	while (fgets(buf, sizeof(buf), outfp) != NULL) {
		strretchomp(buf);

		for (l = matchers->matchers ; l != NULL ; l = g_slist_next(l)) {
			MatcherProp *matcher = (MatcherProp *) l->data;

			if (matcher->done) 
				continue;

			/* Don't scan non-text parts when looking in body, only
			 * when looking in whole message
			 */
			if (partinfo && partinfo->type != MIMETYPE_TEXT &&
			(matcher->criteria == MATCHCRITERIA_NOT_BODY_PART ||
			matcher->criteria == MATCHCRITERIA_BODY_PART))
				continue;

			/* if the criteria is ~body_part or ~message, ZERO lines
			 * must match for the rule to match.
			 */
			if (matcher->criteria == MATCHCRITERIA_NOT_BODY_PART ||
			    matcher->criteria == MATCHCRITERIA_NOT_MESSAGE) {
				if (matcherprop_string_match(matcher, buf, 
							context_str[CONTEXT_BODY_LINE])) {
					matcher->result = FALSE;
					matcher->done = TRUE;
				} else
					matcher->result = TRUE;
			/* else, just one line has to match */
			} else if (matcherprop_criteria_body(matcher) ||
				   matcherprop_criteria_message(matcher)) {
				if (matcherprop_string_match(matcher, buf,
							context_str[CONTEXT_BODY_LINE])) {
					matcher->result = TRUE;
					matcher->done = TRUE;
				}
			}

			/* if the matchers are OR'ed and the rule matched,
			 * no need to check the others. */
			if (matcher->result && matcher->done) {
				if (!matchers->bool_and) {
					fclose(outfp);
					return TRUE;
				}
			}
		}
	}

	fclose(outfp);
	return FALSE;
}

static gboolean match_content_cb(const gchar *buf, gpointer data)
{
	MatcherList *matchers = (MatcherList *)data;
	gboolean all_done = TRUE;
	GSList *l;

	for (l = matchers->matchers ; l != NULL ; l = g_slist_next(l)) {
		MatcherProp *matcher = (MatcherProp *) l->data;

		if (matcher->done) 
			continue;

		/* if the criteria is ~body_part or ~message, ZERO lines
		 * must match for the rule to match.
		 */
		if (matcher->criteria == MATCHCRITERIA_NOT_BODY_PART ||
		    matcher->criteria == MATCHCRITERIA_NOT_MESSAGE) {
			if (matcherprop_string_match(matcher, buf, 
						context_str[CONTEXT_BODY_LINE])) {
				matcher->result = FALSE;
				matcher->done = TRUE;
			} else
				matcher->result = TRUE;
		/* else, just one line has to match */
		} else if (matcherprop_criteria_body(matcher) ||
			   matcherprop_criteria_message(matcher)) {
			if (matcherprop_string_match(matcher, buf,
						context_str[CONTEXT_BODY_LINE])) {
				matcher->result = TRUE;
				matcher->done = TRUE;
			}
		}

		/* if the matchers are OR'ed and the rule matched,
		 * no need to check the others. */
		if (matcher->result && matcher->done) {
			if (!matchers->bool_and) {
				return TRUE;
			}
		}

		if (!matcher->done)
			all_done = FALSE;
	}
	return all_done;
}

static gboolean matcherlist_match_text_content(MatcherList *matchers, MimeInfo *partinfo)
{
	if (partinfo->type != MIMETYPE_TEXT)
		return FALSE;

	return procmime_scan_text_content(partinfo, match_content_cb, matchers);
}

/*!
 *\brief	Check if a line in a message file's body matches
 *		the criteria
 *
 *\param	matchers List of conditions
 *\param	fp Message file
 *
 *\return	gboolean TRUE if succesful match
 */
static gboolean matcherlist_match_body(MatcherList *matchers, gboolean body_only, MsgInfo *info)
{
	MimeInfo *mimeinfo = NULL;
	MimeInfo *partinfo = NULL;
	gboolean first_text_found = FALSE;

	cm_return_val_if_fail(info != NULL, FALSE);

	mimeinfo = procmime_scan_message(info);

	/* Skip headers */
	partinfo = procmime_mimeinfo_next(mimeinfo);

	for (; partinfo != NULL; partinfo = procmime_mimeinfo_next(partinfo)) {

		if (partinfo->type != MIMETYPE_TEXT && body_only)
			continue;

		if (partinfo->type == MIMETYPE_TEXT) {
			first_text_found = TRUE;
			if (matcherlist_match_text_content(matchers, partinfo)) {
				procmime_mimeinfo_free_all(mimeinfo);
				return TRUE;
			}
		} else if (matcherlist_match_binary_content(matchers, partinfo)) {
			procmime_mimeinfo_free_all(mimeinfo);
			return TRUE;
		}

		if (body_only && first_text_found)
			break;
	}
	procmime_mimeinfo_free_all(mimeinfo);

	return FALSE;
}

/*!
 *\brief	Check if a message file matches criteria
 *
 *\param	matchers Criteria
 *\param	info Message info
 *\param	result Default result
 *
 *\return	gboolean TRUE if matched
 */
static gboolean matcherlist_match_file(MatcherList *matchers, MsgInfo *info,
				gboolean result)
{
	gboolean read_headers;
	gboolean read_body;
	gboolean body_only;
	GSList *l;
	FILE *fp;
	gchar *file;

	/* file need to be read ? */

	read_headers = FALSE;
	read_body = FALSE;
	body_only = TRUE;
	for (l = matchers->matchers ; l != NULL ; l = g_slist_next(l)) {
		MatcherProp *matcher = (MatcherProp *) l->data;

		if (matcherprop_criteria_headers(matcher))
			read_headers = TRUE;
		if (matcherprop_criteria_body(matcher))
			read_body = TRUE;
		if (matcherprop_criteria_message(matcher)) {
			read_headers = TRUE;
			read_body = TRUE;
			body_only = FALSE;
		}
		matcher->result = FALSE;
		matcher->done = FALSE;
	}

	if (!read_headers && !read_body)
		return result;

	file = procmsg_get_message_file_full(info, read_headers, read_body);
	if (file == NULL)
		return FALSE;

	if ((fp = g_fopen(file, "rb")) == NULL) {
		FILE_OP_ERROR(file, "fopen");
		g_free(file);
		return result;
	}

	/* read the headers */

	if (read_headers) {
		if (matcherlist_match_headers(matchers, fp))
			read_body = FALSE;
	} else {
		matcherlist_skip_headers(fp);
	}

	/* read the body */
	if (read_body) {
		matcherlist_match_body(matchers, body_only, info);
	}
	
	for (l = matchers->matchers; l != NULL; l = g_slist_next(l)) {
		MatcherProp *matcher = (MatcherProp *) l->data;

		if (matcherprop_criteria_headers(matcher) ||
		    matcherprop_criteria_body(matcher)	  ||
		    matcherprop_criteria_message(matcher)) {
			if (matcher->result) {
				if (!matchers->bool_and) {
					result = TRUE;
					break;
				}
			}
			else {
				if (matchers->bool_and) {
					result = FALSE;
					break;
				}
			}
		}			
	}

	g_free(file);

	fclose(fp);
	
	return result;
}

/*!
 *\brief	Test list of conditions on a message.
 *
 *\param	matchers List of conditions
 *\param	info Message info
 *
 *\return	gboolean TRUE if matched
 */
gboolean matcherlist_match(MatcherList *matchers, MsgInfo *info)
{
	GSList *l;
	gboolean result;

	if (!matchers)
		return FALSE;

	if (matchers->bool_and)
		result = TRUE;
	else
		result = FALSE;

	/* test the cached elements */

	for (l = matchers->matchers; l != NULL ;l = g_slist_next(l)) {
		MatcherProp *matcher = (MatcherProp *) l->data;

		if (debug_filtering_session) {
			gchar *buf = matcherprop_to_string(matcher);
			log_print(LOG_DEBUG_FILTERING, _("checking if message matches [ %s ]\n"), buf);
			g_free(buf);
		}

		switch(matcher->criteria) {
		case MATCHCRITERIA_ALL:
		case MATCHCRITERIA_UNREAD:
		case MATCHCRITERIA_NOT_UNREAD:
		case MATCHCRITERIA_NEW:
		case MATCHCRITERIA_NOT_NEW:
		case MATCHCRITERIA_MARKED:
		case MATCHCRITERIA_NOT_MARKED:
		case MATCHCRITERIA_DELETED:
		case MATCHCRITERIA_NOT_DELETED:
		case MATCHCRITERIA_REPLIED:
		case MATCHCRITERIA_NOT_REPLIED:
		case MATCHCRITERIA_FORWARDED:
		case MATCHCRITERIA_NOT_FORWARDED:
		case MATCHCRITERIA_LOCKED:
		case MATCHCRITERIA_NOT_LOCKED:
		case MATCHCRITERIA_SPAM:
		case MATCHCRITERIA_NOT_SPAM:
		case MATCHCRITERIA_HAS_ATTACHMENT:
		case MATCHCRITERIA_HAS_NO_ATTACHMENT:
		case MATCHCRITERIA_SIGNED:
		case MATCHCRITERIA_NOT_SIGNED:
		case MATCHCRITERIA_COLORLABEL:
		case MATCHCRITERIA_NOT_COLORLABEL:
		case MATCHCRITERIA_IGNORE_THREAD:
		case MATCHCRITERIA_NOT_IGNORE_THREAD:
		case MATCHCRITERIA_WATCH_THREAD:
		case MATCHCRITERIA_NOT_WATCH_THREAD:
		case MATCHCRITERIA_SUBJECT:
		case MATCHCRITERIA_NOT_SUBJECT:
		case MATCHCRITERIA_FROM:
		case MATCHCRITERIA_NOT_FROM:
		case MATCHCRITERIA_TO:
		case MATCHCRITERIA_NOT_TO:
		case MATCHCRITERIA_CC:
		case MATCHCRITERIA_NOT_CC:
		case MATCHCRITERIA_TO_OR_CC:
		case MATCHCRITERIA_NOT_TO_AND_NOT_CC:
		case MATCHCRITERIA_TAG:
		case MATCHCRITERIA_NOT_TAG:
		case MATCHCRITERIA_TAGGED:
		case MATCHCRITERIA_NOT_TAGGED:
		case MATCHCRITERIA_AGE_GREATER:
		case MATCHCRITERIA_AGE_LOWER:
		case MATCHCRITERIA_NEWSGROUPS:
		case MATCHCRITERIA_NOT_NEWSGROUPS:
		case MATCHCRITERIA_INREPLYTO:
		case MATCHCRITERIA_NOT_INREPLYTO:
		case MATCHCRITERIA_REFERENCES:
		case MATCHCRITERIA_NOT_REFERENCES:
		case MATCHCRITERIA_SCORE_GREATER:
		case MATCHCRITERIA_SCORE_LOWER:
		case MATCHCRITERIA_SCORE_EQUAL:
		case MATCHCRITERIA_SIZE_GREATER:
		case MATCHCRITERIA_SIZE_SMALLER:
		case MATCHCRITERIA_SIZE_EQUAL:
		case MATCHCRITERIA_TEST:
		case MATCHCRITERIA_NOT_TEST:
		case MATCHCRITERIA_PARTIAL:
		case MATCHCRITERIA_NOT_PARTIAL:
			if (matcherprop_match(matcher, info)) {
				if (!matchers->bool_and) {
					if (debug_filtering_session)
						log_status_ok(LOG_DEBUG_FILTERING, _("message matches\n"));
					return TRUE;
				}
			}
			else {
				if (matchers->bool_and) {
					if (debug_filtering_session)
						log_status_nok(LOG_DEBUG_FILTERING, _("message does not match\n"));
					return FALSE;
				}
			}
		}
	}

	/* test the condition on the file */

	if (matcherlist_match_file(matchers, info, result)) {
		if (!matchers->bool_and) {
			if (debug_filtering_session)
				log_status_ok(LOG_DEBUG_FILTERING, _("message matches\n"));
			return TRUE;
		}
	} else {
		if (matchers->bool_and) {
			if (debug_filtering_session)
				log_status_nok(LOG_DEBUG_FILTERING, _("message does not match\n"));
			return FALSE;
		}
	}

	if (debug_filtering_session) {
		if (result)
			log_status_ok(LOG_DEBUG_FILTERING, _("message matches\n"));
		else
			log_status_nok(LOG_DEBUG_FILTERING, _("message does not match\n"));
	}
	return result;
}


static gint quote_filter_str(gchar * result, guint size,
			     const gchar * path)
{
	const gchar * p;
	gchar * result_p;
	guint remaining;

	result_p = result;
	remaining = size;

	for(p = path ; * p != '\0' ; p ++) {

		if ((* p != '\"') && (* p != '\\')) {
			if (remaining > 0) {
				* result_p = * p;
				result_p ++; 
				remaining --;
			}
			else {
				result[size - 1] = '\0';
				return -1;
			}
		}
		else { 
			if (remaining >= 2) {
				* result_p = '\\';
				result_p ++; 
				* result_p = * p;
				result_p ++; 
				remaining -= 2;
			}
			else {
				result[size - 1] = '\0';
				return -1;
			}
		}
	}
	if (remaining > 0) {
		* result_p = '\0';
	}
	else {
		result[size - 1] = '\0';
		return -1;
	}
  
	return 0;
}


gchar * matcher_quote_str(const gchar * src)
{
	gchar * res;
	gint len;
	
	len = strlen(src) * 2 + 1;
	res = g_malloc(len);
	quote_filter_str(res, len, src);
	
	return res;
}

/*!
 *\brief	Convert a matcher structure to a string
 *
 *\param	matcher Matcher structure
 *
 *\return	gchar * Newly allocated string
 */
gchar *matcherprop_to_string(MatcherProp *matcher)
{
	gchar *matcher_str = NULL;
	const gchar *criteria_str;
	const gchar *matchtype_str;
	int i;
	gchar * quoted_expr;
	gchar * quoted_header;
	
	criteria_str = NULL;
	for (i = 0; i < (int) (sizeof(matchparser_tab) / sizeof(MatchParser)); i++) {
		if (matchparser_tab[i].id == matcher->criteria)
			criteria_str = matchparser_tab[i].str;
	}
	if (criteria_str == NULL)
		return NULL;

	switch (matcher->criteria) {
	case MATCHCRITERIA_AGE_GREATER:
	case MATCHCRITERIA_AGE_LOWER:
	case MATCHCRITERIA_SCORE_GREATER:
	case MATCHCRITERIA_SCORE_LOWER:
	case MATCHCRITERIA_SCORE_EQUAL:
	case MATCHCRITERIA_SIZE_GREATER:
	case MATCHCRITERIA_SIZE_SMALLER:
	case MATCHCRITERIA_SIZE_EQUAL:
	case MATCHCRITERIA_COLORLABEL:
	case MATCHCRITERIA_NOT_COLORLABEL:
		return g_strdup_printf("%s %i", criteria_str, matcher->value);
	case MATCHCRITERIA_ALL:
	case MATCHCRITERIA_UNREAD:
	case MATCHCRITERIA_NOT_UNREAD:
	case MATCHCRITERIA_NEW:
	case MATCHCRITERIA_NOT_NEW:
	case MATCHCRITERIA_MARKED:
	case MATCHCRITERIA_NOT_MARKED:
	case MATCHCRITERIA_DELETED:
	case MATCHCRITERIA_NOT_DELETED:
	case MATCHCRITERIA_REPLIED:
	case MATCHCRITERIA_NOT_REPLIED:
	case MATCHCRITERIA_FORWARDED:
	case MATCHCRITERIA_NOT_FORWARDED:
	case MATCHCRITERIA_LOCKED:
	case MATCHCRITERIA_NOT_LOCKED:
	case MATCHCRITERIA_SPAM:
	case MATCHCRITERIA_NOT_SPAM:
	case MATCHCRITERIA_HAS_ATTACHMENT:
	case MATCHCRITERIA_HAS_NO_ATTACHMENT:
	case MATCHCRITERIA_SIGNED:
	case MATCHCRITERIA_NOT_SIGNED:
	case MATCHCRITERIA_PARTIAL:
	case MATCHCRITERIA_NOT_PARTIAL:
	case MATCHCRITERIA_IGNORE_THREAD:
	case MATCHCRITERIA_NOT_IGNORE_THREAD:
	case MATCHCRITERIA_WATCH_THREAD:
	case MATCHCRITERIA_NOT_WATCH_THREAD:
	case MATCHCRITERIA_TAGGED:
	case MATCHCRITERIA_NOT_TAGGED:
		return g_strdup(criteria_str);
	case MATCHCRITERIA_TEST:
	case MATCHCRITERIA_NOT_TEST:
		quoted_expr = matcher_quote_str(matcher->expr);
		matcher_str = g_strdup_printf("%s \"%s\"",
					      criteria_str, quoted_expr);
		g_free(quoted_expr);
                return matcher_str;
	case MATCHCRITERIA_FOUND_IN_ADDRESSBOOK:
	case MATCHCRITERIA_NOT_FOUND_IN_ADDRESSBOOK:
		quoted_header = matcher_quote_str(matcher->header);
		quoted_expr = matcher_quote_str(matcher->expr);
		matcher_str = g_strdup_printf("%s \"%s\" in \"%s\"",
					      criteria_str, quoted_header, quoted_expr);
		g_free(quoted_header);
		g_free(quoted_expr);
		return matcher_str;
	}

	matchtype_str = NULL;
	for (i = 0; i < sizeof matchparser_tab / sizeof matchparser_tab[0]; i++) {
		if (matchparser_tab[i].id == matcher->matchtype)
			matchtype_str = matchparser_tab[i].str;
	}

	if (matchtype_str == NULL)
		return NULL;

	switch (matcher->matchtype) {
	case MATCHTYPE_MATCH:
	case MATCHTYPE_MATCHCASE:
	case MATCHTYPE_REGEXP:
	case MATCHTYPE_REGEXPCASE:
		quoted_expr = matcher_quote_str(matcher->expr);
		if (matcher->header) {
			quoted_header = matcher_quote_str(matcher->header);
			matcher_str = g_strdup_printf
					("%s \"%s\" %s \"%s\"",
					 criteria_str, quoted_header,
					 matchtype_str, quoted_expr);
			g_free(quoted_header);
		}
		else
			matcher_str = g_strdup_printf
					("%s %s \"%s\"", criteria_str,
					 matchtype_str, quoted_expr);
                g_free(quoted_expr);
		break;
	}

	return matcher_str;
}

/*!
 *\brief	Convert a list of conditions to a string
 *
 *\param	matchers List of conditions
 *
 *\return	gchar * Newly allocated string
 */
gchar *matcherlist_to_string(const MatcherList *matchers)
{
	gint count;
	gchar **vstr;
	GSList *l;
	gchar **cur_str;
	gchar *result = NULL;

	count = g_slist_length(matchers->matchers);
	vstr = g_new(gchar *, count + 1);

	for (l = matchers->matchers, cur_str = vstr; l != NULL;
	     l = g_slist_next(l), cur_str ++) {
		*cur_str = matcherprop_to_string((MatcherProp *) l->data);
		if (*cur_str == NULL)
			break;
	}
	*cur_str = NULL;
	
	if (matchers->bool_and)
		result = g_strjoinv(" & ", vstr);
	else
		result = g_strjoinv(" | ", vstr);

	for (cur_str = vstr ; *cur_str != NULL ; cur_str ++)
		g_free(*cur_str);
	g_free(vstr);

	return result;
}


#define STRLEN_ZERO(s) ((s) ? strlen(s) : 0)
#define STRLEN_DEFAULT(s,d) ((s) ? strlen(s) : STRLEN_ZERO(d))

static void add_str_default(gchar ** dest,
			    const gchar * s, const gchar * d)
{
	gchar quoted_str[4096];
	const gchar * str;
	
        if (s != NULL)
		str = s;
	else
		str = d;
	
	quote_cmd_argument(quoted_str, sizeof(quoted_str), str);
	strcpy(* dest, quoted_str);
	
	(* dest) += strlen(* dest);
}

/* matching_build_command() - preferably cmd should be unescaped */
/*!
 *\brief	Build the command-line to execute
 *
 *\param	cmd String with command-line specifiers
 *\param	info Message info to use for command
 *
 *\return	gchar * Newly allocated string
 */
gchar *matching_build_command(const gchar *cmd, MsgInfo *info)
{
	const gchar *s = cmd;
	gchar *filename = NULL;
	gchar *processed_cmd;
	gchar *p;
	gint size;

	const gchar *const no_subject    = _("(none)") ;
	const gchar *const no_from       = _("(none)") ;
	const gchar *const no_to         = _("(none)") ;
	const gchar *const no_cc         = _("(none)") ;
	const gchar *const no_date       = _("(none)") ;
	const gchar *const no_msgid      = _("(none)") ;
	const gchar *const no_newsgroups = _("(none)") ;
	const gchar *const no_references = _("(none)") ;

	size = STRLEN_ZERO(cmd) + 1;
	while (*s != '\0') {
		if (*s == '%') {
			s++;
			switch (*s) {
			case '%':
				size -= 1;
				break;
			case 's': /* subject */
				size += STRLEN_DEFAULT(info->subject, no_subject) - 2;
				break;
			case 'f': /* from */
				size += STRLEN_DEFAULT(info->from, no_from) - 2;
				break;
			case 't': /* to */
				size += STRLEN_DEFAULT(info->to, no_to) - 2;
				break;
			case 'c': /* cc */
				size += STRLEN_DEFAULT(info->cc, no_cc) - 2;
				break;
			case 'd': /* date */
				size += STRLEN_DEFAULT(info->date, no_date) - 2;
				break;
			case 'i': /* message-id */
				size += STRLEN_DEFAULT(info->msgid, no_msgid) - 2;
				break;
			case 'n': /* newsgroups */
				size += STRLEN_DEFAULT(info->newsgroups, no_newsgroups) - 2;
				break;
			case 'r': /* references */
                                /* FIXME: using the inreplyto header for reference */
				size += STRLEN_DEFAULT(info->inreplyto, no_references) - 2;
				break;
			case 'F': /* file */
				if (filename == NULL)
					filename = folder_item_fetch_msg(info->folder, info->msgnum);
				
				if (filename == NULL) {
					g_warning("filename is not set");
					return NULL;
				}
				else {
					size += strlen(filename) - 2;
				}
				break;
			}
			s++;
		}
		else s++;
	}
	
	/* as the string can be quoted, we double the result */
	size *= 2;

	processed_cmd = g_new0(gchar, size);
	s = cmd;
	p = processed_cmd;

	while (*s != '\0') {
		if (*s == '%') {
			s++;
			switch (*s) {
			case '%':
				*p = '%';
				p++;
				break;
			case 's': /* subject */
				add_str_default(&p, info->subject,
						no_subject);
				break;
			case 'f': /* from */
				add_str_default(&p, info->from,
						no_from);
				break;
			case 't': /* to */
				add_str_default(&p, info->to,
						no_to);
				break;
			case 'c': /* cc */
				add_str_default(&p, info->cc,
						no_cc);
				break;
			case 'd': /* date */
				add_str_default(&p, info->date,
						no_date);
				break;
			case 'i': /* message-id */
				add_str_default(&p, info->msgid,
						no_msgid);
				break;
			case 'n': /* newsgroups */
				add_str_default(&p, info->newsgroups,
						no_newsgroups);
				break;
			case 'r': /* references */
                                /* FIXME: using the inreplyto header for references */
				add_str_default(&p, info->inreplyto, no_references);
				break;
			case 'F': /* file */
				if (filename != NULL)
					add_str_default(&p, filename, NULL);
				break;
			default:
				*p = '%';
				p++;
				*p = *s;
				p++;
				break;
			}
			s++;
		}
		else {
			*p = *s;
			p++;
			s++;
		}
	}
	g_free(filename);
	
	return processed_cmd;
}
#undef STRLEN_DEFAULT
#undef STRLEN_ZERO

/* ************************************************************ */


/*!
 *\brief	Write filtering list to file
 *
 *\param	fp File
 *\param	prefs_filtering List of filtering conditions
 */
static int prefs_filtering_write(FILE *fp, GSList *prefs_filtering)
{
	GSList *cur = NULL;

	for (cur = prefs_filtering; cur != NULL; cur = cur->next) {
		gchar *filtering_str = NULL;
		gchar *tmp_name = NULL;
		FilteringProp *prop = NULL;

		if (NULL == (prop = (FilteringProp *) cur->data))
			continue;
		
		if (NULL == (filtering_str = filteringprop_to_string(prop)))
			continue;

		if (prop->enabled) {
			if (fputs("enabled ", fp) == EOF) {
				FILE_OP_ERROR("filtering config", "fputs");
				return -1;
			}
		} else {
			if (fputs("disabled ", fp) == EOF) {
				FILE_OP_ERROR("filtering config", "fputs");
				return -1;
			}
		}

		if (fputs("rulename \"", fp) == EOF) {
			FILE_OP_ERROR("filtering config", "fputs");
			g_free(filtering_str);
			return -1;
		}
		tmp_name = prop->name;
		while (tmp_name && *tmp_name != '\0') {
			if (*tmp_name != '"') {
				if (fputc(*tmp_name, fp) == EOF) {
					FILE_OP_ERROR("filtering config", "fputs || fputc");
					g_free(filtering_str);
					return -1;
				}
			} else if (*tmp_name == '"') {
				if (fputc('\\', fp) == EOF ||
				    fputc('"', fp) == EOF) {
					FILE_OP_ERROR("filtering config", "fputs || fputc");
					g_free(filtering_str);
					return -1;
				}
			}
			tmp_name ++;
		}
		if (fputs("\" ", fp) == EOF) {
			FILE_OP_ERROR("filtering config", "fputs");
			g_free(filtering_str);
			return -1;
		}

		if (prop->account_id != 0) {
			gchar *tmp = NULL;

			tmp = g_strdup_printf("account %d ", prop->account_id);
			if (fputs(tmp, fp) == EOF) {
				FILE_OP_ERROR("filtering config", "fputs");
				g_free(tmp);
				return -1;
			}
			g_free(tmp);
		}

		if(fputs(filtering_str, fp) == EOF ||
		    fputc('\n', fp) == EOF) {
			FILE_OP_ERROR("filtering config", "fputs || fputc");
			g_free(filtering_str);
			return -1;
		}
		g_free(filtering_str);
	}
	
	return 0;
}

typedef struct _NodeLoopData {
	FILE *fp;
	gboolean error;
} NodeLoopData;

/*!
 *\brief	Write matchers from a folder item
 *
 *\param	node Node with folder info
 *\param	data File pointer
 *
 *\return	gboolean FALSE
 */
static gboolean prefs_matcher_write_func(GNode *node, gpointer d)
{
	FolderItem *item;
	NodeLoopData *data = (NodeLoopData *)d;
	gchar *id;
	GSList *prefs_filtering;

        item = node->data;
        /* prevent warning */
        if (item->path == NULL)
                return FALSE;
        id = folder_item_get_identifier(item);
        if (id == NULL)
                return FALSE;
        prefs_filtering = item->prefs->processing;

	if (prefs_filtering != NULL) {
		if (fprintf(data->fp, "[%s]\n", id) < 0) {
			data->error = TRUE;
			goto fail;
		}
		if (prefs_filtering_write(data->fp, prefs_filtering) < 0) {
			data->error = TRUE;
			goto fail;
		}
		if (fputc('\n', data->fp) == EOF) {
			data->error = TRUE;
			goto fail;
		}
	}
fail:
	g_free(id);

	return FALSE;
}

/*!
 *\brief	Save matchers from folder items
 *
 *\param	fp File
 */
static int prefs_matcher_save(FILE *fp)
{
	GList *cur;
	NodeLoopData data;
	
	data.fp = fp;
	data.error = FALSE;

	for (cur = folder_get_list() ; cur != NULL ; cur = g_list_next(cur)) {
		Folder *folder;

		folder = (Folder *) cur->data;
		g_node_traverse(folder->node, G_PRE_ORDER, G_TRAVERSE_ALL, -1,
				prefs_matcher_write_func, &data);
	}
        
	if (data.error == TRUE)
		return -1;

        /* pre global rules */
        if (fprintf(fp, "[preglobal]\n") < 0 ||
            prefs_filtering_write(fp, pre_global_processing) < 0 ||
            fputc('\n', fp) == EOF)
		return -1;

        /* post global rules */
        if (fprintf(fp, "[postglobal]\n") < 0 ||
            prefs_filtering_write(fp, post_global_processing) < 0 ||
            fputc('\n', fp) == EOF)
		return -1;
        
        /* filtering rules */
	if (fprintf(fp, "[filtering]\n") < 0 ||
            prefs_filtering_write(fp, filtering_rules) < 0 ||
            fputc('\n', fp) == EOF)
		return -1;

	return 0;
}

/*!
 *\brief	Write filtering / matcher configuration file
 */
void prefs_matcher_write_config(void)
{
	gchar *rcpath;
	PrefFile *pfile;

	debug_print("Writing matcher configuration...\n");

	rcpath = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S,
			     MATCHER_RC, NULL);

	if ((pfile = prefs_write_open(rcpath)) == NULL) {
		g_warning("failed to write configuration to file\n");
		g_free(rcpath);
		return;
	}

	g_free(rcpath);

	if (prefs_matcher_save(pfile->fp) < 0) {
		g_warning("failed to write configuration to file\n");
		prefs_file_close_revert(pfile);
	} else if (prefs_file_close(pfile) < 0) {
		g_warning("failed to save configuration to file\n");
	}
}

/* ******************************************************************* */

static void matcher_add_rulenames(const gchar *rcpath)
{
	gchar *newpath = g_strconcat(rcpath, ".new", NULL);
	FILE *src = g_fopen(rcpath, "rb");
	FILE *dst = g_fopen(newpath, "wb");
	gchar buf[BUFFSIZE];
	int r;
	if (src == NULL) {
		perror("fopen");
		if (dst)
			fclose(dst);
		g_free(newpath);
		return;
	}
	if (dst == NULL) {
		perror("fopen");
		if (src)
			fclose(src);
		g_free(newpath);
		return;
	}

	while (fgets (buf, sizeof(buf), src) != NULL) {
		if (strlen(buf) > 2 && buf[0] != '['
		&& strncmp(buf, "rulename \"", 10)
		&& strncmp(buf, "enabled rulename \"", 18)
		&& strncmp(buf, "disabled rulename \"", 18)) {
			r = fwrite("enabled rulename \"\" ",
				strlen("enabled rulename \"\" "), 1, dst);
			if (r != 1) {
				g_message("cannot fwrite rulename\n");
			}
		}
		r = fwrite(buf, strlen(buf), 1, dst);
		if (r != 1) {
			g_message("cannot fwrite rule\n");
		}
	}
	fclose(dst);
	fclose(src);
	move_file(newpath, rcpath, TRUE);
	g_free(newpath);
}

/*!
 *\brief	Read matcher configuration
 */
void prefs_matcher_read_config(void)
{
	gchar *rcpath;
	gchar *rc_old_format;
	FILE *f;

	create_matchparser_hashtab();
	prefs_filtering_clear();

	rcpath = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S, MATCHER_RC, NULL);
	rc_old_format = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S, MATCHER_RC, 
				".pre_names", NULL);
	
	if (!is_file_exist(rc_old_format) && is_file_exist(rcpath)) {
		/* backup file with no rules names, in case 
		 * anything goes wrong */
		copy_file(rcpath, rc_old_format, FALSE);
		/* now hack the file in order to have it to the new format */
		matcher_add_rulenames(rcpath);
	}
	
	g_free(rc_old_format);

	f = g_fopen(rcpath, "rb");
	g_free(rcpath);

	if (f != NULL) {
		matcher_parser_start_parsing(f);
		fclose(matcher_parserin);
	}
	else {
		/* previous version compatibility */

		/* g_print("reading filtering\n"); */
		rcpath = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S,
				     FILTERING_RC, NULL);
		f = g_fopen(rcpath, "rb");
		g_free(rcpath);
		
		if (f != NULL) {
			matcher_parser_start_parsing(f);
			fclose(matcher_parserin);
		}
		
		/* g_print("reading scoring\n"); */
		rcpath = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S,
				     SCORING_RC, NULL);
		f = g_fopen(rcpath, "rb");
		g_free(rcpath);
		
		if (f != NULL) {
			matcher_parser_start_parsing(f);
			fclose(matcher_parserin);
		}
	}
}
