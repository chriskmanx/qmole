/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto & The Claws Mail Team
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
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <gtk/gtk.h>
#include <stdio.h>

#include "utils.h"
#include "procheader.h"
#include "matcher.h"
#include "filtering.h"
#include "prefs_gtk.h"
#include "compose.h"
#include "prefs_common.h"
#include "addritem.h"
#ifndef USE_NEW_ADDRBOOK
	#include "addrbook.h"
	#include "addressbook.h"
#else
	#include "addressbook-dbus.h"
	#include "addressadd.h"
#endif
#include "addr_compl.h"
#include "tags.h"
#include "log.h"
#include "account.h"
#include "addrindex.h"
#include "folder_item_prefs.h"

GSList * pre_global_processing = NULL;
GSList * post_global_processing = NULL;
GSList * filtering_rules = NULL;

gboolean debug_filtering_session = FALSE;

static gboolean filtering_is_final_action(FilteringAction *filtering_action);

#define STRLEN_WITH_CHECK(expr) \
        strlen_with_check(#expr, __LINE__, expr)
	        
static inline gint strlen_with_check(const gchar *expr, gint fline, const gchar *str)
{
        if (str) 
		return strlen(str);
	else {
	        debug_print("%s(%d) - invalid string %s\n", __FILE__, fline, expr?expr:"(null)");
	        return 0;
	}
}

FilteringAction * filteringaction_new(int type, int account_id,
				      gchar * destination,
				      gint labelcolor, gint score, gchar * header)
{
	FilteringAction * action;

	action = g_new0(FilteringAction, 1);

	action->type = type;
	action->account_id = account_id;
	if (destination) {
		action->destination	  = g_strdup(destination);
	} else {
		action->destination       = NULL;
	}
	if (header) {
		action->header	  = g_strdup(header);
	} else {
		action->header       = NULL;
	}
	action->labelcolor = labelcolor;	
        action->score = score;
	return action;
}

void filteringaction_free(FilteringAction * action)
{
	cm_return_if_fail(action);
	g_free(action->header);
	g_free(action->destination);
	g_free(action);
}

static gint action_list_sort(gconstpointer a, gconstpointer b)
{
	int first  = filtering_is_final_action((FilteringAction *) a) ? 1 : 0;
	int second = filtering_is_final_action((FilteringAction *) b) ? 1 : 0;
	
	return (first - second);
}

GSList *filtering_action_list_sort(GSList *action_list)
{
	return g_slist_sort(action_list, action_list_sort);
}

FilteringProp * filteringprop_new(gboolean enabled,
				  const gchar *name,
				  gint account_id,
				  MatcherList * matchers,
				  GSList * action_list)
{
	FilteringProp * filtering;

	filtering = g_new0(FilteringProp, 1);
	filtering->enabled = enabled;
	filtering->name = name ? g_strdup(name): NULL;
	filtering->account_id = account_id;
	filtering->matchers = matchers;
	filtering->action_list = filtering_action_list_sort(action_list);

	return filtering;
}

static FilteringAction * filteringaction_copy(FilteringAction * src)
{
        FilteringAction * new;
        
        new = g_new0(FilteringAction, 1);
        
	new->type = src->type;
	new->account_id = src->account_id;
	if (src->destination)
		new->destination = g_strdup(src->destination);
	else 
		new->destination = NULL;
	new->labelcolor = src->labelcolor;
	new->score = src->score;

        return new;
}

FilteringProp * filteringprop_copy(FilteringProp *src)
{
	FilteringProp * new;
	GSList *tmp;
	
	new = g_new0(FilteringProp, 1);
	new->matchers = g_new0(MatcherList, 1);

	for (tmp = src->matchers->matchers; tmp != NULL && tmp->data != NULL;) {
		MatcherProp *matcher = (MatcherProp *)tmp->data;
		
		new->matchers->matchers = g_slist_append(new->matchers->matchers,
						   matcherprop_copy(matcher));
		tmp = tmp->next;
	}

	new->matchers->bool_and = src->matchers->bool_and;

        new->action_list = NULL;

        for (tmp = src->action_list ; tmp != NULL ; tmp = tmp->next) {
                FilteringAction *filtering_action;
                
                filtering_action = tmp->data;
                
                new->action_list = g_slist_append(new->action_list,
                    filteringaction_copy(filtering_action));
        }

	new->enabled = src->enabled;
	new->name = g_strdup(src->name);

	return new;
}

void filteringprop_free(FilteringProp * prop)
{
        GSList * tmp;

	cm_return_if_fail(prop);
	matcherlist_free(prop->matchers);
        
        for (tmp = prop->action_list ; tmp != NULL ; tmp = tmp->next) {
                filteringaction_free(tmp->data);
        }
	g_slist_free(prop->action_list);
	g_free(prop->name);
	g_free(prop);
}

/* move and copy messages by batches to be faster on IMAP */
void filtering_move_and_copy_msgs(GSList *msgs)
{
	GSList *messages = g_slist_copy(msgs);
	FolderItem *last_item = NULL;
	FiltOp cur_op = IS_NOTHING;

	debug_print("checking %d messages\n", g_slist_length(msgs));
	while (messages) {
		GSList *batch = NULL, *cur;
		gint found = 0;
		for (cur = messages; cur; cur = cur->next) {
			MsgInfo *info = (MsgInfo *)cur->data;
			if (last_item == NULL) {
				if (info->filter_op == IS_COPY || info->filter_op == IS_MOVE)
					last_item = info->to_filter_folder;
				else if (info->filter_op == IS_DELE)
					last_item = info->folder;
			}
			if (last_item == NULL)
				continue;
			if (cur_op == IS_NOTHING) {
				if (info->filter_op == IS_COPY)
					cur_op = IS_COPY;
				else if (info->filter_op == IS_MOVE)
					cur_op = IS_MOVE;
				else if (info->filter_op == IS_DELE)
					cur_op = IS_DELE;
			}
			if (info->filter_op == IS_COPY || info->filter_op == IS_MOVE) {
				if (info->to_filter_folder == last_item 
				&&  cur_op == info->filter_op) {
					found++;
					batch = g_slist_prepend(batch, info);
				}
			} else if (info->filter_op == IS_DELE) {
				if (info->folder == last_item 
				&&  cur_op == info->filter_op) {
					found++;
					batch = g_slist_prepend(batch, info);
				}
			}
		}
		if (found == 0) {
			debug_print("no more messages to move/copy/del\n");
			break;
		} else {
			debug_print("%d messages to %s in %s\n", found,
				cur_op==IS_COPY ? "copy":(cur_op==IS_DELE ?"delete":"move"), 
				last_item?(last_item->name ? last_item->name:"(noname)"):"nowhere");
		}
		for (cur = batch; cur; cur = cur->next) {
			MsgInfo *info = (MsgInfo *)cur->data;
			messages = g_slist_remove(messages, info);
			info->to_filter_folder = NULL;
			info->filter_op = IS_NOTHING;
		}
		batch = g_slist_reverse(batch);
		if (g_slist_length(batch)) {
			MsgInfo *info = (MsgInfo *)batch->data;
			if (cur_op == IS_COPY && last_item != info->folder) {
				folder_item_copy_msgs(last_item, batch);
			} else if (cur_op == IS_MOVE && last_item != info->folder) {
				if (folder_item_move_msgs(last_item, batch) < 0)
					folder_item_move_msgs(
						folder_get_default_inbox(), 
						batch);
			} else if (cur_op == IS_DELE && last_item == info->folder) {
				folder_item_remove_msgs(last_item, batch);
			}
			/* we don't reference the msginfos, because caller will do */
			if (prefs_common.real_time_sync)
				folder_item_synchronise(last_item);
			g_slist_free(batch);
			batch = NULL;
			GTK_EVENTS_FLUSH();
		}
		last_item = NULL;
		cur_op = IS_NOTHING;
	}
	/* we don't reference the msginfos, because caller will do */
	g_slist_free(messages);
}

/*
  fitleringaction_apply
  runs the action on one MsgInfo
  return value : return TRUE if the action could be applied
*/

#define FLUSH_COPY_IF_NEEDED(info) {					\
	if (info->filter_op == IS_COPY && info->to_filter_folder) {	\
		debug_print("must debatch pending copy\n");		\
		folder_item_copy_msg(info->to_filter_folder, info);	\
		info->filter_op = IS_NOTHING;				\
	}								\
}

static gboolean filteringaction_apply(FilteringAction * action, MsgInfo * info)
{
	FolderItem * dest_folder;
	gint val;
	Compose * compose;
	PrefsAccount * account;
	gchar * cmd;

	switch(action->type) {
	case MATCHACTION_MOVE:
		if (MSG_IS_LOCKED(info->flags))
			return FALSE;
			
		dest_folder =
			folder_find_item_from_identifier(action->destination);
		if (!dest_folder) {
			debug_print("*** folder not found '%s'\n",
				action->destination ?action->destination :"(null)");
			return FALSE;
		}
		
		FLUSH_COPY_IF_NEEDED(info);
		/* mark message to be moved */		
		info->filter_op = IS_MOVE;
		info->to_filter_folder = dest_folder;
		return TRUE;

	case MATCHACTION_COPY:
		dest_folder =
			folder_find_item_from_identifier(action->destination);

		if (!dest_folder) {
			debug_print("*** folder not found '%s'\n",
				action->destination ?action->destination :"(null)");
			return FALSE;
		}

		FLUSH_COPY_IF_NEEDED(info);
		/* mark message to be copied */		
		info->filter_op = IS_COPY;
		info->to_filter_folder = dest_folder;
		return TRUE;

	case MATCHACTION_SET_TAG:
	case MATCHACTION_UNSET_TAG:
		val = tags_get_id_for_str(action->destination);
		if (val == -1) {
			debug_print("*** tag '%s' not found\n",
				action->destination ?action->destination :"(null)");
			return FALSE;
		}
		FLUSH_COPY_IF_NEEDED(info);
		procmsg_msginfo_update_tags(info, (action->type == MATCHACTION_SET_TAG), val);
		return TRUE;

	case MATCHACTION_CLEAR_TAGS:
		FLUSH_COPY_IF_NEEDED(info);
		procmsg_msginfo_clear_tags(info);
		return TRUE;

	case MATCHACTION_DELETE:
		FLUSH_COPY_IF_NEEDED(info);
		info->filter_op = IS_DELE;
		return TRUE;

	case MATCHACTION_MARK:
		FLUSH_COPY_IF_NEEDED(info);
		procmsg_msginfo_set_flags(info, MSG_MARKED, 0);
		return TRUE;

	case MATCHACTION_UNMARK:
		FLUSH_COPY_IF_NEEDED(info);
		procmsg_msginfo_unset_flags(info, MSG_MARKED, 0);
		return TRUE;

	case MATCHACTION_LOCK:
		FLUSH_COPY_IF_NEEDED(info);
		procmsg_msginfo_set_flags(info, MSG_LOCKED, 0);
		return TRUE;

	case MATCHACTION_UNLOCK:
		FLUSH_COPY_IF_NEEDED(info);
		procmsg_msginfo_unset_flags(info, MSG_LOCKED, 0);	
		return TRUE;
		
	case MATCHACTION_MARK_AS_READ:
		FLUSH_COPY_IF_NEEDED(info);
		procmsg_msginfo_unset_flags(info, MSG_UNREAD | MSG_NEW, 0);
		return TRUE;

	case MATCHACTION_MARK_AS_UNREAD:
		FLUSH_COPY_IF_NEEDED(info);
		procmsg_msginfo_set_flags(info, MSG_UNREAD, 0);
		return TRUE;
	
	case MATCHACTION_MARK_AS_SPAM:
		FLUSH_COPY_IF_NEEDED(info);
		procmsg_spam_learner_learn(info, NULL, TRUE);
		procmsg_msginfo_change_flags(info, MSG_SPAM, 0, MSG_NEW|MSG_UNREAD, 0);
		if (procmsg_spam_get_folder(info)) {
			info->filter_op = IS_MOVE;
			info->to_filter_folder = procmsg_spam_get_folder(info);
		}
		return TRUE;

	case MATCHACTION_MARK_AS_HAM:
		FLUSH_COPY_IF_NEEDED(info);
		procmsg_spam_learner_learn(info, NULL, FALSE);
		procmsg_msginfo_unset_flags(info, MSG_SPAM, 0);
		return TRUE;
	
	case MATCHACTION_COLOR:
		FLUSH_COPY_IF_NEEDED(info);
		procmsg_msginfo_unset_flags(info, MSG_CLABEL_FLAG_MASK, 0); 
		procmsg_msginfo_set_flags(info, MSG_COLORLABEL_TO_FLAGS(action->labelcolor), 0);
		return TRUE;

	case MATCHACTION_FORWARD:
	case MATCHACTION_FORWARD_AS_ATTACHMENT:
		account = account_find_from_id(action->account_id);
		compose = compose_forward(account, info,
			action->type == MATCHACTION_FORWARD ? FALSE : TRUE,
			NULL, TRUE, TRUE);
		compose_entry_append(compose, action->destination,
				     compose->account->protocol == A_NNTP
					    ? COMPOSE_NEWSGROUPS
					    : COMPOSE_TO, PREF_NONE);

		val = compose_send(compose);

		return val == 0 ? TRUE : FALSE;

	case MATCHACTION_REDIRECT:
		account = account_find_from_id(action->account_id);
		compose = compose_redirect(account, info, TRUE);
		if (compose->account->protocol == A_NNTP)
			break;
		else
			compose_entry_append(compose, action->destination,
					     COMPOSE_TO, PREF_NONE);

		val = compose_send(compose);
		
		return val == 0 ? TRUE : FALSE;

	case MATCHACTION_EXECUTE:
		cmd = matching_build_command(action->destination, info);
		if (cmd == NULL)
			return FALSE;
		else {
			if (system(cmd) == -1)
				g_warning("couldn't run %s", cmd);
			g_free(cmd);
		}
		return TRUE;

	case MATCHACTION_SET_SCORE:
		FLUSH_COPY_IF_NEEDED(info);
		info->score = action->score;
		return TRUE;

	case MATCHACTION_CHANGE_SCORE:
		FLUSH_COPY_IF_NEEDED(info);
		info->score += action->score;
		return TRUE;

	case MATCHACTION_STOP:
                return FALSE;

	case MATCHACTION_HIDE:
		FLUSH_COPY_IF_NEEDED(info);
                info->hidden = TRUE;
                return TRUE;

	case MATCHACTION_IGNORE:
		FLUSH_COPY_IF_NEEDED(info);
                procmsg_msginfo_set_flags(info, MSG_IGNORE_THREAD, 0);
                return TRUE;

	case MATCHACTION_WATCH:
		FLUSH_COPY_IF_NEEDED(info);
                procmsg_msginfo_set_flags(info, MSG_WATCH_THREAD, 0);
                return TRUE;

	case MATCHACTION_ADD_TO_ADDRESSBOOK:
		{
#ifndef USE_NEW_ADDRBOOK
			AddressDataSource *book = NULL;
			AddressBookFile *abf = NULL;
			ItemFolder *folder = NULL;
#endif
			gchar buf[BUFFSIZE];
			Header *header;
			gint errors = 0;

#ifndef USE_NEW_ADDRBOOK
			if (!addressbook_peek_folder_exists(action->destination, &book, &folder)) {
				g_warning("addressbook folder not found '%s'\n", action->destination?action->destination:"(null)");
				return FALSE;
			}
			if (!book) {
				g_warning("addressbook_peek_folder_exists returned NULL book\n");
				return FALSE;
			}

			abf = book->rawDataSource;
#endif
			/* get the header */
			procheader_get_header_from_msginfo(info, buf, sizeof(buf), action->header);
			header = procheader_parse_header(buf);

			/* add all addresses that are not already in */
			if (header && *header->body && (*header->body != '\0')) {
				GSList *address_list = NULL;
				GSList *walk = NULL;
				gchar *path = NULL;

				if (action->destination == NULL ||
						strcasecmp(action->destination, "Any") == 0 ||
						*(action->destination) == '\0')
					path = NULL;
				else
					path = action->destination;
				start_address_completion(path);

				address_list = address_list_append(address_list, header->body);
				for (walk = address_list; walk != NULL; walk = walk->next) {
					gchar *stripped_addr = g_strdup(walk->data);
					extract_address(stripped_addr);

					if (complete_matches_found(walk->data) == 0) {
						debug_print("adding address '%s' to addressbook '%s'\n",
								stripped_addr, action->destination);
#ifndef USE_NEW_ADDRBOOK
						if (!addrbook_add_contact(abf, folder, stripped_addr, stripped_addr, NULL)) {
#else
						if (!addressadd_selection(NULL, stripped_addr, NULL, NULL)) {
#endif
							g_warning("contact could not been added\n");
							errors++;
						}
					} else {
						debug_print("address '%s' already found in addressbook '%s', skipping\n",
								stripped_addr, action->destination);
					}
					g_free(stripped_addr);
				}

				g_slist_free(address_list);
				end_address_completion();
			} else {
				g_warning("header '%s' not set or empty\n", action->header?action->header:"(null)");
			}
			return (errors == 0);
		}
	default:
		break;
	}
	return FALSE;
}

gboolean filteringaction_apply_action_list(GSList *action_list, MsgInfo *info)
{
	GSList *p;
	cm_return_val_if_fail(action_list, FALSE);
	cm_return_val_if_fail(info, FALSE);
	for (p = action_list; p && p->data; p = g_slist_next(p)) {
		FilteringAction *a = (FilteringAction *) p->data;
		if (filteringaction_apply(a, info)) {
			if (filtering_is_final_action(a))
				break;
		} else
			return FALSE;
		
	}
	return TRUE;
}

static gboolean filtering_match_condition(FilteringProp *filtering, MsgInfo *info,
							PrefsAccount *ac_prefs)

/* this function returns true if a filtering rule applies regarding to its account
   data and if it does, if the conditions list match.

   per-account data of a filtering rule is either matched against current account
   when filtering is done manually, or against the account currently used for
   retrieving messages when it's an manual or automatic fetching of messages.
   per-account data match doesn't apply to pre-/post-/folder-processing rules.

   when filtering messages manually:
    - either the filtering rule is not account-based and it will be processed
	- or it's per-account and we check if we HAVE TO match it against the current
	  account (according to user-land settings, per-account rules might have to
	  be skipped, or only the rules that match the current account have to be
	  applied, or all rules will have to be applied regardless to if they are
	  account-based or not)

   notes about debugging output in that function:
   when not matching, log_status_skip() is used, otherwise log_status_ok() is used
   no debug output is done when filtering_debug_level is low
*/
{
	gboolean matches = FALSE;

	if (ac_prefs != NULL) {
		matches = ((filtering->account_id == 0)
					|| (filtering->account_id == ac_prefs->account_id));

		/* debug output */
		if (debug_filtering_session) {
			if (matches && prefs_common.filtering_debug_level >= FILTERING_DEBUG_LEVEL_HIGH) {
				if (filtering->account_id == 0) {
					log_status_ok(LOG_DEBUG_FILTERING,
							_("rule is not account-based\n"));
				} else {
					if (prefs_common.filtering_debug_level >= FILTERING_DEBUG_LEVEL_MED) {
						log_status_ok(LOG_DEBUG_FILTERING,
								_("rule is account-based [id=%d, name='%s'], "
								"matching the account currently used to retrieve messages\n"),
								ac_prefs->account_id, ac_prefs?ac_prefs->account_name:_("NON_EXISTENT"));
					}
				}
			}
			else
			if (!matches) {
				if (prefs_common.filtering_debug_level >= FILTERING_DEBUG_LEVEL_MED) {
					log_status_skip(LOG_DEBUG_FILTERING,
							_("rule is account-based, "
							"not matching the account currently used to retrieve messages\n"));
				} else {
					if (prefs_common.filtering_debug_level >= FILTERING_DEBUG_LEVEL_HIGH) {
						PrefsAccount *account = account_find_from_id(filtering->account_id);

						log_status_skip(LOG_DEBUG_FILTERING,
								_("rule is account-based [id=%d, name='%s'], "
								"not matching the account currently used to retrieve messages [id=%d, name='%s']\n"),
								filtering->account_id, account?account->account_name:_("NON_EXISTENT"),
								ac_prefs->account_id, ac_prefs?ac_prefs->account_name:_("NON_EXISTENT"));
					}
				}
			}
		}
	} else {
		switch (prefs_common.apply_per_account_filtering_rules) {
		case FILTERING_ACCOUNT_RULES_FORCE:
			/* apply filtering rules regardless to the account info */
			matches = TRUE;

			/* debug output */
			if (debug_filtering_session) {
				if (prefs_common.filtering_debug_level >= FILTERING_DEBUG_LEVEL_HIGH) {
					if (filtering->account_id == 0) {
						log_status_ok(LOG_DEBUG_FILTERING,
								_("rule is not account-based, "
								"all rules are applied on user request anyway\n"));
					} else {
						PrefsAccount *account = account_find_from_id(filtering->account_id);

						log_status_ok(LOG_DEBUG_FILTERING,
								_("rule is account-based [id=%d, name='%s'], "
								"but all rules are applied on user request\n"),
								filtering->account_id, account?account->account_name:_("NON_EXISTENT"));
					}
				}
			}
			break;
		case FILTERING_ACCOUNT_RULES_SKIP:
			/* don't apply filtering rules that belong to an account */
			matches = (filtering->account_id == 0);

			/* debug output */
			if (debug_filtering_session) {
				if (!matches) {
					if (prefs_common.filtering_debug_level >= FILTERING_DEBUG_LEVEL_HIGH) {
						PrefsAccount *account = account_find_from_id(filtering->account_id);

						log_status_skip(LOG_DEBUG_FILTERING,
								_("rule is account-based [id=%d, name='%s'], "
								"skipped on user request\n"),
								filtering->account_id, account?account->account_name:_("NON_EXISTENT"));
					} else {
						log_status_skip(LOG_DEBUG_FILTERING,
								_("rule is account-based, "
								"skipped on user request\n"));
					}
				} else {
					if (matches && prefs_common.filtering_debug_level >= FILTERING_DEBUG_LEVEL_HIGH) {
						log_status_ok(LOG_DEBUG_FILTERING,
								_("rule is not account-based\n"));
					}
				}
			}
			break;
		case FILTERING_ACCOUNT_RULES_USE_CURRENT:
			matches = ((filtering->account_id == 0)
					|| (filtering->account_id == cur_account->account_id));

			/* debug output */
			if (debug_filtering_session) {
				if (!matches) {
					if (prefs_common.filtering_debug_level >= FILTERING_DEBUG_LEVEL_HIGH) {
						PrefsAccount *account = account_find_from_id(filtering->account_id);

						log_status_skip(LOG_DEBUG_FILTERING,
								_("rule is account-based [id=%d, name='%s'], "
								"not matching current account [id=%d, name='%s']\n"),
								filtering->account_id, account?account->account_name:_("NON_EXISTENT"),
								cur_account->account_id, cur_account?cur_account->account_name:_("NON_EXISTENT"));
					} else {
						log_status_skip(LOG_DEBUG_FILTERING,
								_("rule is account-based, "
								"not matching current account\n"));
					}
				} else {
					if (matches && prefs_common.filtering_debug_level >= FILTERING_DEBUG_LEVEL_HIGH) {
						if (filtering->account_id == 0) {
							log_status_ok(LOG_DEBUG_FILTERING,
									_("rule is not account-based\n"));
						} else {
							PrefsAccount *account = account_find_from_id(filtering->account_id);

							log_status_ok(LOG_DEBUG_FILTERING,
									_("rule is account-based [id=%d, name='%s'], "
									"current account [id=%d, name='%s']\n"),
									account->account_id, account?account->account_name:_("NON_EXISTENT"),
									cur_account->account_id, cur_account?cur_account->account_name:_("NON_EXISTENT"));
						}
					}
				}
			}
			break;
		}
	}

	return matches && matcherlist_match(filtering->matchers, info);
}

/*!
 *\brief	Apply a rule on message.
 *
 *\param	filtering List of filtering rules.
 *\param	info Message to apply rules on.
 *\param	final Variable returning TRUE or FALSE if one of the
 *		encountered actions was final. 
 *		See also \ref filtering_is_final_action.
 *
 *\return	gboolean TRUE to continue applying rules.
 */
static gboolean filtering_apply_rule(FilteringProp *filtering, MsgInfo *info,
    gboolean * final)
{
	gboolean result = TRUE;
	gchar    *buf;
        GSList * tmp;
        
        * final = FALSE;
        for (tmp = filtering->action_list ; tmp != NULL ; tmp = tmp->next) {
                FilteringAction * action;

                action = tmp->data;
		buf = filteringaction_to_string(action);
		if (debug_filtering_session)
			log_print(LOG_DEBUG_FILTERING, _("applying action [ %s ]\n"), buf);

                if (FALSE == (result = filteringaction_apply(action, info))) {
					if (debug_filtering_session) {
						if (action->type != MATCHACTION_STOP)
							log_warning(LOG_DEBUG_FILTERING, _("action could not apply\n"));
						log_print(LOG_DEBUG_FILTERING,
								_("no further processing after action [ %s ]\n"), buf);
					}
 					debug_print("No further processing after rule %s\n", buf);
                }
                g_free(buf);
                if (filtering_is_final_action(action)) {
                        * final = TRUE;
                        break;
                }
		
        }
	return result;
}

/*!
 *\brief	Check if an action is "final", i.e. should break further
 *		processing.
 *
 *\param	filtering_action Action to check.
 *
 *\return	gboolean TRUE if \a filtering_action is final.	
 */
static gboolean filtering_is_final_action(FilteringAction *filtering_action)
{
	switch(filtering_action->type) {
	case MATCHACTION_MOVE:
	case MATCHACTION_DELETE:
	case MATCHACTION_STOP:
	case MATCHACTION_MARK_AS_SPAM:
		return TRUE; /* MsgInfo invalid for message */
	default:
		return FALSE;
	}
}

static gboolean filter_msginfo(GSList * filtering_list, MsgInfo * info, PrefsAccount* ac_prefs)
{
	GSList	*l;
	gboolean final;
	gboolean apply_next;
	
	cm_return_val_if_fail(info != NULL, TRUE);
	
	for (l = filtering_list, final = FALSE, apply_next = FALSE; l != NULL; l = g_slist_next(l)) {
		FilteringProp * filtering = (FilteringProp *) l->data;

		if (filtering->enabled) {
			if (debug_filtering_session) {
				gchar *buf = filteringprop_to_string(filtering);
				if (filtering->name && *filtering->name != '\0') {
					log_print(LOG_DEBUG_FILTERING,
						_("processing rule '%s' [ %s ]\n"),
						filtering->name, buf);
				} else {
					log_print(LOG_DEBUG_FILTERING,
						_("processing rule <unnamed> [ %s ]\n"),
						buf);
				}
				g_free(buf);
			}

			if (filtering_match_condition(filtering, info, ac_prefs)) {
				apply_next = filtering_apply_rule(filtering, info, &final);
				if (final)
					break;
			}

		} else {
			if (debug_filtering_session) {
				gchar *buf = filteringprop_to_string(filtering);
				if (prefs_common.filtering_debug_level >= FILTERING_DEBUG_LEVEL_MED) {
					if (filtering->name && *filtering->name != '\0') {
						log_status_skip(LOG_DEBUG_FILTERING,
								_("disabled rule '%s' [ %s ]\n"),
								filtering->name, buf);
					} else {
						log_status_skip(LOG_DEBUG_FILTERING,
								_("disabled rule <unnamed> [ %s ]\n"),
								buf);
					}
				}
				g_free(buf);
			}
		}
	}

    /* put in inbox if the last rule was not a final one, or
     * a final rule could not be applied.
     * Either of these cases is likely. */
    if (!final || !apply_next) {
		return FALSE;
	}

	return TRUE;
}

/*!
 *\brief	Filter a message against a list of rules.
 *
 *\param	flist List of filter rules.
 *\param	info Message.
 *
 *\return	gboolean TRUE if filter rules handled the message.
 *
 *\note		Returning FALSE means the message was not handled,
 *		and that the calling code should do the default
 *		processing. E.g. \ref inc.c::inc_start moves the 
 *		message to the inbox. 	
 */
gboolean filter_message_by_msginfo(GSList *flist, MsgInfo *info, PrefsAccount* ac_prefs,
								   FilteringInvocationType context, gchar *extra_info)
{
	gboolean ret;

	if (prefs_common.enable_filtering_debug) {
		gchar *tmp = _("undetermined");
#ifndef G_OS_WIN32
		switch (context) {
		case FILTERING_INCORPORATION:
			tmp = _("incorporation");
			debug_filtering_session = prefs_common.enable_filtering_debug_inc;
			break;
		case FILTERING_MANUALLY:
			tmp = _("manually");
			debug_filtering_session = prefs_common.enable_filtering_debug_manual;
			break;
		case FILTERING_FOLDER_PROCESSING:
			tmp = _("folder processing");
			debug_filtering_session = prefs_common.enable_filtering_debug_folder_proc;
			break;
		case FILTERING_PRE_PROCESSING:
			tmp = _("pre-processing");
			debug_filtering_session = prefs_common.enable_filtering_debug_pre_proc;
			break;
		case FILTERING_POST_PROCESSING:
			tmp = _("post-processing");
			debug_filtering_session = prefs_common.enable_filtering_debug_post_proc;
			break;
		default:
			debug_filtering_session = FALSE;
			break;
		}
#else
		debug_filtering_session = FALSE;
#endif
		if (debug_filtering_session) {
			gchar *file = procmsg_get_message_file_path(info);
			gchar *spc = g_strnfill(LOG_TIME_LEN + 1, ' ');

			/* show context info and essential info about the message */
			if (prefs_common.filtering_debug_level >= FILTERING_DEBUG_LEVEL_MED) {
				log_print(LOG_DEBUG_FILTERING,
						_("filtering message (%s%s%s)\n"
						"%smessage file: %s\n%s%s %s\n%s%s %s\n%s%s %s\n%s%s %s\n"),
						tmp, extra_info ? _(": ") : "", extra_info ? extra_info : "",
						spc, file, spc, prefs_common_translated_header_name("Date:"), info->date,
						spc, prefs_common_translated_header_name("From:"), info->from,
						spc, prefs_common_translated_header_name("To:"), info->to,
						spc, prefs_common_translated_header_name("Subject:"), info->subject);
			} else {
				log_print(LOG_DEBUG_FILTERING,
						_("filtering message (%s%s%s)\n"
						"%smessage file: %s\n"),
						tmp, extra_info ? _(": ") : "", extra_info ? extra_info : "",
						spc, file);
			}
			g_free(file);
			g_free(spc);
		}
	} else
		debug_filtering_session = FALSE;

	ret = filter_msginfo(flist, info, ac_prefs);
	debug_filtering_session = FALSE;
	return ret;
}

gchar *filteringaction_to_string(FilteringAction *action)
{
	const gchar *command_str;
	gchar * quoted_dest;
	gchar * quoted_header;
	GString *dest = g_string_new("");
	gchar *deststr = NULL;

	command_str = get_matchparser_tab_str(action->type);

	if (command_str == NULL)
		return NULL;

	switch(action->type) {
	case MATCHACTION_MOVE:
	case MATCHACTION_COPY:
	case MATCHACTION_EXECUTE:
	case MATCHACTION_SET_TAG:
	case MATCHACTION_UNSET_TAG:
		quoted_dest = matcher_quote_str(action->destination);
		g_string_append_printf(dest, "%s \"%s\"", command_str, quoted_dest);
		g_free(quoted_dest);
		break;

	case MATCHACTION_DELETE:
	case MATCHACTION_MARK:
	case MATCHACTION_UNMARK:
	case MATCHACTION_LOCK:
	case MATCHACTION_UNLOCK:
	case MATCHACTION_MARK_AS_READ:
	case MATCHACTION_MARK_AS_UNREAD:
	case MATCHACTION_MARK_AS_SPAM:
	case MATCHACTION_MARK_AS_HAM:
	case MATCHACTION_STOP:
	case MATCHACTION_HIDE:
	case MATCHACTION_IGNORE:
	case MATCHACTION_WATCH:
	case MATCHACTION_CLEAR_TAGS:
		g_string_append_printf(dest, "%s", command_str);
		break;

	case MATCHACTION_REDIRECT:
	case MATCHACTION_FORWARD:
	case MATCHACTION_FORWARD_AS_ATTACHMENT:
		quoted_dest = matcher_quote_str(action->destination);
		g_string_append_printf(dest, "%s %d \"%s\"", command_str, action->account_id, quoted_dest);
		g_free(quoted_dest);
		break;

	case MATCHACTION_COLOR:
		g_string_append_printf(dest, "%s %d", command_str, action->labelcolor);
		break;

	case MATCHACTION_CHANGE_SCORE:
	case MATCHACTION_SET_SCORE:
		g_string_append_printf(dest, "%s %d", command_str, action->score);
		break;

	case MATCHACTION_ADD_TO_ADDRESSBOOK:
		quoted_header = matcher_quote_str(action->header);
		quoted_dest = matcher_quote_str(action->destination);
		g_string_append_printf(dest, "%s \"%s\" \"%s\"", command_str, quoted_header, quoted_dest);
		g_free(quoted_dest);
		g_free(quoted_header);
		break;

	default:
		return NULL;
	}
	deststr = dest->str;
	g_string_free(dest, FALSE);
	return deststr;
}

gchar * filteringaction_list_to_string(GSList * action_list)
{
	gchar *action_list_str;
        GSList * tmp;
	gchar *list_str;

        action_list_str = NULL;
        for (tmp = action_list ; tmp != NULL ; tmp = tmp->next) {
                gchar *action_str;
                FilteringAction * action;
                
                action = tmp->data;
                
                action_str = filteringaction_to_string(action);
                
                if (action_list_str != NULL) {
                        list_str = g_strconcat(action_list_str, " ", action_str, NULL);
                        g_free(action_list_str);
                }
                else {
                        list_str = g_strdup(action_str);
                }
		g_free(action_str);
                action_list_str = list_str;
        }

        return action_list_str;
}

gchar * filteringprop_to_string(FilteringProp * prop)
{
	gchar *list_str;
	gchar *action_list_str;
	gchar *filtering_str;

	if (prop == NULL)
		return NULL;

        action_list_str = filteringaction_list_to_string(prop->action_list);

	if (action_list_str == NULL)
		return NULL;

	list_str = matcherlist_to_string(prop->matchers);

	if (list_str == NULL) {
                g_free(action_list_str);
		return NULL;
        }

	filtering_str = g_strconcat(list_str, " ", action_list_str, NULL);
	g_free(action_list_str);
	g_free(list_str);

	return filtering_str;
}

static void prefs_filtering_free(GSList * prefs_filtering)
{
 	while (prefs_filtering != NULL) {
 		FilteringProp * filtering = (FilteringProp *)
			prefs_filtering->data;
 		filteringprop_free(filtering);
 		prefs_filtering = g_slist_remove(prefs_filtering, filtering);
 	}
}

static gboolean prefs_filtering_free_func(GNode *node, gpointer data)
{
	FolderItem *item = node->data;

	cm_return_val_if_fail(item, FALSE);
	cm_return_val_if_fail(item->prefs, FALSE);

	prefs_filtering_free(item->prefs->processing);
	item->prefs->processing = NULL;

	return FALSE;
}

void prefs_filtering_clear(void)
{
	GList * cur;

	for (cur = folder_get_list() ; cur != NULL ; cur = g_list_next(cur)) {
		Folder *folder;

		folder = (Folder *) cur->data;
		g_node_traverse(folder->node, G_PRE_ORDER, G_TRAVERSE_ALL, -1,
				prefs_filtering_free_func, NULL);
	}

	prefs_filtering_free(filtering_rules);
	filtering_rules = NULL;
	prefs_filtering_free(pre_global_processing);
	pre_global_processing = NULL;
	prefs_filtering_free(post_global_processing);
	post_global_processing = NULL;
}

void prefs_filtering_clear_folder(Folder *folder)
{
	cm_return_if_fail(folder);
	cm_return_if_fail(folder->node);

	g_node_traverse(folder->node, G_PRE_ORDER, G_TRAVERSE_ALL, -1,
			prefs_filtering_free_func, NULL);
	/* FIXME: Note folder settings were changed, where the updates? */
}

gboolean filtering_peek_per_account_rules(GSList *filtering_list)
/* return TRUE if there's at least one per-account filtering rule */
{
	GSList *l;

	for (l = filtering_list; l != NULL; l = g_slist_next(l)) {
		FilteringProp * filtering = (FilteringProp *) l->data;

		if (filtering->enabled && (filtering->account_id != 0)) {
			return TRUE;
		}		
	}

	return FALSE;
}

gboolean filtering_action_list_rename_path(GSList *action_list, const gchar *old_path,
					   const gchar *new_path)
{
	gchar *base;
	gchar *prefix;
	gchar *suffix;
	gchar *dest_path;
	gchar *old_path_with_sep;
	gint destlen;
	gint prefixlen;
	gint oldpathlen;
        GSList * action_cur;
	const gchar *separator=G_DIR_SEPARATOR_S;
	gboolean matched = FALSE;
#ifdef G_OS_WIN32
again:
#endif
	oldpathlen = strlen(old_path);
	old_path_with_sep = g_strconcat(old_path,separator,NULL);

	for(action_cur = action_list ; action_cur != NULL ;
		action_cur = action_cur->next) {

		FilteringAction *action = action_cur->data;
                        
		if (action->type == MATCHACTION_SET_TAG ||
		    action->type == MATCHACTION_UNSET_TAG)
			continue;
		if (!action->destination) 
			continue;
		
		destlen = strlen(action->destination);
                        
		if (destlen > oldpathlen) {
			prefixlen = destlen - oldpathlen;
			suffix = action->destination + prefixlen;
                                
			if (!strncmp(old_path, suffix, oldpathlen)) {
				prefix = g_malloc0(prefixlen + 1);
				strncpy2(prefix, action->destination, prefixlen);
                                        
				base = suffix + oldpathlen;
				while (*base == G_DIR_SEPARATOR) base++;
                                if (*base == '\0')
                                	dest_path = g_strconcat(prefix, separator,
                                				new_path, NULL);
				else
					dest_path = g_strconcat(prefix,
								separator,
								new_path,
								separator,
								base, NULL);
                                        
					g_free(prefix);
					g_free(action->destination);
					action->destination = dest_path;
					matched = TRUE;
			} else { /* for non-leaf folders */
				/* compare with trailing slash */
				if (!strncmp(old_path_with_sep, action->destination, oldpathlen+1)) {
                                                
					suffix = action->destination + oldpathlen + 1;
					dest_path = g_strconcat(new_path, separator,
								suffix, NULL);
					g_free(action->destination);
					action->destination = dest_path;
					matched = TRUE;
				}
			}
		} else {
			/* folder-moving a leaf */
			if (!strcmp(old_path, action->destination)) {
                        	dest_path = g_strdup(new_path);
                        	g_free(action->destination);
                        	action->destination = dest_path;
                        	matched = TRUE;
			}
		}
	}
	
	g_free(old_path_with_sep);
#ifdef G_OS_WIN32
	if (!strcmp(separator, G_DIR_SEPARATOR_S) && !matched) {
		separator = "/";
		goto again;
	}
#endif

	return matched;
}
