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

#ifndef FILTER_NEW_H

#define FILTER_NEW_H

#include <glib.h>
#include "matcher.h"
#include "procmsg.h"

struct _FilteringAction {
	gint	type;
	gint	account_id;
	gchar  *destination;
	gint	labelcolor;
	gint	score;
	gchar  *header;
};

typedef struct _FilteringAction FilteringAction;

struct _FilteringProp {
	gboolean enabled;
	gchar *name;
	gint account_id;
	MatcherList * matchers;
	GSList * action_list;
};

typedef struct _FilteringProp FilteringProp;

enum {
	FILTERING_ACCOUNT_RULES_SKIP = 0,
	FILTERING_ACCOUNT_RULES_FORCE = 1,
	FILTERING_ACCOUNT_RULES_USE_CURRENT = 2
};

typedef enum {
	FILTERING_INCORPORATION,
	FILTERING_MANUALLY,
	FILTERING_FOLDER_PROCESSING,
	FILTERING_PRE_PROCESSING,
	FILTERING_POST_PROCESSING
} FilteringInvocationType;

typedef enum {
	FILTERING_DEBUG_LEVEL_LOW,
	FILTERING_DEBUG_LEVEL_MED,
	FILTERING_DEBUG_LEVEL_HIGH
} FilteringDebugLevel;
	
/* extern GSList * prefs_filtering; */


FilteringAction * filteringaction_new(int type, int account_id,
				      gchar * destination,
                                      gint labelcolor, gint score, gchar *header);
void filteringaction_free(FilteringAction *action);
FilteringAction * filteringaction_parse(gchar **str);
gboolean filteringaction_apply_action_list (GSList *action_list, MsgInfo *info);

FilteringProp * filteringprop_new(gboolean enabled,
				  const gchar *name,
				  gint account_id,
				  MatcherList *matchers,
				  GSList *action_list);
void filteringprop_free(FilteringProp *prop);

FilteringProp * filteringprop_parse(gchar **str);

void filter_msginfo_move_or_delete(GSList *filtering_list, MsgInfo *info);
gboolean filter_message_by_msginfo(GSList *flist, MsgInfo *info, PrefsAccount *ac_prefs,
								   FilteringInvocationType context, gchar *extra_info);

gchar * filteringaction_to_string(FilteringAction *action);
void prefs_filtering_write_config(void);
void prefs_filtering_read_config(void);
gchar * filteringaction_list_to_string(GSList * action_list);
gchar * filteringprop_to_string(FilteringProp *prop);

void prefs_filtering_clear(void);
void prefs_filtering_clear_folder(Folder *folder);

FilteringProp * filteringprop_copy(FilteringProp *src);
void filtering_move_and_copy_msgs(GSList *msglist);
extern GSList * filtering_rules;
extern GSList * pre_global_processing;
extern GSList * post_global_processing;

gboolean filtering_peek_per_account_rules(GSList *filtering_list);

GSList *filtering_action_list_sort(GSList *action_list);
gboolean filtering_action_list_rename_path(GSList *action_list, const gchar *old_path,
					const gchar *new_path);

#endif
