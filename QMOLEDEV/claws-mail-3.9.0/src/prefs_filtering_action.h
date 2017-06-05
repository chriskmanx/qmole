/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2003-2012 the Claws Mail team
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

#ifndef __PREFS_FILTERING_ACTION_H__
#define __PREFS_FILTERING_ACTION_H__

#include "matcher.h"

typedef void PrefsFilteringActionSignal	(GSList *action_list);

void prefs_filtering_action_open		(GSList *action_list,
				 PrefsFilteringActionSignal *cb);

#endif /* __PREFS_FILTER_H__ */
