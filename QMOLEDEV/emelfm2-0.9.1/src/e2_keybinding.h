/* $Id: e2_keybinding.h 2064 2010-03-12 13:15:36Z tpgww $

Copyright (C) 2004-2009 tooar <tooar@emelfm2.net>

This file is part of emelfm2.
emelfm2 is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

emelfm2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

#ifndef __E2_KEYBINDING_H__
#define __E2_KEYBINDING_H__

#include "emelfm2.h"

typedef struct _E2_NodeMatch
{
	GQuark qname;
	GNode *match;
} E2_NodeMatch;

typedef struct _E2_NodeData
{
	GQuark qname;
	GtkWidget *widget;
} E2_NodeData;

void e2_keybinding_localise (GtkTreeModel *mdl, GtkTreeIter *iter);
void e2_keybinding_clear_widget_bindings (GPtrArray *bindings);
//void e2_keybinding_find_relations (GPtrArray *boundto, const gchar *thisname,
//	gboolean *ancestor, gboolean *descendant);
gboolean e2_keybinding_walk_toparent (GNode *node, E2_NodeMatch *data);

void e2_keybinding_register_all (void);
void e2_keybinding_enrol (GtkWidget *widget, const gchar *category,
	void (*defaults_func)(E2_OptionSet*));
void e2_keybinding_disrol (GtkWidget *widget, const gchar *category);
void e2_keybinding_block (GtkWidget *widget, const gchar *category);
void e2_keybinding_unblock (GtkWidget *widget, const gchar *category);
void e2_keybinding_output_help (gchar *section);
void e2_keybinding_clean (void);
void e2_keybinding_free_all (void);
#ifdef WITH_KEYFAKE
void e2_keybinding_actions_register (void);
#endif
void e2_keybinding_options_register (void);

#endif //ndef __E2_KEYBINDING_H__
