/* $Id: e2_option_int.c 2582 2012-12-03 01:14:09Z tpgww $

Copyright (C) 2004-2012 tooar <tooar@emelfm2.net>

This file is part of emelFM2.
emelFM2 is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

#include "emelfm2.h"
#include "e2_option.h"
#include "e2_option_int.h"

void e2_option_int_add_widget (GtkWidget *dialog, GtkWidget *box,
	GtkSizeGroup *size_group, E2_OptionSet *set)
{
	GtkWidget *event = e2_widget_add_eventbox (box, TRUE, 0);
#ifdef USE_GTK3_0
	GtkWidget *hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, E2_PADDING);
#else
	GtkWidget *hbox = gtk_hbox_new (FALSE, E2_PADDING);
#endif
	gtk_container_add (GTK_CONTAINER (event), hbox);
	gchar *label_text = g_strconcat (set->desc, ":", NULL);
	GtkWidget *label = e2_widget_add_mid_label (hbox, label_text, 0.0, FALSE, 0);
	g_free (label_text);
	gtk_size_group_add_widget (size_group, label);
	gdouble max_percent = set->ex.num.max / 100.0;
	GtkAdjustment *spinner_adj = (GtkAdjustment *) gtk_adjustment_new
		((gdouble)set->ival, (gdouble)set->ex.num.min, (gdouble)set->ex.num.max,
		1.0, max_percent, 0.0);
	set->widget = gtk_spin_button_new (spinner_adj, 1.0, 0);
	e2_widget_set_safetip (event, set->tip);
	gtk_box_pack_start (GTK_BOX (hbox), set->widget, FALSE, FALSE, E2_PADDING);
}

E2_OptionSet *e2_option_int_register (gchar *name, gchar *group, gchar *desc,
	gchar *tip, gchar *depends, gint def, gint min, gint max, E2_OptionFlags flags)
{
	E2_OptionSet *set = e2_option_register (E2_OPTION_TYPE_INT, name, group,
		desc, tip, depends, flags);
	set->ex.num.min = min;
	set->ex.num.max = max;
//	set s/ival
	set->sval = g_strdup ("");	//it's freed in the value-set func
	set->hook_freezed = TRUE;
	e2_option_int_set_direct (set, def);
	set->hook_freezed = FALSE;
	return set;
}

gint e2_option_int_get (gchar *option)
{
	E2_OptionSet *set;
	set = e2_option_get (option);
	if ((set->type == E2_OPTION_TYPE_SEL) || (set->type == E2_OPTION_TYPE_INT))
	{
		return e2_option_int_get_direct (set);
	} else
	{
		printd (WARN, "trying to get int option '%s' which isn't an int", option);
		return -1;
	}
}

gint e2_option_int_get_direct (E2_OptionSet *set)
{
	return set->ival;
}

gint e2_option_int_set (gchar *option, gint value)
{
	E2_OptionSet *set;
	set = e2_option_get (option);
	if (set->type == E2_OPTION_TYPE_INT)
		return e2_option_int_set_direct (set, value);
	else
	{
		printd (WARN, "trying to set int option '%s' which isn't an int", option);
		return 0;
	}
}

gint e2_option_int_set_direct (E2_OptionSet *set, gint value)
{
	if (value < set->ex.num.min) value = set->ex.num.min;
	if (value > set->ex.num.max) value = set->ex.num.max;
	set->ival = value;
	g_free (set->sval);
	set->sval = g_strdup_printf ("%d", set->ival);
	if (!set->hook_freezed)
		e2_hook_list_run (&set->hook_value_changed, GINT_TO_POINTER (value));
	return value;
}
