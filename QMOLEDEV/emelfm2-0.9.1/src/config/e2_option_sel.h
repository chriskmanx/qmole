/* $Id: e2_option_sel.h 2064 2010-03-12 13:15:36Z tpgww $

Copyright (C) 2004-2009 tooar <tooar@emelfm2.net>

This file is part of emelFM2.
emelFM2 is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

#ifndef __E2_OPTION_SEL_H__
#define __E2_OPTION_SEL_H__

#include "emelfm2.h"
#include "e2_option.h"

GtkWidget *e2_option_sel_add_menu_widget (GtkWidget *controller, GtkWidget *menu,
	E2_OptionSet *set, void (*func)(), gpointer data); //args GtkMenuItem*,pointer
void e2_option_sel_add_widget (GtkWidget *dialog, GtkWidget *box,
	GtkSizeGroup *size_group, E2_OptionSet *set);
E2_OptionSet *e2_option_sel_register (gchar *name, gchar *group, gchar *desc,
	gchar *tip, gchar *depends, gint def, const gchar **values,
	E2_OptionFlags flags);
gint e2_option_sel_set (gchar *option, gint value);
gint e2_option_sel_set_direct (E2_OptionSet *set, gint value);
gint e2_option_sel_get (gchar *option);
gint e2_option_sel_get_direct (E2_OptionSet *set);

#endif // ndef __E2_OPTION_SEL_H__
