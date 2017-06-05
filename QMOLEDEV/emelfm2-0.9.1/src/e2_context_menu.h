/* $Id: e2_context_menu.h 2534 2012-05-27 08:07:07Z tpgww $

Copyright (C) 2003-2012 tooar <tooar@emelfm2.net>
Portions copyright (C) 2004 Florian Zaehringer <flo.zaehringer@web.de>

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

#ifndef __E2_CONTEXT_MENU_H__
#define __E2_CONTEXT_MENU_H__

#include "emelfm2.h"

void e2_context_menu_show (guint button, guint32 time, gint type, ViewInfo *view);
gboolean e2_context_menu_show_menu_action (gpointer from, E2_ActionRuntime *art);
void e2_context_menu_options_register (void);

#endif //ndef __E2_CONTEXT_MENU_H__
