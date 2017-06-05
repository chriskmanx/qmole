/* $Id: e2_print.h 2076 2010-05-17 22:35:19Z tpgww $

Copyright (C) 2010 tooar <tooar@emelfm2.net>

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

#ifndef __E2_PRINT_H__
#define __E2_PRINT_H__

#include "e2_view_dialog.h"

void e2_dialog_print_cb (GtkMenuItem *menuitem, E2_ViewDialogRuntime *rt);
void e2_print_clear (void);

#endif //ndef __E2_PRINT_H__
