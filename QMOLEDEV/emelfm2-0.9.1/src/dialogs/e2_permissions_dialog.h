/* $Id: e2_permissions_dialog.h 2064 2010-03-12 13:15:36Z tpgww $

Copyright (C) 2004-2009 tooar <tooar@emelfm2.net>

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

#ifndef __E2_PERMISSIONS_DIALOG_H__
#define __E2_PERMISSIONS_DIALOG_H__

#include "emelfm2.h"
#include "e2_dialog.h"

typedef enum
{
	E2_RECURSE_NONE  = 0,
	E2_RECURSE_DIRS  = 1,
	E2_RECURSE_OTHER = 1 << 1
} E2_RecurseType;
#define E2_RECURSE_ALL E2_RECURSE_DIRS | E2_RECURSE_OTHER

typedef enum
{
	E2_CHMOD_NONE,
	E2_CHMOD_SET,
	E2_CHMOD_ADD,
	E2_CHMOD_REM
} E2_ChmodType;

DialogButtons e2_permissions_dialog_run (VPATH *localpath,
	gboolean multi, gboolean permitted,
	E2_ChmodType *op_ret, mode_t *mode_ret,
	E2_RecurseType *recurse_ret, gint *winx_ret, gint *winy_ret);

#endif //ndef __E2_PERMISSIONS_DIALOG_H__
