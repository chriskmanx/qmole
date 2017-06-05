/*
 * Geeqie
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */


#ifndef TRASH_H
#define TRASH_H

#include "ui_utildlg.h"

void file_util_trash_clear(void);
gboolean file_util_safe_unlink(const gchar *path);
gchar *file_util_safe_delete_status(void);

#endif /* TRASH_H */
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
