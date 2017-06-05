/*
 *  Leafpad - GTK+ based simple text editor
 *  Copyright (C) 2004-2005 Tarot Osuji
 *  
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *  
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifndef _LEAFPAD_H
#define _LEAFPAD_H

#ifdef HAVE_CONFIG_H
#	include "config.h"
#endif

#include <gtk/gtk.h>
#include "i18n.h"

#ifndef ENABLE_PRINT
#	if GTK_CHECK_VERSION(2, 10, 0)
#		define ENABLE_PRINT
#	endif
#endif

#include "window.h"
#include "menu.h"
#include "callback.h"
#include "view.h"
#include "undo.h"
#include "font.h"
#include "linenum.h"
#include "indent.h"
#include "hlight.h"
#include "selector.h"
#include "file.h"
#include "encoding.h"
#include "search.h"
#include "dialog.h"
#include "about.h"
#include "dnd.h"
#include "utils.h"
#include "emacs.h"
#ifdef ENABLE_PRINT
#	if GTK_CHECK_VERSION(2, 10, 0)
#		include "gtkprint.h"
#	else
#		include "gnomeprint.h"
#	endif
#endif

typedef struct {
	FileInfo *fi;
	MainWin *mw;
} PublicData;

#ifdef GLOBAL_VARIABLE_DEFINE
#	define GLOBAL
#else
#	define GLOBAL extern
#endif
GLOBAL PublicData *pub;

void save_config_file(void);

#endif /* _LEAFPAD_H */
