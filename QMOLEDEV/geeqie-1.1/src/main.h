/*
 * Geeqie
 * (C) 2006 John Ellis
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * Author: John Ellis
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */


#ifndef MAIN_H
#define MAIN_H

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#ifdef HAVE_STRVERSCMP
#  ifndef _GNU_SOURCE
#    define _GNU_SOURCE
#  endif
#endif

#include "intl.h"


/*
 *-------------------------------------
 * Standard library includes
 *-------------------------------------
 */

#include <pwd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <dirent.h>
#include <fcntl.h>

/*
 *-------------------------------------
 * includes for glib / gtk / gdk-pixbuf
 *-------------------------------------
 */

#include <gdk/gdk.h>
#include <gtk/gtk.h>

#include <gdk-pixbuf/gdk-pixbuf.h>
#include <gdk-pixbuf/gdk-pixbuf-loader.h>

#include "compat.h"

/*
 *----------------------------------------------------------------------------
 * defines
 *----------------------------------------------------------------------------
 */

#define USE_XDG 1

#define GQ_APPNAME "Geeqie"
#define GQ_APPNAME_LC "geeqie"
#define GQ_WEBSITE "geeqie.sourceforge.net"
#define GQ_EMAIL_ADDRESS "geeqie-devel@lists.sourceforge.net"

#define GQ_RC_DIR		"." GQ_APPNAME_LC
#define GQ_COLLECTIONS_DIR	"collections"
#define GQ_TRASH_DIR		"trash"

#define GQ_SYSTEM_WIDE_DIR    "/etc/" GQ_APPNAME_LC

#define RC_FILE_NAME GQ_APPNAME_LC "rc.xml"

#define GQ_COLLECTION_EXT ".gqv"

#define SCROLL_RESET_TOPLEFT 0
#define SCROLL_RESET_CENTER 1
#define SCROLL_RESET_NOCHANGE 2

#define MOUSEWHEEL_SCROLL_SIZE 20


#define GQ_DEFAULT_SHELL_PATH "/bin/sh"
#define GQ_DEFAULT_SHELL_OPTIONS "-c"

#define COLOR_PROFILE_INPUTS 4

#define DEFAULT_THUMB_WIDTH	96
#define DEFAULT_THUMB_HEIGHT	72

#define DEFAULT_MINIMAL_WINDOW_SIZE 100

#define IMAGE_MIN_WIDTH 100
#define SIDEBAR_DEFAULT_WIDTH 250


#define DEFAULT_OVERLAY_INFO	"%collection:<i>*</i>\\n%" \
				"(%number%/%total%) [%zoom%] <b>%name%</b>\n" \
				"%res%|%date%|%size%\n" \
				"%formatted.Aperture%|%formatted.ShutterSpeed%|%formatted.ISOSpeedRating:ISO *%|%formatted.FocalLength%|%formatted.ExposureBias:* Ev%\n" \
				"%formatted.Camera:40%|%formatted.Flash%"

#define GQ_LINK_STR "↗"
#include "typedefs.h"
#include "debug.h"
#include "options.h"

#define DESKTOP_FILE_TEMPLATE GQ_APP_DIR "/template.desktop"
/*
 *----------------------------------------------------------------------------
 * main.c
 *----------------------------------------------------------------------------
 */

/*
 * This also doubles as the main.c header.
 */

extern gboolean thumb_format_changed;

void keyboard_scroll_calc(gint *x, gint *y, GdkEventKey *event);
gint key_press_cb(GtkWidget *widget, GdkEventKey *event, gpointer data);

void exit_program(void);

#define CASE_SORT(a, b) ( (options->file_sort.case_sensitive) ? strcmp((a), (b)) : strcasecmp((a), (b)) )


#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
