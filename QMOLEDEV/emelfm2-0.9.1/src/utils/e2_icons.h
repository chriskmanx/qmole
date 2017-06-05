/* $Id: e2_icons.h 3031 2014-01-24 21:07:28Z tpgww $

Copyright (C) 2013 tooar <tooar@emelfm2.net>

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
/* Workarounds for deprecation of stock-items in gtk 3.10+ */

#ifndef __E2_ICONS_H__
#define __E2_ICONS_H__

#include "emelfm2.h"

//enable config/button/menu image-cacheing
#define E2_ICONCACHE

//these replicate the original gtk-stock #define's
#define STOCK_NAME_ABOUT            "gtk-about"
#define STOCK_NAME_ADD              "gtk-add"
#define STOCK_NAME_APPLY            "gtk-apply"
#define STOCK_NAME_BOLD             "gtk-bold"
#define STOCK_NAME_CANCEL           "gtk-cancel"
#define STOCK_NAME_CAPS_LOCK_WARNING "gtk-caps-lock-warning"
#define STOCK_NAME_CDROM            "gtk-cdrom"
#define STOCK_NAME_CLEAR            "gtk-clear"
#define STOCK_NAME_CLOSE            "gtk-close"
#define STOCK_NAME_COLOR_PICKER     "gtk-color-picker"
#define STOCK_NAME_CONNECT          "gtk-connect"
#define STOCK_NAME_CONVERT          "gtk-convert"
#define STOCK_NAME_COPY             "gtk-copy"
#define STOCK_NAME_CUT              "gtk-cut"
#define STOCK_NAME_DELETE           "gtk-delete"
#define STOCK_NAME_DIALOG_AUTHENTICATION "gtk-dialog-authentication"
#define STOCK_NAME_DIALOG_ERROR     "gtk-dialog-error"
#define STOCK_NAME_DIALOG_INFO      "gtk-dialog-info"
#define STOCK_NAME_DIALOG_QUESTION  "gtk-dialog-question"
#define STOCK_NAME_DIALOG_WARNING   "gtk-dialog-warning"
#define STOCK_NAME_DIRECTORY        "gtk-directory"
#define STOCK_NAME_DISCARD          "gtk-discard"
#define STOCK_NAME_DISCONNECT       "gtk-disconnect"
#define STOCK_NAME_DND              "gtk-dnd"
#define STOCK_NAME_DND_MULTIPLE     "gtk-dnd-multiple"
#define STOCK_NAME_EDIT             "gtk-edit"
#define STOCK_NAME_EXECUTE          "gtk-execute"
#define STOCK_NAME_FILE             "gtk-file"
#define STOCK_NAME_FIND_AND_REPLACE "gtk-find-and-replace"
#define STOCK_NAME_FIND             "gtk-find"
#define STOCK_NAME_FLOPPY           "gtk-floppy"
#define STOCK_NAME_FULLSCREEN       "gtk-fullscreen"
#define STOCK_NAME_GO_BACK          "gtk-go-back"
#define STOCK_NAME_GO_DOWN          "gtk-go-down"
#define STOCK_NAME_GO_FORWARD       "gtk-go-forward"
#define STOCK_NAME_GOTO_BOTTOM      "gtk-goto-bottom"
#define STOCK_NAME_GOTO_FIRST       "gtk-goto-first"
#define STOCK_NAME_GOTO_LAST        "gtk-goto-last"
#define STOCK_NAME_GOTO_TOP         "gtk-goto-top"
#define STOCK_NAME_GO_UP            "gtk-go-up"
#define STOCK_NAME_HARDDISK         "gtk-harddisk"
#define STOCK_NAME_HELP             "gtk-help"
#define STOCK_NAME_HOME             "gtk-home"
#define STOCK_NAME_INDENT           "gtk-indent"
#define STOCK_NAME_INDEX            "gtk-index"
#define STOCK_NAME_INFO             "gtk-info"
#define STOCK_NAME_ITALIC           "gtk-italic"
#define STOCK_NAME_JUMP_TO          "gtk-jump-to"
#define STOCK_NAME_JUSTIFY_CENTER   "gtk-justify-center"
#define STOCK_NAME_JUSTIFY_FILL     "gtk-justify-fill"
#define STOCK_NAME_JUSTIFY_LEFT     "gtk-justify-left"
#define STOCK_NAME_JUSTIFY_RIGHT    "gtk-justify-right"
#define STOCK_NAME_LEAVE_FULLSCREEN "gtk-leave-fullscreen"
#define STOCK_NAME_MEDIA_FORWARD    "gtk-media-forward"
#define STOCK_NAME_MEDIA_NEXT       "gtk-media-next"
#define STOCK_NAME_MEDIA_PAUSE      "gtk-media-pause"
#define STOCK_NAME_MEDIA_PLAY       "gtk-media-play"
#define STOCK_NAME_MEDIA_PREVIOUS   "gtk-media-previous"
#define STOCK_NAME_MEDIA_RECORD     "gtk-media-record"
#define STOCK_NAME_MEDIA_REWIND     "gtk-media-rewind"
#define STOCK_NAME_MEDIA_STOP       "gtk-media-stop"
#define STOCK_NAME_MISSING_IMAGE    "gtk-missing-image"
#define STOCK_NAME_NETWORK          "gtk-network"
#define STOCK_NAME_NEW              "gtk-new"
#define STOCK_NAME_NO               "gtk-no"
#define STOCK_NAME_OK               "gtk-ok"
#define STOCK_NAME_OPEN             "gtk-open"
#define STOCK_NAME_ORIENTATION_LANDSCAPE "gtk-orientation-landscape"
#define STOCK_NAME_ORIENTATION_PORTRAIT "gtk-orientation-portrait"
#define STOCK_NAME_ORIENTATION_REVERSE_LANDSCAPE "gtk-orientation-reverse-landscape"
#define STOCK_NAME_ORIENTATION_REVERSE_PORTRAIT "gtk-orientation-reverse-portrait"
#define STOCK_NAME_PAGE_SETUP       "gtk-page-setup"
#define STOCK_NAME_PASTE            "gtk-paste"
#define STOCK_NAME_PREFERENCES      "gtk-preferences"
#define STOCK_NAME_PRINT_ERROR      "gtk-print-error"
#define STOCK_NAME_PRINT            "gtk-print"
#define STOCK_NAME_PRINT_PAUSED     "gtk-print-paused"
#define STOCK_NAME_PRINT_PREVIEW    "gtk-print-preview"
#define STOCK_NAME_PRINT_REPORT     "gtk-print-report"
#define STOCK_NAME_PRINT_WARNING    "gtk-print-warning"
#define STOCK_NAME_PROPERTIES       "gtk-properties"
#define STOCK_NAME_QUIT             "gtk-quit"
#define STOCK_NAME_REDO             "gtk-redo"
#define STOCK_NAME_REFRESH          "gtk-refresh"
#define STOCK_NAME_REMOVE           "gtk-remove"
#define STOCK_NAME_REVERT_TO_SAVED  "gtk-revert-to-saved"
#define STOCK_NAME_SAVE_AS          "gtk-save-as"
#define STOCK_NAME_SAVE             "gtk-save"
#define STOCK_NAME_SELECT_ALL       "gtk-select-all"
#define STOCK_NAME_SELECT_COLOR     "gtk-select-color"
#define STOCK_NAME_SELECT_FONT      "gtk-select-font"
#define STOCK_NAME_SORT_ASCENDING   "gtk-sort-ascending"
#define STOCK_NAME_SORT_DESCENDING  "gtk-sort-descending"
#define STOCK_NAME_SPELL_CHECK      "gtk-spell-check"
#define STOCK_NAME_STOP             "gtk-stop"
#define STOCK_NAME_STRIKETHROUGH    "gtk-strikethrough"
#define STOCK_NAME_UNDELETE         "gtk-undelete"
#define STOCK_NAME_UNDERLINE        "gtk-underline"
#define STOCK_NAME_UNDO             "gtk-undo"
#define STOCK_NAME_UNINDENT         "gtk-unindent"
#define STOCK_NAME_YES              "gtk-yes"
#define STOCK_NAME_ZOOM_100         "gtk-zoom-100"
#define STOCK_NAME_ZOOM_FIT         "gtk-zoom-fit"
#define STOCK_NAME_ZOOM_IN          "gtk-zoom-in"
#define STOCK_NAME_ZOOM_OUT         "gtk-zoom-out"

#ifdef E2_ADD_STOCKS

typedef struct _E2_Stock
{
	gchar *stock;//allocated stock-name, often one of the STOCK_NAME_*'s
	gchar *name; //allocated icon-theme file-basename (no extension) or NULL if same as stock
	gchar *label;//translated or otherwise-public name
	gboolean freelabel; //whether to g_free label when done
} E2_Stock;

void e2_icons_cache_stocks (void);
void e2_icons_clear_stocks (void);

#endif //def E2_ADD_STOCKS

void e2_icons_register_stocks (void);
const gchar *e2_icons_get_stock_label (const gchar *name);
gboolean e2_icons_check_stock (const gchar *name);

#ifdef E2_ICONCACHE
void e2_icons_cache_init (void);
void e2_icons_cache_clear (void);
GdkPixbuf *e2_icons_get_puxbuf (const gchar *name, GtkIconSize size, gboolean missing);
gint e2_icons_get_pixsize (GtkIconSize size);
GtkIconSize e2_icons_get_size (gint psize);
#endif

gchar *e2_icons_get_custom_path (gboolean withtrailer) G_GNUC_MALLOC;

#endif //ndef __E2_ICONS_H__
