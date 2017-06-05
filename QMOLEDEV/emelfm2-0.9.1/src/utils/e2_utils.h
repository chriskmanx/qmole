/* $Id: e2_utils.h 2910 2013-11-13 00:58:59Z tpgww $

Copyright (C) 2003-2013 tooar <tooar@emelfm2.net>

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

#ifndef __E2_UTILS_H__
#define __E2_UTILS_H__

#include "emelfm2.h"
#ifdef E2_MAGIC
# include <magic.h>
#endif

#define LF 10
#define CR 13

typedef enum
{
	E2_DOTS_START,
	E2_DOTS_MIDDLE,
	E2_DOTS_END,
} E2_DotMode;

typedef struct _E2_Duo
{
	gpointer a;
	gpointer b;
} E2_Duo;

typedef struct _E2_Trio
{
	gpointer a;
	gpointer b;
	gpointer c;
} E2_Trio;
/*
typedef struct _E2_Quartet
{
	gpointer a;
	gpointer b;
	gpointer c;
	gpointer d;
} E2_Quartet;
*/
typedef struct _E2_Sextet
{
	gpointer a;
	gpointer b;
	gpointer c;
	gpointer d;
	gpointer e;
	gpointer f;
} E2_Sextet;

typedef struct _E2_Nontet
{
	gpointer a;
	gpointer b;
	gpointer c;
	gpointer d;
	gpointer e;
	gpointer f;
	gpointer g;
	gpointer h;
	gpointer i;
} E2_Nontet;

#ifdef E2_MAGIC
typedef struct
{
	gpointer libhandle;
	magic_t (*open) (gint);
	void (*close) (magic_t);
	gint (*setflags) (magic_t, gint);
	gint (*load) (magic_t,const gchar *);
	const gchar *(*file) (magic_t, const gchar *);
	const gchar *(*error) (magic_t);
} MagicIface;
#endif

//E2_Trio *e2_utils_trio_new (void) G_GNUC_MALLOC;
E2_Sextet *e2_utils_sextet_new (void) G_GNUC_MALLOC;
E2_Nontet *e2_utils_nontet_new (void) G_GNUC_MALLOC;
//void e2_utils_trio_destroy (E2_Trio *t);
void e2_utils_sextet_destroy (E2_Sextet *s);
void e2_utils_nontet_destroy (E2_Nontet *n);
void e2_utils_show_memory_message (void);
void e2_utils_memory_error (void);
void e2_utils_show_help (gchar *title);
gchar *e2_utils_color2str (GdkColor *color) G_GNUC_MALLOC;
gchar *e2_utils_str_replace (const gchar *str, const gchar *old, const gchar *new) G_GNUC_MALLOC;
gint e2_utils_LF_line_ends (gchar *text);
gchar *e2_utils_revert_line_ends (gchar *text, guint linecount, gint separator);
#ifdef E2_MAGIC
gboolean e2_utils_fill_magic_iface (MagicIface *iface);
#endif
gchar *e2_utils_get_mimetype (VPATH *localpath);
void e2_utils_get_charset (const gchar **encoding);
void e2_utils_rowstr_split (gchar *line, gint columns, gchar *parts[]);
gchar *e2_utils_str_stretch (gchar *string) G_GNUC_MALLOC;
gchar *e2_utils_str_shorten (gchar *string, gint limit, E2_DotMode position) G_GNUC_MALLOC;
gchar *e2_utils_str_to_lower (gchar *string) G_GNUC_MALLOC;
gint e2_utils_get_byte_position (const gchar *utf8_string, gint charoffset);
gchar *e2_utils_dircat (ViewInfo *view, const gchar *string, gboolean localised) G_GNUC_MALLOC;
gchar *e2_utils_strcat (const gchar *string1, const gchar *string2) G_GNUC_MALLOC;
gchar **e2_utils_str_breakup (const gchar *string, const gchar *delimiter,
	gint max_tokens) G_GNUC_MALLOC;
gchar *e2_utils_find_whitespace (gchar *string);
gchar *e2_utils_pass_whitespace (gchar *string);
gchar *e2_utils_quote_string (const gchar *string) G_GNUC_MALLOC;
gchar *e2_utils_unquote_string (const gchar *utf8string) G_GNUC_MALLOC;
gchar *e2_utils_bare_strchr (gchar *string, gchar c);
gchar *e2_utils_get_first_part (gchar *string, gboolean quoted) G_GNUC_MALLOC;
gchar *e2_utils_get_tempname (const gchar *orig) G_GNUC_MALLOC;
gboolean e2_utils_get_parent_path (gchar *path, gboolean ignore_trailer);
gchar *e2_utils_path_clean (gchar *path)
#ifdef G_GNUC_WARN_UNUSED_RESULT
 G_GNUC_WARN_UNUSED_RESULT
#endif
;
gchar *e2_utils_translate_relative_path (const gchar *base_dir, const gchar *new_path) G_GNUC_MALLOC;
gchar *e2_utils_create_relative_path (VPATH *src, VPATH *dest) G_GNUC_MALLOC;
const gchar *e2_utils_skip_relative_path (VPATH *localpath);
gchar *e2_utils_get_temp_path (const gchar *id) G_GNUC_MALLOC;
gchar *e2_utils_get_home_path (const gchar *utfpath) G_GNUC_MALLOC;
gchar *e2_utils_get_trash_path (gchar *localpath, gboolean filesplace) G_GNUC_MALLOC;
GList *e2_utils_get_trash_all (void) G_GNUC_MALLOC;

const gchar *e2_utils_get_output_font (void);
void e2_utils_update_gtk_settings (void);
gchar *e2_utils_expand_macros (const gchar *text, const gchar *for_each) G_GNUC_MALLOC;
gchar *e2_utils_replace_name_macros (const gchar *text, const gchar *utfpath) G_GNUC_MALLOC;
gchar *e2_utils_replace_multiname (const gchar *text, gchar *path, GPtrArray *array,
	gboolean single) G_GNUC_MALLOC;
gboolean e2_utils_get_variable (gchar **variable);
gchar *e2_utils_replace_vars (gchar *raw, gboolean rawpath) G_GNUC_MALLOC;
gchar *e2_utils_replace_wildcards (gchar *raw);
void e2_utils_get_abs_pos (GtkWidget *widget, gint *x, gint *y);
#ifdef USE_GTK3_0
void e2_utils_save_state (GtkWidget *widget);
GdkModifierType e2_utils_get_savedstate (GtkWidget *widget);
gboolean e2_utils_get_pointer_position (GtkWidget *widget, gint *x, gint *y);
#else
GdkModifierType e2_utils_get_modifiers (void);
#endif
void e2_utils_beep (void);
gboolean e2_utils_multi_src (GList *srclist);
gunichar e2_utils_get_mnemonic_char (const gchar *label);
guint e2_utils_get_mnemonic_keycode (gchar *label);
void e2_utils_block_thread_signals (void);

#ifndef USE_GTK2_10
gboolean e2_utils_key_is_modifier (GdkEventKey *event);
#endif
void e2_utils_translate_keys (gboolean native, gboolean local);
void e2_utils_translate_key_event (GdkEventKey *event);
void e2_utils_fake_event (void);
gboolean e2_utils_generic_press_cb (GtkWidget *widget, GdkEventButton *event,
	gpointer user_data);
//gboolean e2_utils_generic_release_cb (GtkWidget *widget, GdkEventButton *event,
//	gpointer user_data);
gboolean e2_utils_key_translate_cb (GtkWidget *widget, GdkEventKey *event,
	gpointer user_data);
gboolean e2_utils_check_release (GdkEventButton *event);
gint e2_utils_check_drag (GdkEventButton *event);

#endif //ndef __E2_UTILS_H__
