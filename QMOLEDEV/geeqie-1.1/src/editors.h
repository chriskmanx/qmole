/*
 * Geeqie
 * (C) 2004 John Ellis
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * Author: John Ellis
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */


#ifndef EDITORS_H
#define EDITORS_H


typedef enum {
	EDITOR_KEEP_FS            = 0x00000001,
	EDITOR_VERBOSE            = 0x00000002,
	EDITOR_VERBOSE_MULTI      = 0x00000004,
	EDITOR_TERMINAL		  = 0x00000008,

	EDITOR_DEST               = 0x00000100,
	EDITOR_FOR_EACH           = 0x00000200,
	EDITOR_SINGLE_COMMAND     = 0x00000400,
	EDITOR_NO_PARAM           = 0x00000800,
	/* below are errors */
	EDITOR_ERROR_EMPTY        = 0x00020000,
	EDITOR_ERROR_SYNTAX       = 0x00040000,
	EDITOR_ERROR_INCOMPATIBLE = 0x00080000,
	EDITOR_ERROR_NO_FILE      = 0x00100000,
	EDITOR_ERROR_CANT_EXEC    = 0x00200000,
	EDITOR_ERROR_STATUS       = 0x00400000,
	EDITOR_ERROR_SKIPPED      = 0x00800000,
	/* mask to match errors only */
	EDITOR_ERROR_MASK         = 0xffff0000,
} EditorFlags;

struct _EditorDescription {
	gchar *key; 		/* desktop file name, not including path, including extension */
	gchar *name; 		/* Name, localized name presented to user */
	gchar *icon;		/* Icon */
	gchar *exec;		/* Exec */
	gchar *menu_path;	
	gchar *hotkey;
	GList *ext_list;
	gchar *file;
	gchar *comment;		/* .desktop Comment key, used to show a tooltip */
	EditorFlags flags;
	gboolean hidden;	/* explicitly hidden, shown in configuration dialog */
	gboolean ignored;	/* not interesting, do not show at all */
};

#define EDITOR_ERRORS(flags) ((flags) & EDITOR_ERROR_MASK)
#define EDITOR_ERRORS_BUT_SKIPPED(flags) (!!(((flags) & EDITOR_ERROR_MASK) && !((flags) & EDITOR_ERROR_SKIPPED)))


/* return values from callback function */
enum {
	EDITOR_CB_CONTINUE = 0, /* continue multiple editor execution on remaining files*/
	EDITOR_CB_SKIP,         /* skip the remaining files */
	EDITOR_CB_SUSPEND       /* suspend execution, one of editor_resume or editor_skip
				   must be called later */
};

enum {
	DESKTOP_FILE_COLUMN_KEY,
	DESKTOP_FILE_COLUMN_NAME,
	DESKTOP_FILE_COLUMN_HIDDEN,
	DESKTOP_FILE_COLUMN_WRITABLE,
	DESKTOP_FILE_COLUMN_PATH,
	DESKTOP_FILE_COLUMN_COUNT
};

extern GtkListStore *desktop_file_list;


extern GHashTable *editors;

void editor_table_finish(void);
void editor_table_clear(void);
GList *editor_get_desktop_files(void);
gboolean editor_read_desktop_file(const gchar *path);

GList *editor_list_get(void);


/*
Callback is called even on skipped files, with the EDITOR_ERROR_SKIPPED flag set.
It is a good place to call file_data_change_info_free().

ed - pointer that can be used for editor_resume/editor_skip or NULL if all files were already processed
flags - flags above
list - list of procesed FileData structures, typically single file or whole list passed to start_editor_*
data - generic pointer
*/
typedef gint (*EditorCallback) (gpointer ed, EditorFlags flags, GList *list, gpointer data);


void editor_resume(gpointer ed);
void editor_skip(gpointer ed);



EditorFlags start_editor(const gchar *key, const gchar *working_directory);
EditorFlags start_editor_from_file(const gchar *key, FileData *fd);
EditorFlags start_editor_from_filelist(const gchar *key, GList *list);
EditorFlags start_editor_from_file_full(const gchar *key, FileData *fd, EditorCallback cb, gpointer data);
EditorFlags start_editor_from_filelist_full(const gchar *key, GList *list, const gchar *working_directory, EditorCallback cb, gpointer data);
gboolean editor_window_flag_set(const gchar *key);
gboolean editor_is_filter(const gchar *key);
gboolean editor_no_param(const gchar *key);
const gchar *editor_get_error_str(EditorFlags flags);

const gchar *editor_get_name(const gchar *key);

gboolean is_valid_editor_command(const gchar *key);
gboolean editor_blocks_file(const gchar *key);

EditorFlags editor_command_parse(const EditorDescription *editor, GList *list, gboolean consider_sidecars, gchar **output);

#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
