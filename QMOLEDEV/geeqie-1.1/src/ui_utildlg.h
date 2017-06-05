/*
 * (SLIK) SimpLIstic sKin functions
 * (C) 2004 John Ellis
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * Author: John Ellis
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */

#ifndef UI_UTILDLG_H
#define UI_UTILDLG_H


#define GENERIC_DIALOG(gd) ((GenericDialog *)gd)

typedef struct _GenericDialog GenericDialog;
struct _GenericDialog
{
	GtkWidget *dialog;	/* window */
	GtkWidget *vbox;	/* place to add widgets */

	GtkWidget *hbox;	/* button hbox */

	gboolean auto_close;

	void (*default_cb)(GenericDialog *, gpointer);
	void (*cancel_cb)(GenericDialog *, gpointer);
	gpointer data;

	/* private */
	GtkWidget *cancel_button;
};

typedef struct _FileDialog FileDialog;
struct _FileDialog
{
	GenericDialog gd;

	GtkWidget *entry;

	gint type;

	FileData *source_fd;
	GList *source_list;

	gchar *dest_path;
};


/* When parent is not NULL, the dialog is set as a transient of the window containing parent */
GenericDialog *generic_dialog_new(const gchar *title,
				  const gchar *role,
				  GtkWidget *parent, gboolean auto_close,
				  void (*cancel_cb)(GenericDialog *, gpointer), gpointer data);
void generic_dialog_close(GenericDialog *gd);

GtkWidget *generic_dialog_add_button(GenericDialog *gd, const gchar *stock_id, const gchar *text,
				     void (*func_cb)(GenericDialog *, gpointer), gboolean is_default);
void generic_dialog_attach_default(GenericDialog *gd, GtkWidget *widget);

GtkWidget *generic_dialog_add_message(GenericDialog *gd, const gchar *icon_stock_id,
				      const gchar *heading, const gchar *text);

gboolean generic_dialog_get_alternative_button_order(GtkWidget *widget);

GenericDialog *warning_dialog(const gchar *heading, const gchar *text,
			      const gchar *icon_stock_id, GtkWidget *parent);

FileDialog *file_dialog_new(const gchar *title,
			    const gchar *role,
			    GtkWidget *parent,
			    void (*cancel_cb)(FileDialog *, gpointer), gpointer data);
void file_dialog_close(FileDialog *fd);

GtkWidget *file_dialog_add_button(FileDialog *fd, const gchar *stock_id, const gchar *text,
				  void (*func_cb)(FileDialog *, gpointer), gboolean is_default);

/* default_path is default base directory, and is only used if no history
 *  exists for history_key (HOME is used if default_path is NULL).
 * path can be a full path or only a file name. If name only, appended to
 *  the default_path or the last history (see default_path)
 */
void file_dialog_add_path_widgets(FileDialog *fd, const gchar *default_path, const gchar *path,
				  const gchar *history_key, const gchar *filter, const gchar *filter_desc);

void file_dialog_add_filter(FileDialog *fd, const gchar *filter, const gchar *filter_desc, gboolean set);
void file_dialog_clear_filter(FileDialog *fd);
void file_dialog_sync_history(FileDialog *fd, gboolean dir_only);


#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
