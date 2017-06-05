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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "intl.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <gtk/gtk.h>

#include <gdk/gdkkeysyms.h> /* for keyboard values */

#include "main.h"
#include "ui_utildlg.h"

#include "filedata.h"
#include "ui_fileops.h"
#include "ui_misc.h"
#include "ui_pathsel.h"
#include "ui_tabcomp.h"
#include "window.h"

/*
 *-----------------------------------------------------------------------------
 * generic dialog
 *-----------------------------------------------------------------------------
 */

void generic_dialog_close(GenericDialog *gd)
{
	gtk_widget_destroy(gd->dialog);
	g_free(gd);
}

static void generic_dialog_click_cb(GtkWidget *widget, gpointer data)
{
	GenericDialog *gd = data;
	void (*func)(GenericDialog *, gpointer);
	gboolean auto_close;

	func = g_object_get_data(G_OBJECT(widget), "dialog_function");
	auto_close = gd->auto_close;

	if (func) func(gd, gd->data);
	if (auto_close) generic_dialog_close(gd);
}

static gboolean generic_dialog_default_key_press_cb(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
	GenericDialog *gd = data;

#if GTK_CHECK_VERSION(2,20,0)
	if (event->keyval == GDK_Return && gtk_widget_has_focus(widget)
#else
	if (event->keyval == GDK_Return && GTK_WIDGET_HAS_FOCUS(widget)
#endif
	    && gd->default_cb)
		{
		gboolean auto_close;

		auto_close = gd->auto_close;
		gd->default_cb(gd, gd->data);
		if (auto_close) generic_dialog_close(gd);

		return TRUE;
		}
	return FALSE;
}

void generic_dialog_attach_default(GenericDialog *gd, GtkWidget *widget)
{
	if (!gd || !widget) return;
	g_signal_connect(G_OBJECT(widget), "key_press_event",
			 G_CALLBACK(generic_dialog_default_key_press_cb), gd);
}

static gboolean generic_dialog_key_press_cb(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
	GenericDialog *gd = data;

	if (event->keyval == GDK_Escape)
		{
		if (gd->cancel_cb) gd->cancel_cb(gd, gd->data);
		if (gd->auto_close) generic_dialog_click_cb(widget, data);
		return TRUE;
		}
	return FALSE;
}

static gboolean generic_dialog_delete_cb(GtkWidget *w, GdkEventAny *event, gpointer data)
{
	GenericDialog *gd = data;
	gboolean auto_close;

	auto_close = gd->auto_close;

	if (gd->cancel_cb) gd->cancel_cb(gd, gd->data);
	if (auto_close) generic_dialog_close(gd);

	return TRUE;
}

static void generic_dialog_show_cb(GtkWidget *widget, gpointer data)
{
	GenericDialog *gd = data;
	if (gd->cancel_button)
		{
		gtk_box_reorder_child(GTK_BOX(gd->hbox), gd->cancel_button, -1);
		}

	g_signal_handlers_disconnect_by_func(G_OBJECT(widget),
					     G_CALLBACK(generic_dialog_show_cb), gd);
}

gboolean generic_dialog_get_alternative_button_order(GtkWidget *widget)
{
	GtkSettings *settings;
	GObjectClass *klass;
	gboolean alternative_order = FALSE;

	settings = gtk_settings_get_for_screen(gtk_widget_get_screen(widget));
	klass = G_OBJECT_CLASS(GTK_SETTINGS_GET_CLASS(settings));
	if (g_object_class_find_property(klass, "gtk-alternative-button-order"))
		{
		g_object_get(settings, "gtk-alternative-button-order", &alternative_order, NULL);
		}

	return alternative_order;
}

GtkWidget *generic_dialog_add_button(GenericDialog *gd, const gchar *stock_id, const gchar *text,
				     void (*func_cb)(GenericDialog *, gpointer), gboolean is_default)
{
	GtkWidget *button;
	gboolean alternative_order;

	button = pref_button_new(NULL, stock_id, text, FALSE,
				 G_CALLBACK(generic_dialog_click_cb), gd);

	GTK_WIDGET_SET_FLAGS(button, GTK_CAN_DEFAULT);
	g_object_set_data(G_OBJECT(button), "dialog_function", func_cb);

	gtk_container_add(GTK_CONTAINER(gd->hbox), button);

	alternative_order = generic_dialog_get_alternative_button_order(gd->hbox);

	if (is_default)
		{
		gtk_widget_grab_default(button);
		gtk_widget_grab_focus(button);
		gd->default_cb = func_cb;

		if (!alternative_order) gtk_box_reorder_child(GTK_BOX(gd->hbox), button, -1);
		}
	else
		{
		if (!alternative_order) gtk_box_reorder_child(GTK_BOX(gd->hbox), button, 0);
		}

	gtk_widget_show(button);

	return button;
}

GtkWidget *generic_dialog_add_message(GenericDialog *gd, const gchar *icon_stock_id,
				      const gchar *heading, const gchar *text)
{
	GtkWidget *hbox;
	GtkWidget *vbox;
	GtkWidget *label;

	hbox = pref_box_new(gd->vbox, TRUE, GTK_ORIENTATION_HORIZONTAL, PREF_PAD_SPACE);
	if (icon_stock_id)
		{
		GtkWidget *image;

		image = gtk_image_new_from_stock(icon_stock_id, GTK_ICON_SIZE_DIALOG);
		gtk_misc_set_alignment(GTK_MISC(image), 0.5, 0.0);
		gtk_box_pack_start(GTK_BOX(hbox), image, FALSE, FALSE, 0);
		gtk_widget_show(image);
		}

	vbox = pref_box_new(hbox, TRUE, GTK_ORIENTATION_VERTICAL, PREF_PAD_SPACE);
	if (heading)
		{
		label = pref_label_new(vbox, heading);
		pref_label_bold(label, TRUE, TRUE);
		gtk_misc_set_alignment(GTK_MISC(label), 0.0, 0.5);
		}
	if (text)
		{
		label = pref_label_new(vbox, text);
		gtk_misc_set_alignment(GTK_MISC(label), 0.0, 0.5);
		gtk_label_set_line_wrap(GTK_LABEL(label), TRUE);
		}

	return vbox;
}

static void generic_dialog_setup(GenericDialog *gd,
				 const gchar *title,
				 const gchar *role,
				 GtkWidget *parent, gboolean auto_close,
				 void (*cancel_cb)(GenericDialog *, gpointer), gpointer data)
{
	GtkWidget *vbox;

	gd->auto_close = auto_close;
	gd->data = data;
	gd->cancel_cb = cancel_cb;

	gd->dialog = window_new(GTK_WINDOW_TOPLEVEL, role, NULL, NULL, title);
	gtk_window_set_type_hint(GTK_WINDOW(gd->dialog), GDK_WINDOW_TYPE_HINT_DIALOG);

	if (parent)
		{
		GtkWindow *window = NULL;

		if (GTK_IS_WINDOW(parent))
			{
			window = GTK_WINDOW(parent);
			}
		else
			{
			GtkWidget *top;

			top = gtk_widget_get_toplevel(parent);
#if GTK_CHECK_VERSION(2,20,0)
			if (GTK_IS_WINDOW(top) && gtk_widget_is_toplevel(top)) window = GTK_WINDOW(top);
#else
			if (GTK_IS_WINDOW(top) && GTK_WIDGET_TOPLEVEL(top)) window = GTK_WINDOW(top);
#endif
			}

		if (window) gtk_window_set_transient_for(GTK_WINDOW(gd->dialog), window);
		}

	g_signal_connect(G_OBJECT(gd->dialog), "delete_event",
			 G_CALLBACK(generic_dialog_delete_cb), gd);
	g_signal_connect(G_OBJECT(gd->dialog), "key_press_event",
			 G_CALLBACK(generic_dialog_key_press_cb), gd);

	gtk_window_set_resizable(GTK_WINDOW(gd->dialog), TRUE);
	gtk_container_set_border_width(GTK_CONTAINER(gd->dialog), PREF_PAD_BORDER);

	vbox = gtk_vbox_new(FALSE, PREF_PAD_BUTTON_SPACE);
	gtk_container_add(GTK_CONTAINER(gd->dialog), vbox);
	gtk_widget_show(vbox);

	gd->vbox = gtk_vbox_new(FALSE, PREF_PAD_GAP);
	gtk_box_pack_start(GTK_BOX(vbox), gd->vbox, TRUE, TRUE, 0);
	gtk_widget_show(gd->vbox);

	gd->hbox = gtk_hbutton_box_new();
	gtk_button_box_set_layout(GTK_BUTTON_BOX(gd->hbox), GTK_BUTTONBOX_END);
	gtk_box_set_spacing(GTK_BOX(gd->hbox), PREF_PAD_BUTTON_GAP);
	gtk_box_pack_start(GTK_BOX(vbox), gd->hbox, FALSE, FALSE, 0);
	gtk_widget_show(gd->hbox);

	if (gd->cancel_cb)
		{
		gd->cancel_button = generic_dialog_add_button(gd, GTK_STOCK_CANCEL, NULL, gd->cancel_cb, TRUE);
		}
	else
		{
		gd->cancel_button = NULL;
		}

	if (generic_dialog_get_alternative_button_order(gd->hbox))
		{
		g_signal_connect(G_OBJECT(gd->dialog), "show",
				 G_CALLBACK(generic_dialog_show_cb), gd);
		}

	gd->default_cb = NULL;
}

GenericDialog *generic_dialog_new(const gchar *title,
				  const gchar *role,
				  GtkWidget *parent, gboolean auto_close,
				  void (*cancel_cb)(GenericDialog *, gpointer), gpointer data)
{
	GenericDialog *gd;

	gd = g_new0(GenericDialog, 1);
	generic_dialog_setup(gd, title, role,
			     parent, auto_close, cancel_cb, data);
	return gd;
}
/*
 *-----------------------------------------------------------------------------
 * simple warning dialog
 *-----------------------------------------------------------------------------
 */

static void warning_dialog_ok_cb(GenericDialog *gd, gpointer data)
{
	/* no op */
}

GenericDialog *warning_dialog(const gchar *heading, const gchar *text,
			      const gchar *icon_stock_id, GtkWidget *parent)
{
	GenericDialog *gd;

	gd = generic_dialog_new(heading, "warning", parent, TRUE, NULL, NULL);
	generic_dialog_add_button(gd, GTK_STOCK_OK, NULL, warning_dialog_ok_cb, TRUE);

	generic_dialog_add_message(gd, icon_stock_id, heading, text);

	gtk_widget_show(gd->dialog);

	return gd;
}

/*
 *-----------------------------------------------------------------------------
 * generic file ops dialog routines
 *-----------------------------------------------------------------------------
 */

void file_dialog_close(FileDialog *fdlg)
{
	file_data_unref(fdlg->source_fd);
	g_free(fdlg->dest_path);
	if (fdlg->source_list) filelist_free(fdlg->source_list);

	generic_dialog_close(GENERIC_DIALOG(fdlg));
}

FileDialog *file_dialog_new(const gchar *title,
			    const gchar *role,
			    GtkWidget *parent,
			    void (*cancel_cb)(FileDialog *, gpointer), gpointer data)
{
	FileDialog *fdlg = NULL;

	fdlg = g_new0(FileDialog, 1);

	generic_dialog_setup(GENERIC_DIALOG(fdlg), title,
			     role, parent, FALSE,
			     (gpointer)cancel_cb, data);

	return fdlg;
}

GtkWidget *file_dialog_add_button(FileDialog *fdlg, const gchar *stock_id, const gchar *text,
				  void (*func_cb)(FileDialog *, gpointer), gboolean is_default)
{
	return generic_dialog_add_button(GENERIC_DIALOG(fdlg), stock_id, text,
					 (gpointer)func_cb, is_default);
}

static void file_dialog_entry_cb(GtkWidget *widget, gpointer data)
{
	FileDialog *fdlg = data;
	g_free(fdlg->dest_path);
	fdlg->dest_path = remove_trailing_slash(gtk_entry_get_text(GTK_ENTRY(fdlg->entry)));
}

static void file_dialog_entry_enter_cb(const gchar *path, gpointer data)
{
	GenericDialog *gd = data;

	file_dialog_entry_cb(NULL, data);

	if (gd->default_cb) gd->default_cb(gd, gd->data);
}

void file_dialog_add_path_widgets(FileDialog *fdlg, const gchar *default_path, const gchar *path,
				  const gchar *history_key, const gchar *filter, const gchar *filter_desc)
{
	GtkWidget *tabcomp;
	GtkWidget *list;

	if (fdlg->entry) return;

	tabcomp = tab_completion_new_with_history(&fdlg->entry, NULL,
		  history_key, -1, file_dialog_entry_enter_cb, fdlg);
	gtk_box_pack_end(GTK_BOX(GENERIC_DIALOG(fdlg)->vbox), tabcomp, FALSE, FALSE, 0);
	generic_dialog_attach_default(GENERIC_DIALOG(fdlg), fdlg->entry);
	gtk_widget_show(tabcomp);

	if (path && path[0] == G_DIR_SEPARATOR)
		{
		fdlg->dest_path = g_strdup(path);
		}
	else
		{
		const gchar *base;

		base = tab_completion_set_to_last_history(fdlg->entry);

		if (!base) base = default_path;
		if (!base) base = homedir();

		if (path)
			{
			fdlg->dest_path = g_build_filename(base, path, NULL);
			}
		else
			{
			fdlg->dest_path = g_strdup(base);
			}
		}

	list = path_selection_new_with_files(fdlg->entry, fdlg->dest_path, filter, filter_desc);
	path_selection_add_select_func(fdlg->entry, file_dialog_entry_enter_cb, fdlg);
	gtk_box_pack_end(GTK_BOX(GENERIC_DIALOG(fdlg)->vbox), list, TRUE, TRUE, 0);
	gtk_widget_show(list);

	gtk_widget_grab_focus(fdlg->entry);
	if (fdlg->dest_path)
		{
		gtk_entry_set_text(GTK_ENTRY(fdlg->entry), fdlg->dest_path);
		gtk_editable_set_position(GTK_EDITABLE(fdlg->entry), strlen(fdlg->dest_path));
		}

	g_signal_connect(G_OBJECT(fdlg->entry), "changed",
			 G_CALLBACK(file_dialog_entry_cb), fdlg);
}

void file_dialog_add_filter(FileDialog *fdlg, const gchar *filter, const gchar *filter_desc, gboolean set)
{
	if (!fdlg->entry) return;
	path_selection_add_filter(fdlg->entry, filter, filter_desc, set);
}

void file_dialog_clear_filter(FileDialog *fdlg)
{
	if (!fdlg->entry) return;
	path_selection_clear_filter(fdlg->entry);
}

void file_dialog_sync_history(FileDialog *fdlg, gboolean dir_only)
{
	if (!fdlg->dest_path) return;

	if (!dir_only ||
	    (dir_only && isdir(fdlg->dest_path)) )
		{
		tab_completion_append_to_history(fdlg->entry, fdlg->dest_path);
		}
	else
		{
		gchar *buf = remove_level_from_path(fdlg->dest_path);
		tab_completion_append_to_history(fdlg->entry, buf);
		g_free(buf);
		}
}
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
