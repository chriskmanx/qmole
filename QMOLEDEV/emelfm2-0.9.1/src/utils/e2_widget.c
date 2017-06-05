/* $Id: e2_widget.c 3001 2014-01-17 23:05:07Z tpgww $

Copyright (C) 2003-2014 tooar <tooar@emelfm2.net>
Portions copyright (C) 1999 Michael Clark.

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

/**
@file src/utils/e2_widget.c
@brief miscelleanous GtkWidget utility functions

This file contains various miscelleanous GtkWidget utility functions.
*/

#include "emelfm2.h"
#include <string.h>
#include "e2_widget.h"
#include "e2_dialog.h"
#include "e2_icons.h"

#ifdef USE_GTK2_12TIPS
typedef struct
{
	gboolean oneactive;
	gchar *tip1;
	gchar *tip2;
} E2_TipsToggle;

static void _e2_widget_destroy_ttips (E2_TipsToggle *data)
{
	g_free (data->tip1);
	g_free (data->tip2);
	DEMALLOCATE (E2_TipsToggle, data);
}
/**
@brief callback for "query-tooltip" signal on tipped widgets
@param widget the widget which was moused etc
@param x UNUSED
@param y UNUSED
@param keyboard_mode UNUSED
@param tooltip where to set the tip
@param user_data UNUSED pointer to data specified when signal was connected

@return TRUE to show the tip
*/
static gboolean _e2_widget_show_tip_cb (GtkWidget  *widget,
									gint        x,
									gint        y,
									gboolean    keyboard_mode,
									GtkTooltip *tooltip,
									gpointer    user_data)
{
	gchar *text = (gchar *) g_object_get_data (G_OBJECT (widget), "_e2-tiptext");
	if (text != NULL && tooltip != NULL)
	{
		NEEDCLOSEBGL
		gtk_tooltip_set_text (tooltip, text);
		NEEDOPENBGL
		return TRUE;
	}
	return FALSE;
}
/**
@brief idle-callback to add or remove a tip with BGL safely closed
@param widget the widget whose tip is to be altered
@return FALSE always, to cancel the source
*/
static gboolean _e2_widget_defer_tip (GtkWidget *widget)
{
	gpointer tip = g_object_get_data (G_OBJECT (widget), "_e2-tiptext");
	CLOSEBGL
	gtk_widget_set_has_tooltip (widget, (tip != NULL));
	OPENBGL
	if (tip != NULL)
		g_signal_connect (G_OBJECT (widget), "query-tooltip",
			G_CALLBACK(_e2_widget_show_tip_cb), NULL);
	else
		g_signal_handlers_disconnect_by_func ((gpointer)widget,
			_e2_widget_show_tip_cb, NULL);
	return FALSE;
}
/**
@brief set tooltip @a tiptext for @a widget
@param widget the item to which the tip is to be applied
@param tiptext UTF8 non-markup text, or NULL to disable tip
@return
*/
void e2_widget_set_safetip (GtkWidget *widget, const gchar *tiptext)
{
	if (tiptext != NULL && *tiptext != '\0')
		g_object_set_data_full (G_OBJECT (widget), "_e2-tiptext", g_strdup(tiptext), g_free);
	else
		g_object_set_data (G_OBJECT (widget), "_e2-tiptext", NULL);
	g_idle_add ((GSourceFunc) _e2_widget_defer_tip, widget);
}
/**
@brief setup for a button to have 2 alternate tooltips

@param widget the button widget
@param initialtip initially-displayed tip, utf-8, my have markup
@param othertip alternate tip, utf-8, my have markup

@return
*/
void e2_widget_set_toggletip (GtkWidget *widget, const gchar *initialtip,
	const gchar *othertip)
{
//a tooltip window doesn't exist until the tip is ready to show
	E2_TipsToggle *data = MALLOCATE (E2_TipsToggle);
	if (data != NULL)
	{
		data->tip1 = g_strdup (initialtip);
		data->tip2 = g_strdup (othertip);
		data->oneactive = TRUE;
		g_object_set_data_full (G_OBJECT (widget), "_e2-tips-data", data,
			(GDestroyNotify) _e2_widget_destroy_ttips);
	}
	e2_widget_set_safetip (widget, initialtip);
}
#else //pre-2.12 form of tip-handling
/**
@brief set tooltips @a tiptext1 (main) and @a tiptext2 (private) for @a widget
@param widget the item to which the tip is to be applied
@param tiptext1 UTF8 non-markup text for primary tip, or NULL
@param tiptext2 UTF8 non-markup text for private tip, or NULL

@return
*/
void e2_widget_set_tip (GtkWidget *widget, const gchar *tiptext1, const gchar *tiptext2)
{
	/* gtk >= 2.12 (downstream) uses gtk_widget_set_tooltip_text(), and for
	   gtk < 2.16.3 that synchronously accesses X server, so needs BGL closed */
	gboolean close = (app.gtkversion >= 21200 //revised tips-code is in play from 2.12.0
				   && app.gtkversion < 21603);//tip-setting mechanism adjusted in 2.16.3
	if (close)
	{
#ifdef NATIVE_BGL
		printd (WARN, "Closing BGL with default mutex will sometimes be recursively bad");
		CLOSEBGL
#else
# ifdef DEBUG_MESSAGES
		//FIXME find a way to make errorcheck mutex block, to avoid having to abort
		gint _lockres = pthread_mutex_trylock (&display_mutex);
		if (_lockres != 0)
		{
			printd (DEBUG, "%s BGL trylock result %d", __PRETTY_FUNCTION__, _lockres);
			if (_lockres != EBUSY)
			{
				printd (DEBUG, "Close BGL failed, ignoring tooltip request");
				return;
			}
		}
# else
		//recursive mutex is more tolerant
		CLOSEBGL
# endif
#endif
	}
/*	else
		printd (DEBUG, "No closing BGL with gtk version %d", app.gtkversion);
*/
	gtk_tooltips_set_tip (app.tooltips, widget, tiptext1, tiptext2);

	if (close)
		OPENBGL
}
#endif //def USE_GTK2_12TIPS

/**
@brief toggle the visiblity of 2 tip labels for a toggle-button

@param widget the button widget

@return
*/
void e2_widget_swap_tooltip (GtkWidget *widget)
{
#ifdef USE_GTK2_12TIPS
	//use data setup by e2_widget_set_toggletip()
	E2_TipsToggle *data = g_object_get_data (G_OBJECT (widget), "btn-toggle-data");
	if (data != NULL)
	{
		gchar *newtip = (data->oneactive) ? data->tip2 : data->tip1;
		if (G_LIKELY (newtip != NULL))
		{
			gtk_widget_set_has_tooltip (widget, TRUE);
			g_object_set_data_full (G_OBJECT (widget), "_e2-tiptext", g_strdup(newtip), g_free);
		}
		else
		{
			gtk_widget_set_has_tooltip (widget, FALSE);
			g_object_set_data (G_OBJECT (widget), "_e2-tiptext", NULL);
		}
		data->oneactive = !data->oneactive;
	}
#else
	GtkTooltipsData *tips = gtk_tooltips_data_get (widget);
	if (app.gtkversion >= 21200)
		e2_widget_set_tip (widget, tips->tip_private, tips->tip_text);
	else
	{
		gchar *tmp = tips->tip_text;
		tips->tip_text = tips->tip_private;
		tips->tip_private = tmp;
	}
#endif
}
/**
@brief get image widget for an icon

@param icon gtk-stock-item name, or localised custom-icon filename with or without path
@param size enumerator of the desired image size

@return image widget, or NULL
*/
GtkWidget *e2_widget_get_icon (const gchar *icon, GtkIconSize size)
{
	if (icon == NULL || *icon == '\0')
		return NULL;

#ifdef E2_ICONCACHE
	GdkPixbuf *pxb = e2_icons_get_puxbuf (icon, size, TRUE);
	if (pxb != NULL)
		return (gtk_image_new_from_pixbuf (pxb));
	return NULL;
#else
	GtkWidget *image;
	if (e2_icons_check_stock (icon))
		image = gtk_image_new_from_icon_name (icon, size);
	else
	{
		GdkPixbuf *pixbuf;
		gchar *fullname;
		if (g_path_is_absolute (icon))
			fullname = icon;
		else
		{
			gchar *localpath = e2_icons_get_custom_path (TRUE);
			fullname = e2_utils_strcat (localpath, icon);
			g_free (localpath);
		}

		if ((pixbuf = gdk_pixbuf_new_from_file (fullname, NULL)) != NULL)
		{
			GdkPixbuf *pixbuf2;
			gint width, height;
			if (!gtk_icon_size_lookup (size, &width, &height))
			{
				width = 16; height = 16; //unlikely default to minimum size
			}
			pixbuf2 = gdk_pixbuf_scale_simple (pixbuf, width, height,
				GDK_INTERP_BILINEAR);
			image = gtk_image_new_from_pixbuf (pixbuf2);
			g_object_unref (pixbuf2);
			g_object_unref (pixbuf);
		}
		else
			image = NULL;

		if (fullname != icon)
			g_free (fullname);
	}
	return image;
#endif
}
/**
@brief create a vertically-centred GtkLabel and add it to @a table at specified position

@param table the table to attach the new label into
@param text the text for the new label, may have markup
@param xalign the label's horizontal alignment
@param left index of column to left of attach-position
@param right index of column to right of attach-position
@param top index of row above attach-position
@param bottom index of row below attach-position

@return  the label widget
*/
GtkWidget *e2_widget_add_mid_label_to_table (GtkWidget *table, gchar *text,
	gfloat xalign, gint left, gint right, gint top, gint bottom)
{
	GtkWidget *label;
	label = gtk_label_new (NULL);
	gtk_label_set_markup (GTK_LABEL (label), text);
	gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
	gtk_misc_set_alignment (GTK_MISC (label), xalign, 0.5);
//	gtk_label_set_selectable (GTK_LABEL (label), FALSE);
#ifdef USE_GTK3_2
	gtk_grid_attach (GTK_GRID (table), label, left, top, right-left, bottom-top);
#else
	gtk_table_attach_defaults (GTK_TABLE (table), label, left, right, top, bottom);
#endif
	return label;
}
/**
@brief create a vertically-centred GtkLabel and if @a box is non-NULL, pack the entry into that

@param box a box widget to hold the label, or NULL
@param text the text for the new label, may have markup
@param xalign the label's horizontal alignment
@param exp expandable property for packing the label into @a box
@param pad padding for packing the label into @a box

@return  the label widget
*/
GtkWidget *e2_widget_add_mid_label (GtkWidget *box, const gchar *text,
	gfloat xalign, gboolean exp, guint pad)
{
	return e2_widget_add_label (box, text, xalign, 0.5, exp, pad);
}
/**
@brief create a GtkLabel and if @a box is non-NULL, pack the entry into that

@param box a box widget to hold the label, or NULL
@param text the text for the new label, may have markup
@param xalign the label's horizontal alignment
@param yalign the label's vertical alignment
@param exp expandable property for packing the label into @a box
@param pad padding for packing the label into @a box

@return  the label widget
*/
GtkWidget *e2_widget_add_label (GtkWidget *box, const gchar *text,
	gfloat xalign, gfloat yalign, gboolean exp, guint pad)
{
	GtkWidget *label = gtk_label_new (NULL);
	gtk_label_set_markup (GTK_LABEL (label), text);
//	gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
	gtk_misc_set_alignment (GTK_MISC (label), xalign, yalign);
	gtk_label_set_line_wrap (GTK_LABEL (label), TRUE);
	gtk_label_set_selectable (GTK_LABEL (label), FALSE);
	if (box != NULL)
		gtk_box_pack_start (GTK_BOX (box), label, exp, TRUE, pad);
	return label;
}
/**
@brief callback for entry widget key-press, to shorten string in @a entry
<Shift>Delete removes from cursor to end
@param entry the entry widget
@param event pointer to event data struct
@param user_data UNUSED data specified when callback was connnected

@return TRUE if the key was <Shift>Delete
*/
static gboolean _e2_widget_entry_keypress_cb (GtkWidget *entry, GdkEventKey *event,
	gpointer user_data)
{
	OPENBGL //TODO do this if NEEDCLOSEBGL is empty
	//prevent sporadic bad latency in UI updates
	gtk_widget_queue_draw (entry);
	CLOSEBGL

	NEEDCLOSEBGLX //do this if NEEDCLOSEBGL is empty
	if (event->keyval == GDK_Delete)
	{
		guint modifiers = gtk_accelerator_get_default_mod_mask ();
		if ((event->state & modifiers) == GDK_SHIFT_MASK)
		{
			gint start = gtk_editable_get_position (GTK_EDITABLE (entry));
			gtk_editable_delete_text (GTK_EDITABLE (entry), start, -1);
			NEEDOPENBGL
			return TRUE;
		}
	}
	NEEDOPENBGL
	return FALSE;
}
/**
@brief create a GtkEntry and if @a box is non-NULL, pack the entry into that

@param box a box widget to hold the entry, or NULL
@param init_text the initial text for the new entry, or NULL
@param exp expandable property for packing the entry into @a box
@param select_text TRUE to select the initial text in the entry

@return the entry widget
*/
GtkWidget *e2_widget_add_entry (GtkWidget *box, gchar *init_text,
	gboolean exp, gboolean select_text)
{
	GtkWidget *entry;
	entry = gtk_entry_new ();
	if (init_text != NULL)
	{
		gtk_entry_set_text (GTK_ENTRY (entry), init_text);
		if (select_text)
			gtk_editable_select_region (GTK_EDITABLE (entry), 0, -1);
	}
	gtk_entry_set_activates_default (GTK_ENTRY (entry), TRUE);
	g_signal_connect (G_OBJECT (entry), "key-press-event",
		G_CALLBACK (_e2_widget_entry_keypress_cb), NULL);
	if (box != NULL)
		gtk_box_pack_start (GTK_BOX (box), entry, exp, TRUE, E2_PADDING_XSMALL);
	gtk_widget_grab_focus (entry);

	return entry;
}
/* *
@brief create a GtkEntry and add it to @a table

@param table the table to attach the new entry into
@param init_text the initial text for the new entry, or NULL
@param select_text TRUE to select the initial text in the entry
@param left index of column to left of attach-position
@param right index of column to right of attach-position
@param top index of row above attach-position
@param bottom index of row below attach-position

@return the entry widget
*/
/*UNUSED
GtkWidget *e2_widget_add_entry_to_table (GtkWidget *table,
	gchar *init_text, gboolean select_text
	gint left, gint right, gint top, gint bottom)
{
	GtkWidget *entry;
	entry = gtk_entry_new ();
	if (init_text != NULL)
	{
		gtk_entry_set_text (GTK_ENTRY (entry), init_text);
		if (select_text)
			gtk_editable_select_region (GTK_EDITABLE (entry), 0, -1);
	}
	g_signal_connect (G_OBJECT (entry), "key-press-event",
		G_CALLBACK (_e2_widget_entry_keypress_cb), NULL);
#ifdef USE_GTK3_2
	gtk_grid_attach (GTK_GRID (table), entry, left, top, right-left, bottom-top);
#else
	gtk_table_attach_defaults (GTK_TABLE (table), entry, left, right, top, bottom);
#endif
	return entry;
}
*/
/**
@brief create a horizontal or vertical box widget

@param vertical TRUE for a vbox, FALSE for a hbox
@param homogen the new box's "homogenous" parameter
@param spacing the new box's "spacing" parameter

@return the widget, unshown
*/
GtkWidget *e2_widget_get_box (gboolean vertical, gboolean homogen, gint spacing)
{
#ifdef USE_GTK3_0
	GtkWidget *box = (vertical) ?
		gtk_box_new (GTK_ORIENTATION_VERTICAL, spacing):
		gtk_box_new (GTK_ORIENTATION_HORIZONTAL, spacing);
	if (homogen)
		gtk_box_set_homogeneous ((GtkBox*)box, TRUE);
#else
	GtkWidget *box = (vertical) ?
		gtk_vbox_new (homogen, spacing):
		gtk_hbox_new (homogen, spacing);
#endif
	return box;
}
/**
@brief create a horizontal or vertical box widget, and pack it into @a parentbox

@param parentbox the parent box to pack the new box into
@param exp expandable property for packing the box into @a parentbox
@param pad padding for packing the box into @a parentbox
@param vertical TRUE for a vbox, FALSE for a hbox
@param homogen the new box's "homogenous" parameter
@param spacing the new box's "spacing" parameter

@return the new box widget
*/
GtkWidget *e2_widget_add_box (GtkWidget *parentbox, gboolean exp, guint pad,
	gboolean vertical, gboolean homogen, gint spacing)
{
	GtkWidget *box = e2_widget_get_box (vertical, homogen, spacing);
	gtk_box_pack_start (GTK_BOX (parentbox), box, exp, TRUE, pad);
	return box;
}
/**
@brief create a GtkScrolledWindow without a shadow

@param h_policy  scrolled window horizontal scrolling policy
@param v_policy  scrolled window vertical scrolling policy

@return  the scrolled window widget, unshown
*/
GtkWidget *e2_widget_get_sw_plain (GtkPolicyType h_policy, GtkPolicyType v_policy)
{
	return e2_widget_get_sw (h_policy, v_policy, GTK_SHADOW_NONE);
}
/**
@brief create a GtkScrolledWindow

This function creates a GtkScrolledWindow. The parameters @a h_policy and
@a v_policy are used to set the scrolling policy of the scrolled window.
The shadow type is set to @a shadow.

@param h_policy  scrolled window horizontal scrolling policy
@param v_policy  scrolled window vertical scrolling policy
@param shadow  scrolled window shadow type

@return the scrolled window widget, unshown
*/
GtkWidget *e2_widget_get_sw (GtkPolicyType h_policy, GtkPolicyType v_policy,
	GtkShadowType shadow)
{
 	GtkWidget *scrolled_window = gtk_scrolled_window_new (NULL, NULL);
	//set bars positions even for policy none, that may change later
	gtk_scrolled_window_set_placement (GTK_SCROLLED_WINDOW (scrolled_window),
		e2_option_sel_get ("scrollbar-position"));
	gtk_scrolled_window_set_policy ((GtkScrolledWindow*) scrolled_window,
		h_policy, v_policy);
	gtk_scrolled_window_set_shadow_type ((GtkScrolledWindow*) scrolled_window,
		shadow);
//#ifdef USE_GTK3_4
//	gtk_scrolled_window_set_kinetic_scrolling (scrolled_window, FALSE);
//#endif

 	return scrolled_window;
}
/**
@brief create a GtkScrolledWindow and pack it into start of @a box

This function creates a GtkScrolledWindow using e2_widget_get_sw_plain() and
adds it to @a box. The parameters @a h_policy and @a v_policy are used as
arguments for e2_widget_get_sw_plain(). @a exp and @a pad are used as arguments
for gtk_box_pack_start() to adjust the packing of the scrolled window.
The shadow type is gtk-default (GTK_SHADOW_NONE).
@param box the parent box to pack the scrolled window to
@param h_policy  scrolled window horizontal scrolling policy
@param v_policy  scrolled window vertical scrolling policy
@param exp what to do with additional free space in the box
@param pad number of pixels to pad the scrolled window in the box

@return  the scrolled window
*/
GtkWidget *e2_widget_add_sw (GtkWidget *box, GtkPolicyType h_policy,
	GtkPolicyType v_policy, gboolean exp, guint pad)
{
	GtkWidget *scrolled_window = e2_widget_get_sw_plain (h_policy, v_policy);
	gtk_box_pack_start (GTK_BOX (box), scrolled_window, exp, TRUE, pad);
	return scrolled_window;
}
/**
@brief add a child with a viewport to a GtkScrolledWindow

In contrast to the @a e2_widget_[add|get]* functions, this function does not
create a scrolled window. It only adds a viewport to the scrolled window
@a sw and then adds @a child to the viewport. his is done by calling
gtk_scrolled_window_add_with_viewport(). The shadow type of the viewport is
set to GTK_SHADOW_NONE.

@param sw  the scrolled window
@param child  the child that should be added to the scrolled window

@return
*/
void e2_widget_sw_add_with_viewport (GtkWidget *sw, GtkWidget *child)
{
#ifdef USE_GTK3_8
	gtk_container_add (GTK_CONTAINER (sw), child);
#else
	gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW (sw), child);
#endif
	gtk_viewport_set_shadow_type (
#ifdef USE_GTK2_14
		GTK_VIEWPORT (gtk_bin_get_child (GTK_BIN (sw))),
#else
		GTK_VIEWPORT (GTK_BIN (sw)->child),
#endif
		GTK_SHADOW_NONE);
}
/**
@brief create a horizontal or vertical separator widget, and pack it into @a box

@param box the box to pack the new separator into, may be NULL
@param exp expandable property for packing the separator into @a box
@param pad padding for packing the separator into @a box

@return the separator widget (horizontal-type if @a box is NULL)
*/
GtkWidget *e2_widget_add_separator (GtkWidget *box, gboolean exp, guint pad)
{
	GtkWidget *sep;
#ifdef USE_GTK3_2
	if (box != NULL &&
		gtk_orientable_get_orientation (GTK_ORIENTABLE (box)) == GTK_ORIENTATION_HORIZONTAL)
		sep = gtk_separator_new (GTK_ORIENTATION_VERTICAL);
	else
		sep = gtk_separator_new (GTK_ORIENTATION_HORIZONTAL);
#else
	if (GTK_IS_HBOX (box))
		sep = gtk_vseparator_new ();
	else if (box == NULL || GTK_IS_VBOX (box))
		sep = gtk_hseparator_new ();
	else
		return NULL;
#endif
	if (box != NULL)
		gtk_box_pack_start (GTK_BOX (box), sep, exp, TRUE, pad);
	return sep;
}
/**
@brief create a table widget, and pack it into @a box

@param box the box to pack the new separator into, may be NULL
@param rows the no. of rows for the table
@param cols the no. of columns for the table
@param homogen TRUE if all table cells are to be resized to the size of the cell containing the largest widget
@param exp expandable property for packing the separator into @a box
@param pad padding for packing the separator into @a box

@return the table widget
*/
GtkWidget *e2_widget_add_table (GtkWidget *box, gint rows, gint cols,
	gboolean homogen, gboolean exp, guint pad)
{
	GtkWidget *table;
#ifdef USE_GTK3_2
	gint i;
	table = gtk_grid_new ();
	if (homogen)
		gtk_grid_set_column_homogeneous ((GtkGrid *)table, TRUE);
	for (i = 0; i < cols; i++)
		gtk_grid_insert_column ((GtkGrid *)table, i);
	for (i = 0; i < rows; i++)
		gtk_grid_insert_row ((GtkGrid *)table, i);
#else
	table = gtk_table_new (rows, cols, homogen);
#endif
	if (box != NULL)
		gtk_box_pack_start (GTK_BOX (box), table, exp, TRUE, pad);
	return table;
}
/*UNUSED
GtkWidget *e2_widget_add_framed_table (GtkWidget *box, gchar *title, gint rows, gint cols,
	gboolean exp, guint pad)
{
	GtkWidget *frame, *table;

	frame = gtk_frame_new (title);
	gtk_box_pack_start (GTK_BOX (box), frame, exp, TRUE, pad);

#ifdef USE_GTK3_2
	gint i;
	table = gtk_grid_new ();
	for (i = 0; i < cols; i++)
		gtk_grid_insert_column ((GtkGrid *)table, i);
	for (i = 0; i < rows; i++)
		gtk_grid_insert_row ((GtkGrid *)table, i);
	gtk_grid_set_row_spacing ((GtkGrid *)table, E2_PADDING);
#else
	table = gtk_table_new (rows, cols, FALSE);
	gtk_table_set_row_spacings (GTK_TABLE (table), E2_PADDING);
#endif
	gtk_container_set_border_width (GTK_CONTAINER (table), E2_PADDING_XSMALL);
	gtk_container_add (GTK_CONTAINER (frame), table);
	return table;
}
UNUSED
GtkWidget *e2_widget_add_framed_widget (GtkWidget *box, gchar *title,
	GtkWidget *widget, gboolean exp, guint pad)
{
	GtkWidget *frame;

	frame = gtk_frame_new (title);
	gtk_box_pack_start (GTK_BOX (box), frame, exp, TRUE, pad);
	gtk_container_add (GTK_CONTAINER (frame), widget);
	return frame;
}
*/
GtkWidget *e2_widget_add_frame (GtkWidget *box, gboolean fill, guint pad,
	gchar *title, gboolean stretch)
{
	gchar *real_title = NULL;
	if (title != NULL)
		real_title = g_strconcat (" ", title, " ", NULL);
	GtkWidget *frame = gtk_frame_new (real_title);
	if (title != NULL)
	{
		g_free (real_title);
		GtkWidget *label = gtk_frame_get_label_widget (GTK_FRAME (frame));
		gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
	}
	gtk_box_pack_start (GTK_BOX (box), frame, fill, fill, pad);
	return frame;
}

GtkWidget *e2_widget_add_eventbox (GtkWidget *box, gboolean fill, guint pad)
{
	GtkWidget *event = gtk_event_box_new ();
	gtk_box_pack_start (GTK_BOX (box), event, fill, fill, pad);
	return event;
}
/**
@brief create a new GtkNotebook

@param func the switch-page callback function or NULL
@param data data pointer for the callback function

@return the notebook widget, unshown
*/
GtkWidget *e2_widget_get_notebook (
	void (*func)(/*GtkNotebook*,GtkNotebookPage*(2)|GtkWidget*(3),guint,gpointer*/), gpointer data)
{
	GtkWidget *notebook = gtk_notebook_new ();
	if (func != NULL)
		g_signal_connect (G_OBJECT (notebook), "switch-page", G_CALLBACK (func), data);
	return notebook;
}
/**
@brief create a notebook and add it to @a box

@param box the box to add the notebook to
@param fill fill parameter for the box child
@param pad pad parameter for the box child
@param func the switch-page callback function for the notebook or NULL
@param data data pointer for the callback function

@return the notebook widget, unshown
*/
GtkWidget *e2_widget_add_notebook (GtkWidget *box, gboolean fill, guint pad,
	void (*func)(/*GtkNotebook*,GtkNotebookPage*(2)|GtkWidget*(3),guint,gpointer*/), gpointer data)
{
	GtkWidget *notebook = e2_widget_get_notebook (func, data);
	gtk_box_pack_start (GTK_BOX (box), notebook, fill, fill, pad);
	return notebook;
}
/**
@brief create a notebook page and add it to @a notebook
In order to make the initial displayed size of the notebook reflect the size of
its largest child, may wish to make @a swpolicy GTK_POLICY_NEVER, and change it
after notebook is sized, e.g. somewhere else:
	g_signal_connect (G_OBJECT (GTK_DIALOG (dialog)), "show",
		G_CALLBACK (e2_dialog_show_notebook_cb), GTK_NOTEBOOK (book));
@param notebook the notebook widget
@param tabname string with name for the tab
@param swpolicy scrollbar policy applied to scrolled window in the tab

@return vbox widget into which tab content can be packed
*/
GtkWidget *e2_widget_add_notebook_page (GtkWidget *notebook, gchar *tabname,
	GtkPolicyType swpolicy)
{
	GtkWidget *outerbox = e2_widget_get_box (TRUE, FALSE, 0);
	GtkWidget *label = gtk_label_new (tabname);
	gtk_notebook_append_page (GTK_NOTEBOOK (notebook), outerbox, label);
	GtkWidget *scrolled = e2_widget_add_sw (outerbox, swpolicy, swpolicy, TRUE, 0);
	//in case of later sw policy-change, make it accessible
	g_object_set_data (G_OBJECT (outerbox), "e2-tab-scrolled-window", scrolled);
	GtkWidget *vbox = e2_widget_get_box (TRUE, FALSE, 0);
	e2_widget_sw_add_with_viewport (scrolled, vbox);
	return vbox;
}
/**
@brief  set font for @a widget to @a font_string

@param widget widget using the font
@param font_string string naming the font

@return
*/
void e2_widget_set_font (GtkWidget *widget, const gchar *font_string)
{
	PangoFontDescription *font_desc;
	font_desc = pango_font_description_from_string (font_string);
#ifdef USE_GTK3_0
	gtk_widget_override_font (widget, font_desc);
#else
	gtk_widget_modify_font (widget, font_desc);
#endif
	pango_font_description_free (font_desc);
}
/**
@brief get approximate size of a character used in @a widget

This retrieves approximate width and/or height of a character in the font
used in @a widget. So it should be called only after a font has been assigned
@a height may not be the whole distance between lines (e.g. spacing applies)

@param widget the widget to be evaluated
@param width pointer to int to store the character pixel-width, or NULL
@param height pointer to int to store the character pixel-height, or NULL

@return
*/
void e2_widget_get_font_pixels (GtkWidget *widget, gint *width, gint *height)
{
	PangoContext *context = gtk_widget_get_pango_context (widget);
#ifdef USE_GTK3_0
	GtkStyleContext *sc = gtk_widget_get_style_context (widget);
# ifdef USE_GTK3_8
	const PangoFontDescription *desc;
	gtk_style_context_get (sc, GTK_STATE_FLAG_NORMAL,
		GTK_STYLE_PROPERTY_FONT, &desc, NULL);
# else
	const PangoFontDescription *desc = gtk_style_context_get_font (sc, GTK_STATE_NORMAL);
# endif
	PangoFontMetrics *metrics = pango_context_get_metrics (context, desc,
		pango_context_get_language (context));
#else
# if defined (USE_GTK2_14)
	GtkStyle *style = gtk_widget_get_style (widget);
# endif
	PangoFontMetrics *metrics = pango_context_get_metrics (context,
# ifndef USE_GTK2_14
		widget->
# endif
		style->font_desc, pango_context_get_language (context));
#endif
	if (width != NULL)
		*width = PANGO_PIXELS (pango_font_metrics_get_approximate_char_width
			(metrics));
	if (height != NULL)
		*height = PANGO_PIXELS (pango_font_metrics_get_ascent (metrics) +
				pango_font_metrics_get_descent (metrics));
	pango_font_metrics_unref (metrics);
/*
#ifdef USE_GTK3_0
	GtkStyleContext *sc = gtk_widget_get_style_context (widget);
	PangoFontDescription *desc = gtk_style_context_get_font (sc, GTK_STATE_NORMAL);
#else
	PangoFontDescription *desc = gtk_widget_get_style (widget)->font_desc;
#endif
	*height = pango_font_description_get_size (desc) / PANGO_SCALE * DPI / 72; //(pixels/in/points/in);
	height = size of the font in points, scaled by PANGO_SCALE,
	(i.e. a size value of 10 * PANGO_SCALE is a 10 point font.
	For screen display, a logical DPI of 96 is common, what is it actually ?

	or, if
	pango_font_description_get_size_is_absolute(), returned size is pixels * PANGO_SCALE
	*height = pango_font_description_get_size (desc) / PANGO_SCALE

	PangoLanguage *language = pango_context_get_language (context);
	const gchar *defstr =  pango_language_get_sample_string (language);
	gint count = g_utf8_strlen (defstr, -1);

	gint _width, _height;
	PangoLayout *layout;
//	layout = gtk_widget_create_pango_layout (widget, "m");
	layout = gtk_widget_create_pango_layout (widget, defstr);
//	layout = gtk_widget_create_pango_layout (widget, _("abcdefghijklmnopqrstuvwxyz.ABCDEFGHIJKLMNOPQRSTUVWXYZ"));
//	layout = gtk_widget_create_pango_layout (widget, _("abcd efgh ABCD EFGH"));
	pango_layout_get_pixel_size (layout, &_width, &_height);
//	g_object_unref (layout);
//	if (width != NULL)
//		*width = _width;
//		*width = _width/53;
//		*width = _width/19;
//	if (height != NULL)
//		*height = _height;
	_width /= count;

	layout = gtk_widget_create_pango_layout (widget, ("a\na\n"));
	pango_layout_set_single_paragraph_mode (layout, FALSE);
	gint debug = PANGO_PIXELS (pango_layout_get_spacing (layout));
	g_object_unref (layout);
*/
}
/**
@brief callback upon tied check-button click
If not blocked by the controller widget, update the associated set value
@param button the widget that changed
@param boolset pointer to data for boolean set tied to button state

@return
*/
static void _e2_widget_check_changed_cb (GtkToggleButton *button, E2_OptionSet *boolset)
{
	GtkWidget *controller = g_object_get_data (G_OBJECT (button),
		"e2-controller-widget");
	if (!GPOINTER_TO_INT (g_object_get_data (G_OBJECT (controller),
		"e2-controller-blocked")))
	{
//		NEEDCLOSEBGL
		e2_option_bool_set_direct (boolset,
#ifdef USE_GTK2_14
			gtk_toggle_button_get_active (button)
#else
			button->active
#endif
		);
//		NEEDOPENBGL
	}
}
/**
@brief set @a button state if it's not blocked and not already at the desired state
This is a hook-function callack upon change of set data associated with @a button
@param state pointerised TRUE/FALSE, the value to apply to @a expander
@param button the widget to change

@return TRUE always
*/
static gboolean _e2_widget_set_check_state_hook (gpointer state, GtkWidget *button)
{
	GtkWidget *controller = g_object_get_data (G_OBJECT (button),
		"e2-controller-widget");
	if (!GPOINTER_TO_INT (g_object_get_data (G_OBJECT (controller),
		"e2-controller-blocked")))
	{
		gboolean value = GPOINTER_TO_INT (state);
		gboolean current = gtk_toggle_button_get_active
			(GTK_TOGGLE_BUTTON (button));
		if (value != current)
			gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), value);
	}
	return TRUE;
}
/**
@brief add to @a box a check button whose value is tied to value of @a set

@param box the widget to hold the button
@param boolset pointer to config data for the tied set
@param controller widget which may have "e2-controller-blocked" data to prevent set value being updated when button toggles, can be NULL

@return the created button widget
*/
GtkWidget *e2_widget_add_tied_check_button
	(GtkWidget *box, E2_OptionSet *boolset, GtkWidget *controller)
{
	boolset->widget = e2_button_add_toggle (box, TRUE, boolset->ival, boolset->desc,
		NULL, FALSE, 0, _e2_widget_check_changed_cb, boolset);
	g_object_set_data (G_OBJECT (boolset->widget), "e2-controller-widget", controller);
	e2_option_attach_value_changed_simple (boolset, boolset->widget,
		(HookFunc)_e2_widget_set_check_state_hook, boolset->widget);
	e2_widget_set_safetip (boolset->widget, boolset->tip);
	e2_widget_handle_depends (boolset->widget, boolset);

	return boolset->widget;
}

/**
@brief change sensitivity of @a menu_item after value-change of dependant set
@param state pointerised TRUE/FALSE, the value to use to adjust to @a widget
@param widget the widget to change
@return TRUE always
*/
static gboolean _e2_widget_dependent_changed_hook (gpointer state, GtkWidget *widget)
{
	gboolean value = GPOINTER_TO_INT (state);
	if (value &&
#ifdef USE_GTK2_18
			!gtk_widget_is_sensitive (widget)
#else
			!GTK_WIDGET_SENSITIVE (widget)
#endif
	)
		gtk_widget_set_sensitive (widget, TRUE);
	else if (!value &&
#ifdef USE_GTK2_18
		 	gtk_widget_is_sensitive (widget)
#else
			GTK_WIDGET_SENSITIVE (widget)
#endif
	)
		gtk_widget_set_sensitive (widget, FALSE);
	return TRUE;
}
/**
@brief change sensitivity of @a menu_item after value-change of set with inverse dependency
@param state pointerised TRUE/FALSE, the value to use to adjust to @a widget
@param widget the widget to change
@return TRUE always
*/
static gboolean _e2_widget_invdependent_changed_hook (gpointer state, GtkWidget *widget)
{
	gboolean value = GPOINTER_TO_INT (state);
	if (value &&
#ifdef USE_GTK2_18
		 	gtk_widget_is_sensitive (widget)
#else
			GTK_WIDGET_SENSITIVE (widget)
#endif
	)
		gtk_widget_set_sensitive (widget, FALSE);
	else if (!value &&
#ifdef USE_GTK2_18
		 	!gtk_widget_is_sensitive (widget)
#else
			!GTK_WIDGET_SENSITIVE (widget)
#endif
	)
		gtk_widget_set_sensitive (widget, TRUE);
	return TRUE;
}
/**
@brief adjust current and future widget sensitivity according to a dependent set's data

@param widget the widget to process
@param boolset pointer to data for set tied to @a widget

@return
*/
void e2_widget_handle_depends (GtkWidget *widget, E2_OptionSet *boolset)
{
	if (boolset->depends != NULL)
	{
		E2_OptionSet *dep;
		if (boolset->depends[0] == '!')
		{
			dep = e2_option_get (boolset->depends + 1);
			if ((dep != NULL) && (dep->type == E2_OPTION_TYPE_BOOL))
			{
				gtk_widget_set_sensitive (widget,
					!e2_option_bool_get_direct (dep));
				e2_option_attach_value_changed_simple (dep, widget,
					(HookFunc)_e2_widget_invdependent_changed_hook, widget);
			}
		}
		else
		{
			dep = e2_option_get (boolset->depends);
			if ((dep != NULL) && (dep->type == E2_OPTION_TYPE_BOOL))
			{
				gtk_widget_set_sensitive (widget,
					e2_option_bool_get_direct (dep));
				e2_option_attach_value_changed_simple (dep, widget,
					(HookFunc)_e2_widget_dependent_changed_hook, widget);
			}
		}
	}
}

#ifdef E2_ASSISTED
/**
@brief set relations on @a label for @a widget and vice-versa
This enables accessiblity tools to identify @a label as descriptive item for
@a @widget etc
@param label the label(etc) associated with @a widget
@param widget the widget to be tagged

@return
*/
void e2_widget_set_label_relations (GtkWidget *label, GtkWidget *widget)
{
	AtkObject *atklabelob = gtk_widget_get_accessible (GTK_WIDGET (label));
	AtkObject *atkob = gtk_widget_get_accessible (widget);
	if (G_LIKELY (atkob != NULL && atklabelob != NULL))
	{
		AtkRelationSet *relset = atk_object_ref_relation_set (atklabelob);
		AtkRelation *relation = atk_relation_new (&atkob, 1, ATK_RELATION_LABEL_FOR);
		atk_relation_set_add (relset, relation);
		g_object_unref (G_OBJECT (relation));

		relset = atk_object_ref_relation_set (atkob);
		relation = atk_relation_new (&atklabelob, 1, ATK_RELATION_LABELLED_BY);
		atk_relation_set_add (relset, relation);
		g_object_unref (G_OBJECT (relation));
	}
}
/**
@brief create accessibility object for @a widget
@param widget the widget to be tagged
@param label the label associated with @a widget

@return the atk object
*/
AtkObject *e2_widget_get_accessible (GtkWidget *widget, const gchar *name,
	const gchar *desc, AtkRole role)
{
	AtkObject *atkob = gtk_widget_get_accessible (widget);
	if (G_LIKELY (atkob != NULL))
	{
		// set custom atk properties
		if (name != NULL)
			atk_object_set_name (atkob, name);
		if (desc != NULL)
			atk_object_set_description (atkob, desc);
		if (role >= ATK_ROLE_LAST_DEFINED)	//no custom roles, for now at least
			role = ATK_ROLE_INVALID;
		atk_object_set_role (atkob, role);
	}
	return atkob;
}
#endif
