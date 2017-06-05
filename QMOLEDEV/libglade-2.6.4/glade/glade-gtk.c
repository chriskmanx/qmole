/* -*- Mode: C; c-basic-offset: 4 -*-
 * libglade - a library for building interfaces from XML files at runtime
 * Copyright (C) 1998-2002  James Henstridge <james@daa.com.au>
 *
 * glade-gtk.c: support for GTK+ widgets in libglade
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the 
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA  02111-1307, USA.
 */
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <string.h>
#include <stdlib.h>

/* for GtkText et all */
#ifndef GTK_ENABLE_BROKEN
#define GTK_ENABLE_BROKEN
#endif
#ifdef GTK_DISABLE_DEPRECATED
#undef GTK_DISABLE_DEPRECATED
#endif
#include <gtk/gtk.h>

#include <glade/glade.h>
#include <glade/glade-build.h>
#include <glade/glade-private.h>

#define INT(s)   (strtol ((s), NULL, 0))
#define BOOL(s)  (g_ascii_tolower (*(s)) == 't' || g_ascii_tolower (*(s)) == 'y' || INT (s))
#define FLOAT(s) (g_strtod ((s), NULL))

void _glade_init_gtk_widgets(void);

static void
custom_noop (GladeXML *xml, GtkWidget *widget,
	     const char *name, const char *value)
{
    ;
}

static void
set_visible(GladeXML *xml, GtkWidget *widget,
	    const gchar *prop_name, const gchar *prop_value)
{
    static GQuark visible_id = 0;

    if (visible_id == 0)
	visible_id = g_quark_from_static_string("Libglade::visible");

    if (BOOL(prop_value))
	g_object_set_qdata(G_OBJECT(widget), visible_id,GINT_TO_POINTER(TRUE));
}

static void
set_tooltip(GladeXML *xml, GtkWidget *widget,
	    const gchar *prop_name, const gchar *prop_value)
{
    if (GTK_IS_TOOL_ITEM (widget))
	gtk_tool_item_set_tooltip (GTK_TOOL_ITEM (widget), xml->priv->tooltips,
				   prop_value, NULL);
    else
	gtk_tooltips_set_tip(xml->priv->tooltips, widget, prop_value, NULL);
}

static void
set_has_default(GladeXML *xml, GtkWidget *widget,
		const gchar *prop_name, const gchar *prop_value)
{
    if (BOOL(prop_value))
	xml->priv->default_widget = widget;
}

static void
set_has_focus(GladeXML *xml, GtkWidget *widget,
	      const gchar *prop_name, const gchar *prop_value)
{
    if (BOOL(prop_value))
	xml->priv->focus_widget = widget;
}

static void
pixmap_set_build_insensitive (GladeXML *xml, GtkWidget *w,
			      const char *name, const char *value)
{
    gtk_pixmap_set_build_insensitive (GTK_PIXMAP (w), BOOL (value));
}

static void
pixmap_set_filename (GladeXML *xml, GtkWidget *w,
		     const char *name, const char *value)
{
    GdkPixbuf *pb;
    char *file;
    GdkPixmap *pixmap = NULL;
    GdkBitmap *bitmap = NULL;
    GdkColormap *cmap;
    
    file = glade_xml_relative_file (xml, value);
    pb = gdk_pixbuf_new_from_file (file, NULL);
    g_free (file);

    if (!pb)
	return;

    cmap = gtk_widget_get_colormap (w);
    gdk_pixbuf_render_pixmap_and_mask_for_colormap (pb, cmap, &pixmap, &bitmap, 127);
    gtk_pixmap_set (GTK_PIXMAP (w), pixmap, bitmap);

    if (pixmap) g_object_unref (pixmap);
    if (bitmap) g_object_unref (bitmap);

    g_object_unref (pb);
}

static void
progress_set_format (GladeXML *xml, GtkWidget *w,
		     const char *name, const char *value)
{
    gtk_progress_set_format_string (GTK_PROGRESS (w), value);
}

static void
list_item_set_label (GladeXML *xml, GtkWidget *w,
		     const char *name, const char *value)
{
    GtkWidget *child;

    child = gtk_bin_get_child (GTK_BIN (w));
    if (child)
	gtk_container_remove (GTK_CONTAINER (w), child);
    
    /* copied from gtk_list_item_new_with_label() */
    child = gtk_label_new (value);
    gtk_misc_set_alignment (GTK_MISC (child), 0.0, 0.5);
    gtk_misc_set_padding (GTK_MISC (child), 0, 1);

    gtk_container_add (GTK_CONTAINER (w), child);
    gtk_widget_show (child);
}

static void
text_view_set_text (GladeXML *xml, GtkWidget *w,
		    const char *name, const char *value)
{
    GtkTextBuffer *buffy;

    buffy = gtk_text_buffer_new (NULL);
    gtk_text_buffer_set_text (buffy, value, strlen (value));
    gtk_text_view_set_buffer (GTK_TEXT_VIEW (w), buffy);
    g_object_unref (G_OBJECT (buffy));
}

static void
calendar_set_display_options (GladeXML *xml, GtkWidget *w,
			      const char *name, const char *value)
{
    gtk_calendar_display_options (
	GTK_CALENDAR (w),
	glade_flags_from_string (GTK_TYPE_CALENDAR_DISPLAY_OPTIONS,
				 value));
}

static void
clist_set_column_widths (GladeXML *xml, GtkWidget *w,
			 const char *name, const char *value)
{
    char *pos = (char *)value;
    int cols = 0;
    while (pos && *pos) {
	int width = strtol (pos, &pos, 10);
	if (*pos == ',') pos++;
	gtk_clist_set_column_width (GTK_CLIST (w), cols++, width);
    }
}

static void
clist_set_selection_mode (GladeXML *xml, GtkWidget *w,
			  const char *name, const char *value)
{
    gtk_clist_set_selection_mode (
	GTK_CLIST (w),
	glade_enum_from_string (GTK_TYPE_SELECTION_MODE,
				value));
}

static void
clist_set_shadow_type (GladeXML *xml, GtkWidget *w,
		       const char *name, const char *value)
{
    gtk_clist_set_shadow_type (
	GTK_CLIST (w),
	glade_enum_from_string (GTK_TYPE_SHADOW_TYPE,
				value));
}

static void
clist_set_show_titles (GladeXML *xml, GtkWidget *w,
		       const char *name, const char *value)
{
    if (BOOL (value))
	gtk_clist_column_titles_show (GTK_CLIST (w));
    else
	gtk_clist_column_titles_hide (GTK_CLIST (w));
}

static void
tree_set_selection_mode (GladeXML *xml, GtkWidget *w,
			 const char *name, const char *value)
{
    gtk_tree_set_selection_mode (
	GTK_TREE (w),
	glade_enum_from_string (GTK_TYPE_SELECTION_MODE, value));
}

static void
tree_set_view_mode (GladeXML *xml, GtkWidget *w,
		    const char *name, const char *value)
{
    gtk_tree_set_view_mode (
	GTK_TREE (w),
	glade_enum_from_string (GTK_TYPE_TREE_VIEW_MODE, value));
}

static void
tree_set_view_line (GladeXML *xml, GtkWidget *w,
		    const char *name, const char *value)
{
    gtk_tree_set_view_lines (GTK_TREE (w), BOOL (value));
}

static void
list_set_selection_mode (GladeXML *xml, GtkWidget *w,
			 const char *name, const char *value)
{
    gtk_list_set_selection_mode (
	GTK_LIST (w),
	glade_enum_from_string (GTK_TYPE_SELECTION_MODE, value));
}

static void
check_menu_item_set_always_show_toggle (GladeXML *xml, GtkWidget *w,
					const char *name, const char *value)
{
    gtk_check_menu_item_set_show_toggle (GTK_CHECK_MENU_ITEM (w), BOOL (value));
}

static void
text_set_text (GladeXML *xml, GtkWidget *w,
	       const char *name, const char *value)
{
    int pos = 0;

    gtk_editable_insert_text (GTK_EDITABLE (w), value, -1, &pos);
}

static void
radio_menu_item_set_group (GladeXML *xml, GtkWidget *w,
			   const char *name, const char *value)
{
    GtkWidget *group;

    group = glade_xml_get_widget (xml, value);
    if (!group) {
	g_warning ("Radio button group %s could not be found", value);
	return;
    }

    if (group == w) {
	g_message ("Group is self, skipping.");
	return;
    }

    gtk_radio_menu_item_set_group (
	GTK_RADIO_MENU_ITEM (w),
	gtk_radio_menu_item_get_group (
	    GTK_RADIO_MENU_ITEM (group)));
}

static void
toolbar_set_tooltips (GladeXML *xml, GtkWidget *w,
		      const char *name, const char *value)
{
    gtk_toolbar_set_tooltips (GTK_TOOLBAR (w), BOOL (value));
}

static void
statusbar_set_has_resize_grip (GladeXML *xml, GtkWidget *w,
			       const char *name, const char *value)
{
    gtk_statusbar_set_has_resize_grip (GTK_STATUSBAR (w), BOOL (value));
}

static void
ruler_set_metric (GladeXML *xml, GtkWidget *w,
		  const char *name, const char *value)
{
    gtk_ruler_set_metric (
	GTK_RULER (w),
	glade_enum_from_string (GTK_TYPE_METRIC_TYPE, value));
}

static void
menu_item_set_label (GladeXML *xml, GtkWidget *w,
		     const char *name, const char *value)
{
    GtkWidget *child = GTK_BIN (w)->child;

    if (!child) {
	child = gtk_accel_label_new("");
	gtk_misc_set_alignment(GTK_MISC(child), 0.0, 0.5);
	gtk_container_add(GTK_CONTAINER(w), child);
	gtk_accel_label_set_accel_widget(GTK_ACCEL_LABEL(child), w);
	gtk_widget_show(child);
    }

    if (GTK_IS_LABEL (child))
	gtk_label_set_text (GTK_LABEL (child), value);
}

static void
menu_item_set_use_underline (GladeXML *xml, GtkWidget *w,
			     const char *name, const char *value)
{
    GtkWidget *child = GTK_BIN (w)->child;

    if (!child) {
	child = gtk_accel_label_new("");
	gtk_misc_set_alignment(GTK_MISC(child), 0.0, 0.5);
	gtk_container_add(GTK_CONTAINER(w), child);
	gtk_accel_label_set_accel_widget(GTK_ACCEL_LABEL(child), w);
	gtk_widget_show(child);
    }

    if (GTK_IS_LABEL (child))
	gtk_label_set_use_underline (GTK_LABEL (child), BOOL (value));
}

static void
menu_item_set_use_stock (GladeXML *xml, GtkWidget *w,
			 const gchar *name, const gchar *value)
{
    GtkWidget *child = GTK_BIN (w)->child;

    if (!child) {
	child = gtk_accel_label_new("");
	gtk_misc_set_alignment(GTK_MISC(child), 0.0, 0.5);
	gtk_container_add(GTK_CONTAINER(w), child);
	gtk_accel_label_set_accel_widget(GTK_ACCEL_LABEL(child), w);
	gtk_widget_show(child);
    }

    if (GTK_IS_LABEL (child) && BOOL(value)) {
	const gchar *stock_id = gtk_label_get_label(GTK_LABEL(child));
	GtkStockItem stock_item;

	if (gtk_stock_lookup(stock_id, &stock_item)) {
	    /* put in the stock image next to the text.  Done before
             * messing with the label child, so that stock_id doesn't
             * become invalid. */
	    if (GTK_IS_IMAGE_MENU_ITEM(w)) {
		GtkWidget *image =
		    gtk_image_new_from_stock(stock_id, GTK_ICON_SIZE_MENU);

		gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(w), image);
		gtk_widget_show(image);
	    }

	    gtk_label_set_text(GTK_LABEL(child), stock_item.label);
	    gtk_label_set_use_underline(GTK_LABEL(child), TRUE);

	    if (stock_item.keyval)
		gtk_widget_add_accelerator (w,
					    "activate",
					    glade_xml_ensure_accel(xml),
					    stock_item.keyval,
					    stock_item.modifier,
					    GTK_ACCEL_VISIBLE);
	} else {
	    g_warning("could not look up stock id '%s'", stock_id);
	}
    }
}

static void
window_set_wmclass_name (GladeXML *xml, GtkWidget *w,
			 const gchar *name, const gchar *value)
{
    GtkWindow *window = GTK_WINDOW(w);
    gchar *wmclass = g_strdup(window->wmclass_class);

    gtk_window_set_wmclass(window, value, wmclass);
    g_free(wmclass);
}

static void
window_set_wmclass_class (GladeXML *xml, GtkWidget *w,
			  const gchar *name, const gchar *value)
{
    GtkWindow *window = GTK_WINDOW(w);
    gchar *wmname = g_strdup(window->wmclass_name);

    gtk_window_set_wmclass(window, wmname, value);
    g_free(wmname);
}

static void
entry_set_invisible_char (GladeXML *xml, GtkWidget *w,
			  const gchar *name, const gchar *value)
{
    gunichar c;

    c = g_utf8_get_char_validated (value, strlen (value));
    if (c > 0)
        gtk_entry_set_invisible_char (GTK_ENTRY (w), c);
}

static void
button_set_response_id (GladeXML *xml, GtkWidget *w,
			const gchar *name, const gchar *value)
{
    g_object_set_data (G_OBJECT (w), "response_id",
		       GINT_TO_POINTER (INT (value)));
}

static void
toggle_tool_button_set_active (GladeXML *xml, GtkWidget *w,
			       const char *name, const char *value)
{
    gtk_toggle_tool_button_set_active (GTK_TOGGLE_TOOL_BUTTON (w),
				       BOOL (value));
}

static void
tool_button_set_icon (GladeXML *xml, GtkWidget *w,
		      const char *name, const char *value)
{
    GdkPixbuf *pb;
    GtkWidget *image;
    char *file;

    file = glade_xml_relative_file (xml, value);
    pb = gdk_pixbuf_new_from_file (file, NULL);
    g_free (file);

    if (!pb) {
	g_warning ("Couldn't find image file: %s", value);
	return;
    }

    image = gtk_image_new_from_pixbuf (pb);
    g_object_unref (pb);

    gtk_widget_show (image);
    gtk_tool_button_set_icon_widget (GTK_TOOL_BUTTON (w), image);
}

static void
combo_box_set_items (GladeXML *xml, GtkWidget *w,
		     const char *name, const char *value)
{
    GtkListStore *store;
    GtkCellRenderer *cell;
    gchar *items, *items_end, *item_start, *item_end;
    GtkTreeIter iter;

    /* If the "items" property is set, we create a simple model with just
       one column of text. */
    store = gtk_list_store_new (1, G_TYPE_STRING);
    gtk_combo_box_set_model (GTK_COMBO_BOX (w), GTK_TREE_MODEL (store));

    /* GtkComboBoxEntry creates the cell renderer itself, but we have to set
       the column containing the text. */
    if (GTK_IS_COMBO_BOX_ENTRY (w)) {
	gtk_combo_box_entry_set_text_column (GTK_COMBO_BOX_ENTRY (w), 0);
    } else {
	cell = gtk_cell_renderer_text_new ();
	gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (w), cell, TRUE);
	gtk_cell_layout_set_attributes (GTK_CELL_LAYOUT (w), cell,
					"text", 0,
					NULL);
    }

    /* Now add the items one at a time. */
    items = g_strdup (value);
    items_end = &items[strlen (items)];
    item_start = items;
    while (item_start < items_end) {
	item_end = strchr (item_start, '\n');
	if (item_end == NULL)
	    item_end = items_end;
	*item_end = '\0';

	gtk_list_store_append (store, &iter);
	gtk_list_store_set (store, &iter, 0, item_start, -1);

	item_start = item_end + 1;
    }
    g_free (items);
}

static void
menuitem_build_children(GladeXML *self, GtkWidget *w,
			GladeWidgetInfo *info)
{
    gint i;

    g_object_ref(G_OBJECT(w));
    for (i = 0; i < info->n_children; i++) {
	GtkWidget       *child;
	GladeWidgetInfo *childinfo = info->children[i].child;

	if (info->children[i].internal_child) {
	    glade_xml_handle_internal_child(self, w, &info->children[i]);
	    continue;
	}

	child = glade_xml_build_widget(self, childinfo);

	if (GTK_IS_MENU(child))
	    gtk_menu_item_set_submenu(GTK_MENU_ITEM(w), child);
	else
	    gtk_container_add(GTK_CONTAINER(w), child);
    }
    g_object_unref(G_OBJECT(w));
}

static void
gtk_dialog_build_children(GladeXML *self, GtkWidget *w,
			  GladeWidgetInfo *info)

{
    GtkDialog *dialog = GTK_DIALOG (w);
    GList *children, *list;

    glade_standard_build_children (self, w, info);

    if (dialog->action_area == NULL)
	return;

    /* repack children of action_area */
    children = gtk_container_get_children(GTK_CONTAINER(dialog->action_area));
    for (list = children; list; list = list->next) {
	GtkWidget *child = GTK_WIDGET(list->data);

	g_object_ref(child);
	gtk_container_remove (GTK_CONTAINER (dialog->action_area), child);
    }
    for (list = children; list; list = list->next) {
	GtkWidget *child = GTK_WIDGET(list->data);
	gint response_id;

	response_id = GPOINTER_TO_INT(g_object_steal_data(G_OBJECT(child),
							  "response_id"));
	gtk_dialog_add_action_widget(dialog, child, response_id);
	g_object_unref(child);

    }
    g_list_free (children);
}


static void
frame_build_children(GladeXML *self, GtkWidget *parent,
		     GladeWidgetInfo *info)
{
    gint i, j;
    enum {
	FRAME_ITEM,
	LABEL_ITEM
    } type;

    g_object_ref(G_OBJECT(parent));
    for (i = 0; i < info->n_children; i++) {
	GladeWidgetInfo *childinfo = info->children[i].child;
	GtkWidget *child = glade_xml_build_widget(self, childinfo);

	type = FRAME_ITEM;
	/* there should really only be 2 children */
	for (j = 0; j < info->children[i].n_properties; j++) {
	    if (!strcmp (info->children[i].properties[j].name, "type")) {
		const char *value = info->children[i].properties[j].value;

		if (!strcmp (value, "label_item"))
		    type = LABEL_ITEM;
		break;
	    }
	}

	if (type == LABEL_ITEM) {
	    gtk_frame_set_label_widget (GTK_FRAME (parent), child);
	} else {
	    gtk_container_add (GTK_CONTAINER (parent), child);
	}
    }
    g_object_unref(G_OBJECT(parent));
}

static void
expander_build_children (GladeXML        *self,
			 GtkWidget       *parent,
			 GladeWidgetInfo *info)
{
    int i;

    g_object_ref (parent);
    for (i = 0; i < info->n_children; i++) {
	GladeWidgetInfo *childinfo = info->children [i].child;
	GtkWidget       *child;
	gboolean         label_item = FALSE;
	int              j;
	
	child = glade_xml_build_widget (self, childinfo);

	/* there should really only be 2 children */
	for (j = 0; j < info->children [i].n_properties; j++) {
	    if (!strcmp (info->children [i].properties[j].name, "type")) {
		const char *value = info->children [i].properties [j].value;

		if (!strcmp (value, "label_item"))
		    label_item = TRUE;
		break;
	    }
	}

	if (label_item)
	    gtk_expander_set_label_widget (GTK_EXPANDER (parent), child);
	else
	    gtk_container_add (GTK_CONTAINER (parent), child);
    }
    g_object_unref (parent);
}

static void
notebook_build_children(GladeXML *self, GtkWidget *parent,
			GladeWidgetInfo *info)
{
    gint i, j, tab = 0;
    enum {
	PANE_ITEM,
	TAB_ITEM,
	MENU_ITEM
    } type;

    g_object_ref(G_OBJECT(parent));
    for (i = 0; i < info->n_children; i++) {
	GladeWidgetInfo *childinfo = info->children[i].child;
	GtkWidget *child = glade_xml_build_widget(self, childinfo);

	type = PANE_ITEM;
	for (j = 0; j < info->children[i].n_properties; j++) {
	    if (!strcmp (info->children[i].properties[j].name, "type")) {
		const char *value = info->children[i].properties[j].value;

		if (!strcmp (value, "tab"))
		    type = TAB_ITEM;
		break;
	    }
	}

	if (type == TAB_ITEM) { /* The GtkNotebook API blows */
	    GtkWidget *body;

	    body = gtk_notebook_get_nth_page (GTK_NOTEBOOK (parent), (tab - 1));
	    gtk_notebook_set_tab_label (GTK_NOTEBOOK (parent), body, child);
	} else {
	    gtk_notebook_append_page (GTK_NOTEBOOK (parent), child, NULL);
	    tab++;
	}
    }
    g_object_unref(G_OBJECT(parent));
}

static GtkWidget *
build_preview (GladeXML *xml, GType widget_type,
	       GladeWidgetInfo *info)
{
    GtkWidget *preview;

    GtkPreviewType type = GTK_PREVIEW_COLOR;
    gboolean expand = TRUE;

    int i;

    for (i = 0; i < info->n_properties; i++) {
	const char *name  = info->properties[i].name;
	const char *value = info->properties[i].value;

	if (!strcmp (name, "expand"))
	    expand = BOOL (value);
	else if (!strcmp (name, "type"))
	    type = glade_enum_from_string (GTK_TYPE_PREVIEW_TYPE, value);
    }

    preview = gtk_preview_new (type);
    gtk_preview_set_expand (GTK_PREVIEW (preview), expand);

    return preview;
}

static void
option_menu_build_children (GladeXML *xml, GtkWidget *parent,
			    GladeWidgetInfo *info)
{
    int i, history = 0;

    for (i = 0; i < info->n_children; i++) {
	GtkWidget *child;
	GladeWidgetInfo *childinfo = info->children[i].child;

	if (strcmp(childinfo->classname, "GtkMenu") != 0) {
	    g_warning("the child of the option menu '%s' was not a GtkMenu",
		      info->name);
	    continue;
	}
	child = glade_xml_build_widget(xml, childinfo);

	gtk_option_menu_set_menu(GTK_OPTION_MENU(parent), child);
    }

    for (i = 0; i < info->n_properties; i++) {
	const char *name  = info->properties[i].name;
	const char *value = info->properties[i].value;

	if (strcmp (name, "history"))
	    continue;

	history = INT (value);
	break;
    }

    /* we have to set the history *after* building the child menu */
    gtk_option_menu_set_history (GTK_OPTION_MENU (parent), history);
}

static void
clist_build_children(GladeXML *self, GtkWidget *parent,
		     GladeWidgetInfo *info)
{
    int i;

    g_object_ref (G_OBJECT (parent));
    for (i = 0; i < info->n_children; i++) {
	GladeWidgetInfo *childinfo;
	GtkWidget *child = NULL;

	childinfo = info->children[i].child;

	/* treat GtkLabels specially */
	if (!strcmp (childinfo->classname, "GtkLabel")) {
	    int j;
	    const char *label = NULL;

	    for (j = 0; j < childinfo->n_properties; j++) {
		if (!strcmp (childinfo->properties[j].name, "label")) {
		    label = childinfo->properties[j].value;
		    break;
		} else {
		    g_warning ("Unknown CList child property: %s", childinfo->properties[j].name);
		}
	    }

	    if (label) {
		/* FIXME: translate ? */
		gtk_clist_set_column_title (GTK_CLIST(parent), i, label);
		child = gtk_clist_get_column_widget (GTK_CLIST (parent), i);
		child = GTK_BIN(child)->child;
		glade_xml_set_common_params(self, child, childinfo);
	    }
	}

	if (!child) {
	    child = glade_xml_build_widget (self, childinfo);
	    gtk_clist_set_column_widget (GTK_CLIST (parent), i, child);
	}
    }

    g_object_unref (G_OBJECT (parent));
}

static void
toolbar_build_children (GladeXML *xml, GtkWidget *parent,
			GladeWidgetInfo *info)
{
    int i;

    g_object_ref (G_OBJECT (parent));

    for (i = 0; i < info->n_children; i++) {
	GladeChildInfo *childinfo;
	GtkWidget *child = NULL;

	childinfo = &info->children[i];

	if (!strcmp (childinfo->child->classname, "toggle") ||
	    !strcmp (childinfo->child->classname, "radio") ||
	    !strcmp (childinfo->child->classname, "button")) {
	    const char *label = NULL, *stock = NULL, *group_name = NULL;
	    const char *tooltip = NULL;
	    char *icon = NULL;
	    gboolean use_stock = FALSE, active = FALSE, new_group = FALSE;
	    gboolean use_underline = FALSE;
	    GtkWidget *iconw = NULL;
	    int j;

	    for (j = 0; j < childinfo->child->n_properties; j++) {
		const char *name  = childinfo->child->properties[j].name;
		const char *value = childinfo->child->properties[j].value;

		if (!strcmp (name, "label")) {
		    label = value;
		} else if (!strcmp (name, "use_stock")) {
		    use_stock = TRUE;
		} else if (!strcmp (name, "icon")) {
		    g_free (icon);
		    stock = NULL;
		    icon = glade_xml_relative_file (xml, value);
		} else if (!strcmp (name, "stock_pixmap")) {
		    g_free (icon);
		    icon = NULL;
		    stock = value;
		} else if (!strcmp (name, "active")) {
		    active = BOOL (value);
		} else  if (!strcmp (name, "group")) {
		    group_name = value;
		} else if (!strcmp (name, "new_group")) {
		    new_group = BOOL (value);
		} else if (!strcmp (name, "visible")) {
		    /* ignore for now */
		} else if (!strcmp (name, "tooltip")) {
		    tooltip = value;
		} else if (!strcmp (name, "use_underline")) {
		    use_underline = BOOL (value);
		} else if (!strcmp (name, "inconsistent")) {
		    /* ignore for now */
		} else {
		    g_warning ("Unknown GtkToolbar child property: %s", name);
		}
	    }

	    /* For stock items, we create the stock icon and get the label
	       here, partly because GTK+ doesn't have direct support for stock
	       toggle & radio items. */
	    if (use_stock) {
		stock = label;
		label = NULL;
	    }

	    if (stock) {
		iconw = gtk_image_new_from_stock (
		    stock, GTK_TOOLBAR (parent)->icon_size);
		if (!iconw)
		    g_warning ("Could not create stock item: %s", stock);
	    } else if (icon) {
		iconw = gtk_image_new_from_file (icon);
		g_free (icon);
	    }

	    if (iconw)
		gtk_widget_show (iconw);

	    if (new_group)
		gtk_toolbar_append_space (GTK_TOOLBAR (parent));

	    /* FIXME: these should be translated */
	    if (!strcmp (childinfo->child->classname, "toggle")) {
		child = gtk_toolbar_append_element (
		    GTK_TOOLBAR (parent),
		    GTK_TOOLBAR_CHILD_TOGGLEBUTTON, NULL,
		    label, tooltip, NULL, iconw, NULL, NULL);
		gtk_toggle_button_set_active
		    (GTK_TOGGLE_BUTTON (child), active);
	    } else if (!strcmp (childinfo->child->classname, "radio")) {
		child = gtk_toolbar_append_element (
		    GTK_TOOLBAR (parent),
		    GTK_TOOLBAR_CHILD_RADIOBUTTON, NULL,
		    label, tooltip, NULL, iconw, NULL, NULL);

		if (group_name) {
		    g_object_set (G_OBJECT (child),
				  "group", glade_xml_get_widget (xml, group_name),
				  NULL);
		}
		gtk_toggle_button_set_active
		    (GTK_TOGGLE_BUTTON (child), active);
	    } else
		child = gtk_toolbar_append_item (
		    GTK_TOOLBAR (parent),
		    label, tooltip, NULL, iconw, NULL, NULL);
	    
	    /* GTK+ doesn't support use_underline directly, so we have to hack
	       it. */
	    if (use_underline) {
		GList *elem = g_list_last (GTK_TOOLBAR (parent)->children);
		GtkToolbarChild *toolbar_child = elem->data;
		gtk_label_set_use_underline (GTK_LABEL (toolbar_child->label),
					     TRUE);
	    }

	    glade_xml_set_common_params (xml, child, childinfo->child);
	} else {
	    child = glade_xml_build_widget (xml, childinfo->child);

	    if (GTK_IS_TOOL_ITEM (child))
		gtk_toolbar_insert (GTK_TOOLBAR (parent), GTK_TOOL_ITEM (child), -1);
	    else
		gtk_toolbar_append_widget (GTK_TOOLBAR (parent), child, NULL, NULL);
	}
    }
}

static void
paned_build_children (GladeXML *xml, GtkWidget *w, GladeWidgetInfo *info)
{
    int i;
    GtkWidget *child;
    gboolean resize, shrink;
    GladeChildInfo *cinfo;

    if (info->n_children == 0)
	return;
    
    cinfo = &info->children[0];
    child = glade_xml_build_widget (xml, cinfo->child);
    
    resize = FALSE; shrink = TRUE;
    for (i = 0; i < cinfo->n_properties; i++) {
	const char *name  = cinfo->properties[i].name;
	const char *value = cinfo->properties[i].value;

	if (!strcmp (name, "resize"))
	    resize = BOOL (value);
	else if (!strcmp (name, "shrink"))
	    shrink = BOOL (value);
	else
	    g_warning ("Unknown GtkPaned child property: %s", name);
    }

    gtk_paned_pack1 (GTK_PANED(w), child, resize, shrink);
    
    if (info->n_children == 1)
	return;

    cinfo = &info->children[1];
    child = glade_xml_build_widget (xml, cinfo->child);
    resize = TRUE; shrink = TRUE;

    for (i = 0; i < cinfo->n_properties; i++) {
	const char *name  = cinfo->properties[i].name;
	const char *value = cinfo->properties[i].value;

	if (!strcmp (name, "resize"))
	    resize = BOOL (value);
	else if (!strcmp (name, "shrink"))
	    shrink = BOOL (value);
	else
	    g_warning ("Unknown GtkPaned child property: %s", name);
    }
	
    gtk_paned_pack2 (GTK_PANED(w), child, resize, shrink);
}

static void
layout_build_children (GladeXML *xml, GtkWidget *w, GladeWidgetInfo *info)
{
    gint i;

    for (i = 0; i < info->n_children; i++) {
	GladeWidgetInfo *childinfo = info->children[i].child;
	GtkWidget *child = NULL;
	gint j, x = 0, y = 0;

	if (info->children[i].internal_child) {
	    glade_xml_handle_internal_child(xml, w, &info->children[i]);
	    continue;
	}

	child = glade_xml_build_widget(xml, childinfo);
	for (j = 0; j < info->children[i].n_properties; j++) {
	    const gchar *name = info->children[i].properties[j].name;
	    const gchar *value = info->children[i].properties[j].value;

	    if (name[0] == 'x' && name[1] == '\0')
		x = INT(value);
	    else if (name[0] == 'y' && name[1] == '\0')
		y = INT(value);
	    else
		g_warning("unknown child packing property %s for GtkLayout",
			  name);
	}

	gtk_layout_put(GTK_LAYOUT(w), child, x, y);
    }
}

static GtkWidget *
dialog_find_internal_child(GladeXML *xml, GtkWidget *parent,
			   const gchar *childname)
{
    if (!strcmp(childname, "vbox"))
	return GTK_DIALOG(parent)->vbox;
    if (!strcmp(childname, "action_area"))
	return GTK_DIALOG(parent)->action_area;

    return NULL;
}

static GtkWidget *
image_menu_find_internal_child(GladeXML *xml, GtkWidget *parent,
			       const gchar *childname)
{
    if (!strcmp(childname, "image")) {
	GtkWidget *pl;

	pl = gtk_image_menu_item_get_image (GTK_IMAGE_MENU_ITEM (parent));
	if (!pl) {
	    pl = gtk_image_new ();

	    gtk_image_menu_item_set_image (
		GTK_IMAGE_MENU_ITEM (parent), pl);
	}

	return pl;
    }
    return NULL;
}

static GtkWidget *
scrolled_window_find_internal_child(GladeXML *xml, GtkWidget *parent,
				    const gchar *childname)
{
    if (!strcmp(childname, "vscrollbar"))
	return GTK_SCROLLED_WINDOW (parent)->vscrollbar;
    if (!strcmp(childname, "hscrollbar"))
	return GTK_SCROLLED_WINDOW (parent)->hscrollbar;
    return NULL;
}

static GtkWidget *
filesel_find_internal_child(GladeXML *xml, GtkWidget *parent,
			    const gchar *childname)
{
    if (!strcmp(childname, "vbox"))
	return GTK_DIALOG(parent)->vbox;
    if (!strcmp(childname, "action_area"))
	return GTK_DIALOG(parent)->action_area;
    if (!strcmp(childname, "ok_button"))
	return GTK_FILE_SELECTION(parent)->ok_button;
    if (!strcmp(childname, "cancel_button"))
	return GTK_FILE_SELECTION(parent)->cancel_button;
    if (!strcmp(childname, "help_button"))
	return GTK_FILE_SELECTION(parent)->help_button;
    return NULL;
}

static GtkWidget *
colorseldlg_find_internal_child(GladeXML *xml, GtkWidget *parent,
				const gchar *childname)
{
    if (!strcmp(childname, "vbox"))
	return GTK_DIALOG(parent)->vbox;
    if (!strcmp(childname, "action_area"))
	return GTK_DIALOG(parent)->action_area;
    if (!strcmp(childname, "ok_button"))
	return GTK_COLOR_SELECTION_DIALOG(parent)->ok_button;
    if (!strcmp(childname, "cancel_button"))
	return GTK_COLOR_SELECTION_DIALOG(parent)->cancel_button;
    if (!strcmp(childname, "help_button"))
	return GTK_COLOR_SELECTION_DIALOG(parent)->help_button;
    if (!strcmp(childname, "color_selection"))
	return GTK_COLOR_SELECTION_DIALOG(parent)->colorsel;
    return NULL;
}

static GtkWidget *
fontseldlg_find_internal_child(GladeXML *xml, GtkWidget *parent,
			       const gchar *childname)
{
    if (!strcmp(childname, "vbox"))
	return GTK_DIALOG(parent)->vbox;
    if (!strcmp(childname, "action_area"))
	return GTK_DIALOG(parent)->action_area;
    if (!strcmp(childname, "ok_button"))
	return GTK_FONT_SELECTION_DIALOG(parent)->ok_button;
    if (!strcmp(childname, "cancel_button"))
	return GTK_FONT_SELECTION_DIALOG(parent)->cancel_button;
    if (!strcmp(childname, "apply_button"))
	return GTK_FONT_SELECTION_DIALOG(parent)->apply_button;
    if (!strcmp(childname, "font_selection"))
	return GTK_FONT_SELECTION_DIALOG(parent)->fontsel;
    return NULL;
}

static GtkWidget *
combo_find_internal_child(GladeXML *xml, GtkWidget *parent,
			  const gchar *childname)
{
    if (!strcmp(childname, "entry"))
	return GTK_COMBO(parent)->entry;
    if (!strcmp(childname, "button"))
	return GTK_COMBO(parent)->button;
    if (!strcmp(childname, "popup"))
	return GTK_COMBO(parent)->popup;
    if (!strcmp(childname, "popwin"))
	return GTK_COMBO(parent)->popwin;
    if (!strcmp(childname, "list"))
	return GTK_COMBO(parent)->list;
    return NULL;
}

static GtkWidget *
combo_box_entry_find_internal_child(GladeXML *xml, GtkWidget *parent,
				    const gchar *childname)
{
    if (!strcmp(childname, "entry"))
	return gtk_bin_get_child(GTK_BIN(parent));
    return NULL;
}

void
_glade_init_gtk_widgets(void)
{
    glade_register_custom_prop (GTK_TYPE_WIDGET, "visible", set_visible);
    glade_register_custom_prop (GTK_TYPE_WIDGET, "tooltip", set_tooltip);
    glade_register_custom_prop (GTK_TYPE_WIDGET, "has_default", set_has_default);
    glade_register_custom_prop (GTK_TYPE_WIDGET, "has_focus", set_has_focus);

    glade_register_custom_prop (GTK_TYPE_PIXMAP, "build_insensitive", pixmap_set_build_insensitive);
    glade_register_custom_prop (GTK_TYPE_PIXMAP, "filename", pixmap_set_filename);
    glade_register_custom_prop (GTK_TYPE_PROGRESS, "format", progress_set_format);
    glade_register_custom_prop (GTK_TYPE_OPTION_MENU, "history", custom_noop);
    glade_register_custom_prop (GTK_TYPE_TEXT_VIEW, "text", text_view_set_text);
    glade_register_custom_prop (GTK_TYPE_CALENDAR, "display_options", calendar_set_display_options);
    glade_register_custom_prop (GTK_TYPE_CLIST, "column_widths", clist_set_column_widths);
    glade_register_custom_prop (GTK_TYPE_CLIST, "selection_mode", clist_set_selection_mode);
    glade_register_custom_prop (GTK_TYPE_CLIST, "shadow_type", clist_set_shadow_type);
    glade_register_custom_prop (GTK_TYPE_CLIST, "show_titles", clist_set_show_titles);
    glade_register_custom_prop (GTK_TYPE_TREE, "selection_mode", tree_set_selection_mode);
    glade_register_custom_prop (GTK_TYPE_TREE, "view_mode", tree_set_view_mode);
    glade_register_custom_prop (GTK_TYPE_TREE, "view_line", tree_set_view_line);
    glade_register_custom_prop (GTK_TYPE_LIST, "selection_mode", list_set_selection_mode);
    glade_register_custom_prop (GTK_TYPE_CHECK_MENU_ITEM, "always_show_toggle",
				check_menu_item_set_always_show_toggle);
    glade_register_custom_prop (GTK_TYPE_TEXT, "text", text_set_text);
    glade_register_custom_prop (GTK_TYPE_RADIO_MENU_ITEM, "group",
				radio_menu_item_set_group);
    glade_register_custom_prop (GTK_TYPE_TOOLBAR, "tooltips", toolbar_set_tooltips);
    glade_register_custom_prop (GTK_TYPE_STATUSBAR, "has_resize_grip", statusbar_set_has_resize_grip);
    glade_register_custom_prop (GTK_TYPE_RULER, "metric", ruler_set_metric);
    glade_register_custom_prop (GTK_TYPE_MENU_ITEM, "label", menu_item_set_label);
    glade_register_custom_prop (GTK_TYPE_MENU_ITEM, "use_underline", menu_item_set_use_underline);
    glade_register_custom_prop (GTK_TYPE_MENU_ITEM, "use_stock", menu_item_set_use_stock);
    glade_register_custom_prop (GTK_TYPE_WINDOW, "wmclass_name", window_set_wmclass_name);
    glade_register_custom_prop (GTK_TYPE_WINDOW, "wmclass_class", window_set_wmclass_class);
    glade_register_custom_prop (GTK_TYPE_LIST_ITEM, "label", list_item_set_label);
    glade_register_custom_prop (GTK_TYPE_BUTTON, "response_id", button_set_response_id);
    glade_register_custom_prop (GTK_TYPE_ENTRY, "invisible_char", entry_set_invisible_char);
    glade_register_custom_prop (GTK_TYPE_TOGGLE_TOOL_BUTTON, "active", toggle_tool_button_set_active);
    glade_register_custom_prop (GTK_TYPE_TOOL_BUTTON, "icon", tool_button_set_icon);
    glade_register_custom_prop (GTK_TYPE_COMBO_BOX, "items", combo_box_set_items);

    glade_register_widget (GTK_TYPE_ABOUT_DIALOG, NULL,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_ACCEL_LABEL, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_ALIGNMENT, glade_standard_build_widget,
			   glade_standard_build_children, NULL);
    glade_register_widget (GTK_TYPE_ARROW, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_ASPECT_FRAME, glade_standard_build_widget,
			   frame_build_children, NULL);
    glade_register_widget (GTK_TYPE_BUTTON, glade_standard_build_widget,
			   glade_standard_build_children, NULL);
    glade_register_widget (GTK_TYPE_CALENDAR, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_CHECK_BUTTON, glade_standard_build_widget,
			   glade_standard_build_children, NULL);
    glade_register_widget (GTK_TYPE_CHECK_MENU_ITEM, glade_standard_build_widget,
			   menuitem_build_children, NULL);
    glade_register_widget (GTK_TYPE_CLIST, glade_standard_build_widget,
			   clist_build_children, NULL);
    glade_register_widget (GTK_TYPE_COLOR_BUTTON, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_COLOR_SELECTION, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_COLOR_SELECTION_DIALOG, NULL,
			   glade_standard_build_children, colorseldlg_find_internal_child);
    glade_register_widget (GTK_TYPE_COMBO, glade_standard_build_widget,
			   glade_standard_build_children, combo_find_internal_child);
    glade_register_widget (GTK_TYPE_COMBO_BOX, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_COMBO_BOX_ENTRY, glade_standard_build_widget,
			   glade_standard_build_children, combo_box_entry_find_internal_child);
    glade_register_widget (GTK_TYPE_CTREE, glade_standard_build_widget,
			   clist_build_children, NULL);
    glade_register_widget (GTK_TYPE_CURVE, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_DIALOG, NULL,
			   gtk_dialog_build_children, dialog_find_internal_child);
    glade_register_widget (GTK_TYPE_DRAWING_AREA, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_ENTRY, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_EVENT_BOX, glade_standard_build_widget,
			   glade_standard_build_children, NULL);
    glade_register_widget (GTK_TYPE_EXPANDER, glade_standard_build_widget,
			   expander_build_children, NULL);
    glade_register_widget (GTK_TYPE_FILE_CHOOSER, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_FILE_CHOOSER_DIALOG, glade_standard_build_widget,
			   gtk_dialog_build_children, dialog_find_internal_child);
    glade_register_widget (GTK_TYPE_FILE_CHOOSER_BUTTON, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_FILE_SELECTION, NULL,
			   glade_standard_build_children, filesel_find_internal_child);
    glade_register_widget (GTK_TYPE_FIXED, glade_standard_build_widget,
			   glade_standard_build_children, NULL);
    glade_register_widget (GTK_TYPE_FONT_BUTTON, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_FONT_SELECTION, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_FONT_SELECTION_DIALOG, NULL,
			   glade_standard_build_children, fontseldlg_find_internal_child);
    glade_register_widget (GTK_TYPE_FRAME, glade_standard_build_widget,
			   frame_build_children, NULL);
    glade_register_widget (GTK_TYPE_GAMMA_CURVE, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_HANDLE_BOX, glade_standard_build_widget,
			   glade_standard_build_children, NULL);
    glade_register_widget (GTK_TYPE_HBUTTON_BOX, glade_standard_build_widget,
			   glade_standard_build_children, NULL);
    glade_register_widget (GTK_TYPE_HBOX, glade_standard_build_widget,
			   glade_standard_build_children, NULL);
    glade_register_widget (GTK_TYPE_HPANED, glade_standard_build_widget,
			   paned_build_children, NULL);
    glade_register_widget (GTK_TYPE_HRULER, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_HSCALE, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_HSCROLLBAR, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_HSEPARATOR, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_ICON_VIEW, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_IMAGE, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_IMAGE_MENU_ITEM, glade_standard_build_widget,
			   menuitem_build_children, image_menu_find_internal_child);
    glade_register_widget (GTK_TYPE_INPUT_DIALOG, NULL,
			   glade_standard_build_children, NULL);
    glade_register_widget (GTK_TYPE_LABEL, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_LAYOUT, glade_standard_build_widget,
			   layout_build_children, NULL);
    glade_register_widget (GTK_TYPE_LIST, glade_standard_build_widget,
			   glade_standard_build_children, NULL);
    glade_register_widget (GTK_TYPE_LIST_ITEM, glade_standard_build_widget,
			   glade_standard_build_children, NULL);
    glade_register_widget (GTK_TYPE_MENU, glade_standard_build_widget,
			   glade_standard_build_children, NULL);
    glade_register_widget (GTK_TYPE_MENU_BAR, glade_standard_build_widget,
			   glade_standard_build_children, NULL);
    glade_register_widget (GTK_TYPE_MENU_ITEM, glade_standard_build_widget,
			   menuitem_build_children, NULL);
    glade_register_widget (GTK_TYPE_MENU_TOOL_BUTTON, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_MESSAGE_DIALOG, NULL,
			   glade_standard_build_children, NULL);
    glade_register_widget (GTK_TYPE_NOTEBOOK, glade_standard_build_widget,
			   notebook_build_children, NULL);
    glade_register_widget (GTK_TYPE_OPTION_MENU, glade_standard_build_widget,
			   option_menu_build_children, NULL);
    glade_register_widget (GTK_TYPE_PIXMAP, glade_standard_build_widget,
			   NULL, NULL);
#ifdef HAVE_GTK_PLUG
    glade_register_widget (GTK_TYPE_PLUG, NULL,
			   NULL, NULL);
#endif
    glade_register_widget (GTK_TYPE_PREVIEW, build_preview,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_PROGRESS, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_PROGRESS_BAR, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_RADIO_BUTTON, glade_standard_build_widget,
			   glade_standard_build_children, NULL);
    glade_register_widget (GTK_TYPE_RADIO_MENU_ITEM, glade_standard_build_widget,
			   menuitem_build_children, NULL);
    glade_register_widget (GTK_TYPE_RADIO_TOOL_BUTTON, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_SCROLLED_WINDOW, glade_standard_build_widget,
			   glade_standard_build_children,
			   scrolled_window_find_internal_child);
    glade_register_widget (GTK_TYPE_SEPARATOR_MENU_ITEM, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_SEPARATOR_TOOL_ITEM, glade_standard_build_widget,
			   NULL, NULL);
#ifdef HAVE_GTK_PLUG
    glade_register_widget (GTK_TYPE_SOCKET, glade_standard_build_widget,
			   NULL, NULL);
#endif
    glade_register_widget (GTK_TYPE_SPIN_BUTTON, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_STATUSBAR, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_TABLE, glade_standard_build_widget,
			   glade_standard_build_children, NULL);
    glade_register_widget (GTK_TYPE_TEAROFF_MENU_ITEM, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_TEXT, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_TEXT_VIEW, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_TIPS_QUERY, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_TOGGLE_BUTTON, glade_standard_build_widget,
			   glade_standard_build_children, NULL);
    glade_register_widget (GTK_TYPE_TOGGLE_TOOL_BUTTON, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_TOOLBAR, glade_standard_build_widget,
			   toolbar_build_children, NULL);
    glade_register_widget (GTK_TYPE_TOOL_ITEM, glade_standard_build_widget,
			   glade_standard_build_children, NULL);
    glade_register_widget (GTK_TYPE_TOOL_BUTTON, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_TREE, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_TREE_VIEW, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_VBUTTON_BOX, glade_standard_build_widget,
			   glade_standard_build_children, NULL);
    glade_register_widget (GTK_TYPE_VBOX, glade_standard_build_widget,
			   glade_standard_build_children, NULL);
    glade_register_widget (GTK_TYPE_VPANED, glade_standard_build_widget,
			   paned_build_children, NULL);
    glade_register_widget (GTK_TYPE_VRULER, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_VSCALE, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_VSCROLLBAR, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_VSEPARATOR, glade_standard_build_widget,
			   NULL, NULL);
    glade_register_widget (GTK_TYPE_VIEWPORT, glade_standard_build_widget,
			   glade_standard_build_children, NULL);
    glade_register_widget (GTK_TYPE_WINDOW, NULL,
			   glade_standard_build_children, NULL);

    glade_provide("gtk");
}
