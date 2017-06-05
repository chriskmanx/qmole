/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2001-2012 Hiroyuki Yamamoto & The Claws Mail Team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * 
 */

/* (alfons) - based on a contribution by Satoshi Nagayasu; revised for colorful 
 * menu and more Sylpheed integration. The idea to put the code in a separate
 * file is just that it make it easier to allow "user changeable" label colors.
 */

#include "defs.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#include "colorlabel.h"
#include "utils.h"
#include "gtkutils.h"
#include "prefs_common.h"

static gchar *labels[COLORLABELS] = {
	N_("Orange"),
	N_("Red") ,
	N_("Pink"),
	N_("Sky blue"),
	N_("Blue"),
	N_("Green"),
	N_("Brown"),
	N_("Grey"),
	N_("Light brown"),
	N_("Dark red"),
	N_("Dark pink"),
	N_("Steel blue"),
	N_("Gold"),
	N_("Bright green"),
	N_("Magenta")
};

static GdkColor default_colors[COLORLABELS] = {
	{ 0, 0xffff, (0x99 << 8), 0x0 },
	{ 0, 0xffff, 0x0, 0x0 },
	{ 0, 0xffff, (0x66 << 8), 0xffff },
	{ 0, 0x0, (0xcc << 8), 0xffff },
	{ 0, 0x0, 0x0, 0xffff },
	{ 0, 0x0, (0x99 << 8), 0x0 },
	{ 0, (0x66 << 8), (0x33 << 8), (0x33 << 8) },
	{ 0, (0xaa << 8), (0xaa << 8), (0xaa << 8) },
	{ 0, (0xc0 << 8), (0x72 << 8), (0x54 << 8) },
	{ 0, (0xc0 << 8), 0x0, 0x0 },
	{ 0, (0xcc << 8), (0x10 << 8), (0x74 << 8) },
	{ 0, (0x50 << 8), (0x94 << 8), (0xcd << 8) },
	{ 0, 0xffff, (0xd5 << 8), 0x0 },
	{ 0, 0x0, (0xd8 << 8), 0x0 },
	{ 0, (0xc0 << 8), (0x60 << 8), (0xc0 << 8) }
};

	
typedef enum LabelColorChangeFlags_ {
	LCCF_COLOR = 1 << 0,
	LCCF_LABEL = 1 << 1,
	LCCF_ALL   = LCCF_COLOR | LCCF_LABEL
} LabelColorChangeFlags;

/* XXX: if you add colors, make sure you also check the procmsg.h.
 * color indices are stored as 3 bits; that explains the max. of 7 colors */
static struct 
{
	LabelColorChangeFlags	changed; 
	/* color here is initialized from default_colors[] at startup */
	GdkColor		color;

	/* XXX: note that the label member is supposed to be dynamically 
	 * allocated and freed */
	gchar			*label;
	GtkWidget		*widget;
} label_colors[NUM_MENUS][COLORLABELS] = {
    {
	{ LCCF_ALL, { 0 }, NULL, NULL },
	{ LCCF_ALL, { 0 }, NULL, NULL },
	{ LCCF_ALL, { 0 }, NULL, NULL },
	{ LCCF_ALL, { 0 }, NULL, NULL },
	{ LCCF_ALL, { 0 }, NULL, NULL },
	{ LCCF_ALL, { 0 }, NULL, NULL },
	{ LCCF_ALL, { 0 }, NULL, NULL },
	{ LCCF_ALL, { 0 }, NULL, NULL },
	{ LCCF_ALL, { 0 }, NULL, NULL },
	{ LCCF_ALL, { 0 }, NULL, NULL },
	{ LCCF_ALL, { 0 }, NULL, NULL },
	{ LCCF_ALL, { 0 }, NULL, NULL },
	{ LCCF_ALL, { 0 }, NULL, NULL },
	{ LCCF_ALL, { 0 }, NULL, NULL },
	{ LCCF_ALL, { 0 }, NULL, NULL }},
    {
	{ LCCF_ALL, { 0 }, NULL, NULL },
	{ LCCF_ALL, { 0 }, NULL, NULL },
	{ LCCF_ALL, { 0 }, NULL, NULL },
	{ LCCF_ALL, { 0 }, NULL, NULL },
	{ LCCF_ALL, { 0 }, NULL, NULL },
	{ LCCF_ALL, { 0 }, NULL, NULL },
	{ LCCF_ALL, { 0 }, NULL, NULL },
	{ LCCF_ALL, { 0 }, NULL, NULL },
	{ LCCF_ALL, { 0 }, NULL, NULL },
	{ LCCF_ALL, { 0 }, NULL, NULL },
	{ LCCF_ALL, { 0 }, NULL, NULL },
	{ LCCF_ALL, { 0 }, NULL, NULL },
	{ LCCF_ALL, { 0 }, NULL, NULL },
	{ LCCF_ALL, { 0 }, NULL, NULL },
	{ LCCF_ALL, { 0 }, NULL, NULL }}
};

#define LABEL_COLOR_WIDTH	28
#define LABEL_COLOR_HEIGHT	16

#define LABEL_COLORS_ELEMS (sizeof label_colors[0] / sizeof label_colors[0][0])

#define G_RETURN_VAL_IF_INVALID_COLOR(color, val) \
	do if ((color) < 0 || (color) >= LABEL_COLORS_ELEMS) {	\
		return val;				    	\
	} while(0)

#define INTCOLOR_TO_GDKCOLOR(intcolor, gdkcolor) \
	gdkcolor.red   = ((intcolor >> 16UL) & 0xFFUL) << 8UL; \
	gdkcolor.green = ((intcolor >>  8UL) & 0xFFUL) << 8UL; \
	gdkcolor.blue  = ((intcolor)         & 0xFFUL) << 8UL;

static void colorlabel_recreate        (gint);
static void colorlabel_recreate_label  (gint);

void colorlabel_update_colortable_from_prefs(void)
{
	gint i, c;

	for (i = 0; i < NUM_MENUS; i++) {
		for (c = 0; c < COLORLABELS; c++) {
			INTCOLOR_TO_GDKCOLOR(prefs_common.custom_colorlabel[c].color,
					label_colors[i][c].color);
			g_free(label_colors[i][c].label);
			label_colors[i][c].label =
					g_strdup(prefs_common.custom_colorlabel[c].label);
		}
	}
}


gint colorlabel_get_color_count(void)
{
	return LABEL_COLORS_ELEMS;
}

GdkColor colorlabel_get_color(gint color_index)
{
	GdkColor invalid = { 0 };

	G_RETURN_VAL_IF_INVALID_COLOR(color_index, invalid);

	return label_colors[0][color_index].color;
}

GdkColor colorlabel_get_default_color(gint color_index)
{
	GdkColor invalid = { 0 };

	G_RETURN_VAL_IF_INVALID_COLOR(color_index, invalid);

	return default_colors[color_index];
}
		
gchar *colorlabel_get_color_default_text(gint color_index)
{
	G_RETURN_VAL_IF_INVALID_COLOR(color_index, NULL);

	return labels[color_index];
}

static gboolean colorlabel_drawing_area_expose_event_cb
#if !GTK_CHECK_VERSION(3, 0, 0)
	(GtkWidget *widget, GdkEventExpose *expose, gpointer data)
#else
	(GtkWidget *widget, cairo_t *cr, gpointer data)
#endif
{
#if !GTK_CHECK_VERSION(3, 0, 0)
	cairo_t *cr;
	GdkWindow *drawable = gtk_widget_get_window(widget);
#endif
	GtkAllocation allocation;
	gulong c = (gulong) GPOINTER_TO_INT(data);
	GdkColor color;

	INTCOLOR_TO_GDKCOLOR(c, color)

#if !GTK_CHECK_VERSION(3, 0, 0)
	gdk_colormap_alloc_color(gtk_widget_get_colormap(widget), &color, FALSE, TRUE);
	cr = gdk_cairo_create(drawable);
#endif
	gtk_widget_get_allocation(widget, &allocation);

	cairo_set_source_rgb(cr, 0., 0., 0.);
	cairo_rectangle(cr, 0, 0,
	    allocation.width - 1,
	    allocation.height - 1);
	cairo_stroke(cr);
	gdk_cairo_set_source_color(cr, &color);
	cairo_rectangle(cr, 1, 1,
	    allocation.width - 2,
	    allocation.height - 2);
	cairo_fill(cr);
#if !GTK_CHECK_VERSION(3, 0, 0)
	cairo_destroy(cr);
#endif
	
	return FALSE;
}

static GtkWidget *colorlabel_create_color_widget(GdkColor color)
{
	GtkWidget *widget;

	widget = gtk_drawing_area_new();
	gtk_widget_set_size_request(widget, LABEL_COLOR_WIDTH - 2, 
				    LABEL_COLOR_HEIGHT - 4);

#define CL(x)		(((gulong) (x) >> (gulong) 8) & 0xFFUL)	
#define CR(r, g, b)	((CL(r) << (gulong) 16) | \
			 (CL(g) << (gulong)  8) | \
			 (CL(b)))

#if !GTK_CHECK_VERSION(3, 0, 0)
	g_signal_connect(G_OBJECT(widget), "expose_event", 
			 G_CALLBACK
			   	(colorlabel_drawing_area_expose_event_cb),
			 GINT_TO_POINTER
			   	((gint)CR(color.red, color.green, color.blue)));
#else
	g_signal_connect(G_OBJECT(widget), "draw", 
			 G_CALLBACK
			   	(colorlabel_drawing_area_expose_event_cb),
			 GINT_TO_POINTER
			   	((gint)CR(color.red, color.green, color.blue)));
#endif

	return widget;
}

/* XXX: colorlabel_recreate_XXX are there to make sure everything
 * is initialized ok, without having to call a global _xxx_init_
 * function */
static void colorlabel_recreate_color(gint color)
{
	GtkWidget *widget;
	int i;
	
	for (i = 0; i < NUM_MENUS; i++) {
		if (!(label_colors[i][color].changed & LCCF_COLOR))
			continue;

		widget = colorlabel_create_color_widget(label_colors[i][color].color);
		cm_return_if_fail(widget);

		if (label_colors[i][color].widget) 
			gtk_widget_destroy(label_colors[i][color].widget);

		label_colors[i][color].widget = widget;		
		label_colors[i][color].changed &= ~LCCF_COLOR;
	}
}

static void colorlabel_recreate_label(gint color)
{
	int i;
	
	for (i = 0; i < NUM_MENUS; i++) {
		if (!(label_colors[i][color].changed & LCCF_LABEL))
			continue;

		if (label_colors[i][color].label == NULL) 
			label_colors[i][color].label = g_strdup(gettext(labels[color]));

		label_colors[i][color].changed &= ~LCCF_LABEL;
	}
}

/* XXX: call this function everytime when you're doing important
 * stuff with the label_colors[] array */
static void colorlabel_recreate(gint color)
{
	colorlabel_recreate_label(color);
	colorlabel_recreate_color(color);
}

static void colorlabel_recreate_all(void)
{
	gint n;

	for ( n = 0; n < LABEL_COLORS_ELEMS; n++) 
		colorlabel_recreate(n);
}

/* colorlabel_create_check_color_menu_item() - creates a color
 * menu item with a check box */
GtkWidget *colorlabel_create_check_color_menu_item(gint color_index, gboolean force, gint menu_index)
{
	GtkWidget *label; 
	GtkWidget *hbox; 
	GtkWidget *vbox; 
	GtkWidget *item;
	gchar *accel;
	
	G_RETURN_VAL_IF_INVALID_COLOR(color_index, NULL);

	item = gtk_check_menu_item_new();

	if (force) {
		label_colors[menu_index][color_index].changed |= LCCF_COLOR;
		label_colors[menu_index][color_index].changed |= LCCF_LABEL;
	}
	colorlabel_recreate(color_index);

	/* XXX: gnome-core::panel::menu.c is a great example of
	 * how to create pixmap menus */
	label = gtk_label_new(label_colors[menu_index][color_index].label);

	gtk_widget_show(label);
	hbox = gtk_hbox_new(FALSE, 0);
	gtk_widget_show(hbox);
	gtk_container_add(GTK_CONTAINER(item), hbox);

	vbox = gtk_vbox_new(TRUE, 0);
	gtk_widget_show(vbox);
	gtk_container_set_border_width(GTK_CONTAINER(vbox), 1);

	gtk_container_add(GTK_CONTAINER(vbox),
			  label_colors[menu_index][color_index].widget);
	gtk_widget_show(label_colors[menu_index][color_index].widget);

	gtk_box_pack_start(GTK_BOX(hbox), vbox, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 4);
	if (color_index < 9) {
		accel = gtk_accelerator_get_label(GDK_KEY_1+color_index, GDK_CONTROL_MASK);
		label = gtk_label_new(accel);
		gtk_widget_show(label);
		gtk_misc_set_alignment(GTK_MISC(label), 1, 0.5);
		g_free(accel);
		gtk_box_pack_start(GTK_BOX(hbox), label, TRUE, TRUE, 4);
		g_object_set_data(G_OBJECT(item), "accel_label", label);
	} else {
		label = gtk_label_new("");
		gtk_widget_show(label);
		gtk_misc_set_alignment(GTK_MISC(label), 1, 0.5);
		gtk_box_pack_start(GTK_BOX(hbox), label, TRUE, TRUE, 4);
		g_object_set_data(G_OBJECT(item), "accel_label", label);
	}
	return item;
}

/* Work around a gtk bug (?): without that, the selected menu item's 
 * colored rectangle is drawn at 0,0 in the window...
 */
static void refresh_menu (GtkWidget *menushell, gpointer data)
{
	GtkMenu *menu = (GtkMenu *)data;
	GtkWidget *widget = gtk_menu_get_attach_widget(menu);
	gtk_widget_hide_all(widget);
	gtk_widget_unrealize(widget);
	gtk_widget_show_all(widget);
	gtk_widget_queue_draw(widget);
}

/* colorlabel_create_color_menu() - creates a color menu without 
 * checkitems, probably for use in combo items */
GtkWidget *colorlabel_create_color_menu(void)
{
	GtkWidget *label; 
	GtkWidget *item;
	GtkWidget *menu;
	gint i;

	colorlabel_recreate_all();

	/* create the menu items. each item has its color code attached */
	menu = gtk_menu_new();
	g_object_set_data(G_OBJECT(menu), "label_color_menu", menu);

	item = gtk_menu_item_new_with_label(_("None"));
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
	g_object_set_data(G_OBJECT(item), "color", GUINT_TO_POINTER(0));
	gtk_widget_show(item);

	item = gtk_separator_menu_item_new();
	gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
	gtk_widget_show(item);

	/* and the color items */
	for (i = 0; i < LABEL_COLORS_ELEMS; i++) {
		GtkWidget *hbox; 
		GtkWidget *vbox;
		GtkWidget *widget;

		item  = gtk_menu_item_new();
		g_object_set_data(G_OBJECT(item), "color",
				  GUINT_TO_POINTER(i + 1));

		label = gtk_label_new(label_colors[0][i].label);
		
		gtk_widget_show(label);
		hbox = gtk_hbox_new(FALSE, 0);
		gtk_widget_show(hbox);
		gtk_container_add(GTK_CONTAINER(item), hbox);

		vbox = gtk_vbox_new(TRUE, 0);
		gtk_widget_show(vbox);
		gtk_container_set_border_width(GTK_CONTAINER(vbox), 1);

		widget = colorlabel_create_color_widget(label_colors[0][i].color);
		gtk_widget_show(widget);
		gtk_box_pack_start(GTK_BOX(vbox), widget, FALSE, FALSE, 0);

		gtk_box_pack_start(GTK_BOX(hbox), vbox, FALSE, FALSE, 0);
		gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 4);
		
		gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
		gtk_widget_show(item);
	}
	
	g_signal_connect(G_OBJECT(menu), "selection-done", 
			G_CALLBACK(refresh_menu), menu);
	gtk_widget_show(menu);

	return menu;
}

guint colorlabel_get_color_menu_active_item(GtkWidget *menu)
{
	GtkWidget *menuitem;
	guint color;

	menuitem = gtk_menu_get_active(GTK_MENU(menu));
	color = GPOINTER_TO_UINT
		(g_object_get_data(G_OBJECT(menuitem), "color"));
	return color;
}
