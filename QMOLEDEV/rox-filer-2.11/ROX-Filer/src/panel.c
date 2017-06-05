/*
 * ROX-Filer, filer for the ROX desktop project
 * Copyright (C) 2006, Thomas Leonard and others (see changelog for details).
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 59 Temple
 * Place, Suite 330, Boston, MA  02111-1307  USA
 */

/* panel.c - code for dealing with panel windows */

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <unistd.h>
#include <libxml/parser.h>

#include <gtk/gtk.h>
#include <gdk/gdkx.h>

#include "global.h"

#include "panel.h"
#include "options.h"
#include "choices.h"
#include "main.h"
#include "type.h"
#include "gui_support.h"
#include "diritem.h"
#include "pixmaps.h"
#include "filer.h"
#include "display.h"
#include "bind.h"
#include "dnd.h"
#include "support.h"
#include "icon.h"
#include "run.h"
#include "appinfo.h"
#include "pixmaps.h"
#include "xml.h"
#include "pinboard.h"		/* For pinboard_get_window() */

/* The width of the separator at the inner edge of the panel */
#define EDGE_WIDTH 1

/* The gap between panel icons */
#define PANEL_ICON_SPACING 8

enum {TEXT_BESIDE_ICON, TEXT_UNDER_ICON};

static gboolean tmp_icon_selected = FALSE;		/* When dragging */

typedef struct _PanelIconClass PanelIconClass;
typedef struct _PanelIcon PanelIcon;

struct _PanelIconClass {
	IconClass parent;
};

struct _PanelIcon {
	Icon		icon;
	GdkPixbuf 	*image;

	Panel		*panel;
	GtkWidget	*widget;	/* The drawing area for the icon */
	GtkWidget	*label;
	GtkWidget	*socket;	/* For applets */

	int		style;
};

#define PANEL_ICON(obj) GTK_CHECK_CAST((obj), panel_icon_get_type(), PanelIcon)
#define IS_PANEL_ICON(obj) \
	G_TYPE_CHECK_INSTANCE_TYPE((obj), panel_icon_get_type())

Panel *current_panel[PANEL_NUMBER_OF_SIDES];

/* NULL => Not loading a panel */
static Panel *loading_panel = NULL;

static GtkWidget *panel_options_dialog = NULL;

/* Static prototypes */
static int panel_delete(GtkWidget *widget, GdkEvent *event, Panel *panel);
static void panel_destroyed(GtkWidget *widget, Panel *panel);
static const char *pan_from_file(gchar *line);
static gint icon_button_release(GtkWidget *widget,
				GdkEventButton *event,
				PanelIcon *pi);
static gint icon_button_press(GtkWidget *widget,
			      GdkEventButton *event,
			      PanelIcon *pi);
static void reposition_panel(GtkWidget *window,
				GtkAllocation *alloc, Panel *panel);
static gint expose_icon(GtkWidget *widget,
			GdkEventExpose *event,
			PanelIcon *pi);
static gint draw_icon(GtkWidget *widget,
			GdkRectangle *badarea,
			PanelIcon *pi);
static gint panel_button_release(GtkWidget *widget,
			      GdkEventButton *event,
			      Panel *panel);
static gint panel_button_press(GtkWidget *widget,
			      GdkEventButton *event,
			      Panel *panel);
static void panel_post_resize(GtkWidget *box,
			GtkRequisition *req, Panel *panel);
static void drag_set_panel_dest(PanelIcon *pi);
static void add_uri_list(GtkWidget          *widget,
			 GdkDragContext     *context,
			 gint               x,
			 gint               y,
			 GtkSelectionData   *selection_data,
			 guint              info,
			 guint32            time,
			 Panel		    *panel);
static void panel_add_item(Panel *panel,
			   const gchar *path,
			   const gchar *name,
			   gboolean after,
			   const gchar *shortcut,
			   const gchar *args,
			   gboolean locked);
static gboolean panel_drag_motion(GtkWidget	*widget,
			    GdkDragContext	*context,
			    gint		x,
			    gint		y,
			    guint		time,
			    Panel		*panel);
static gboolean insert_drag_motion(GtkWidget	*widget,
			    GdkDragContext	*context,
			    gint		x,
			    gint		y,
			    guint		time,
			    Panel		*panel);
static gboolean drag_motion(GtkWidget		*widget,
			    GdkDragContext	*context,
			    gint		x,
			    gint		y,
			    guint		time,
			    PanelIcon		*pi);
static void panel_drag_leave(GtkWidget	*widget,
		       GdkDragContext	*context,
		       guint32		time,
		       Panel		*panel);
static void drag_leave(GtkWidget	*widget,
		       GdkDragContext	*context,
		       guint32		time,
		       Icon	*icon);
static GtkWidget *make_insert_frame(Panel *panel);
static gboolean enter_icon(GtkWidget *widget,
			   GdkEventCrossing *event,
			   Icon *icon);
static gint icon_motion_event(GtkWidget *widget,
			      GdkEventMotion *event,
			      PanelIcon *pi);
static gint panel_leave_event(GtkWidget *widget,
			      GdkEventCrossing *event,
			      Panel *panel);
static gint panel_motion_event(GtkWidget *widget,
			      GdkEventMotion *event,
			      Panel *panel);
static void reposition_icon(PanelIcon *pi, int index);
static void start_drag(PanelIcon *pi, GdkEventMotion *event);
static void drag_end(GtkWidget *widget,
		     GdkDragContext *context,
		     Icon *icon);
static void perform_action(Panel *panel,
			   PanelIcon *pi,
			   GdkEventButton *event);
static void run_applet(PanelIcon *pi);
static void size_request(GtkWidget *widget, GtkRequisition *req, PanelIcon *pi);
static void panel_load_from_xml(Panel *panel, xmlDocPtr doc);
static gboolean draw_panel_edge(GtkWidget *widget, GdkEventExpose *event,
				Panel *panel);
static PanelIcon *panel_icon_new(Panel *panel,
				 const char *pathname,
				 const char *name);
static GType panel_icon_get_type(void);
static gboolean panel_want_show_text(PanelIcon *pi);
static void panel_show_menu(GdkEventButton *event, PanelIcon *pi, Panel *panel);
static void panel_style_changed(void);
static void motion_may_raise(Panel *panel, int x, int y);
static void panel_update(Panel *panel);
static GList *build_monitor_number(Option *option,
					xmlNode *node, guchar *label);
static gboolean may_autoscroll(Panel *panel);
static void panel_update_geometry(Panel *panel);
static gboolean panel_keep_below(Panel *panel, gboolean setting);


static GtkWidget *dnd_highlight = NULL; /* (stops flickering) */

#define SHOW_BOTH 0
#define SHOW_APPS_SMALL 1
#define SHOW_ICON 2
static Option o_panel_style;
static Option o_panel_width;
static Option o_panel_xinerama;
static Option o_panel_monitor;
static Option o_panel_avoid;
static Option o_panel_is_dock;
static Option o_panel_on_top;
static Option o_panel_obey_workarea;

static gint panel_monitor = -1;

static int closing_panel = 0;	/* Don't panel_save; destroying! */

/****************************************************************
 *			EXTERNAL INTERFACE			*
 ****************************************************************/

void panel_init(void)
{
	option_add_int(&o_panel_style, "panel_style", SHOW_APPS_SMALL);
	option_add_int(&o_panel_width, "panel_width", 52);

	option_add_int(&o_panel_xinerama, "panel_xinerama", 0);
	option_add_int(&o_panel_monitor, "panel_monitor", 0);

	option_add_int(&o_panel_avoid, "panel_avoid", TRUE);
	option_add_int(&o_panel_is_dock, "panel_is_dock", TRUE);
	option_add_int(&o_panel_on_top, "panel_on_top", FALSE);

	option_add_int(&o_panel_obey_workarea, "panel_obey_workarea", FALSE);

	option_add_notify(panel_style_changed);

	option_register_widget("monitor-number", build_monitor_number);
}

/* Return a free edge for a new panel.
 * If no edge is free, returns PANEL_BOTTOM.
 */
static PanelSide find_free_side()
{
	if (!current_panel[PANEL_BOTTOM])
		return PANEL_BOTTOM;

	if (!current_panel[PANEL_TOP])
		return PANEL_TOP;

	if (!current_panel[PANEL_LEFT])
		return PANEL_LEFT;

	if (!current_panel[PANEL_RIGHT])
		return PANEL_RIGHT;

	return PANEL_BOTTOM;
}

/* Returns TRUE and sets *target if the property exists, otherwise returns
 * FALSE and leaves *target unchanged */
static gboolean get_int_prop(xmlNodePtr node, const char *name, int *target)
{
	char *prop = xmlGetProp(node, name);

	if (prop)
	{
		*target = atoi(prop);
		g_free(prop);
		return TRUE;
	}
	return FALSE;
}

static void set_int_prop(xmlNodePtr node, const char *name, int value)
{
	char prop[16];

	sprintf(prop, "%d", value);
	xmlSetProp(node, name, prop);
}

static void panel_load_options_from_xml(Panel *panel, xmlDocPtr doc)
{
	xmlNodePtr root;
	xmlNodePtr options;

	root = xmlDocGetRootElement(doc);
	options = get_subnode(root, NULL, "options");
	if (!options)
		return;
	get_int_prop(options, "style", &panel->style);
	get_int_prop(options, "width", &panel->width);
	get_int_prop(options, "avoid", &panel->avoid);
	get_int_prop(options, "xinerama", &panel->xinerama);
	get_int_prop(options, "monitor", &panel->monitor);
}

static void save_panels(void)
{
	char *filename = choices_find_xdg_path_save("panels",
				"ROX-Filer", "rox.sourceforge.net", TRUE);
	char *tmp = g_strconcat(filename, ".new", NULL);
	FILE *fp = fopen(tmp, "w");

	if (fp)
	{
		PanelSide n;

		for (n = 0; n < PANEL_NUMBER_OF_SIDES; ++n)
		{
			if (current_panel[n])
				fprintf(fp, "%s\n", current_panel[n]->name);
		}
		fclose(fp);
		if (rename(tmp, filename))
			g_critical(_("Unable to replace '%s'"), filename);
	}
	else
	{
		g_critical(_("Unable to save '%s'"), tmp);
	}
	g_free(tmp);
	g_free(filename);
}

/* 'name' may be NULL or "" to remove the panel */
Panel *panel_new(const gchar *name, PanelSide side)
{
	guchar	*load_path;
	Panel	*panel;
	GtkWidget *vp, *box, *frame, *align;
	xmlDocPtr panel_doc = NULL;
	gboolean need_resave = FALSE;

	g_return_val_if_fail(side == PANEL_DEFAULT_SIDE ||
			(side >= 0 && side < PANEL_NUMBER_OF_SIDES), NULL);
	g_return_val_if_fail(loading_panel == NULL, NULL);

	if (name && *name == '\0')
		name = NULL;

	if (!name)
		load_path = NULL;
	else if (strchr(name, '/'))
		load_path = g_strdup(name);
	else
	{
		guchar	*leaf;

		leaf = g_strconcat("pan_", name, NULL);
		load_path = choices_find_xdg_path_load(leaf, PROJECT, SITE);
		g_free(leaf);
	}

	if (load_path && access(load_path, F_OK) == 0)
	{
		char *saved_side;
		xmlNodePtr root;

		panel_doc = xmlParseFile(load_path);
		root = xmlDocGetRootElement(panel_doc);

		saved_side = xmlGetProp(root, "side");
		if (saved_side)
		{
			PanelSide old_side;
			old_side = panel_name_to_side(saved_side);
			g_free(saved_side);

			if (side == PANEL_DEFAULT_SIDE)
				side = old_side;
			else if (side != old_side)
				need_resave = TRUE;
		}
		else
			need_resave = TRUE;
	}

	if (side == PANEL_DEFAULT_SIDE)
		side = find_free_side();

	if (current_panel[side])
	{
		if (name)
			number_of_windows++;
		closing_panel++;
		gtk_widget_destroy(current_panel[side]->window);
		closing_panel--;
		if (name)
			number_of_windows--;
	}

	if (name == NULL || *name == '\0')
	{
		save_panels();
		return NULL;
	}

	panel = g_new(Panel, 1);
	panel->name = g_strdup(name);
	panel->side = side;
	panel->window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	panel->autoscroll_speed = 0;

	/* These are fallbacks from legacy global options */
	panel->style = o_panel_style.int_value;
	panel->width = o_panel_width.int_value;
	panel->xinerama = o_panel_xinerama.int_value;
	panel->monitor = o_panel_monitor.int_value;
	panel->avoid = o_panel_avoid.int_value;

	/* Now try to load options from this panel's XML */
	if (panel_doc)
	{
		panel_load_options_from_xml(panel, panel_doc);
	}
	else
	{
		/* Otherwise ensure old settings are migrated */
		need_resave = TRUE;
	}

	panel_update_geometry(panel);

	gtk_window_set_resizable(GTK_WINDOW(panel->window), FALSE);
	gtk_window_set_wmclass(GTK_WINDOW(panel->window), "ROX-Panel", PROJECT);
	gtk_widget_set_name(panel->window, "rox-panel");
	gtk_widget_set_events(panel->window,
			GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK |
			GDK_POINTER_MOTION_MASK | GDK_LEAVE_NOTIFY_MASK);

	/* We make the panel a drop target only so that we can auto-raise! */
	gtk_drag_dest_set(panel->window, 0, NULL, 0, GDK_ACTION_PRIVATE);
	g_signal_connect(panel->window, "drag_leave",
			G_CALLBACK(panel_drag_leave), panel);
	g_signal_connect(panel->window, "drag_motion",
			G_CALLBACK(panel_drag_motion), panel);

	g_signal_connect(panel->window, "delete-event",
			G_CALLBACK(panel_delete), panel);
	g_signal_connect(panel->window, "destroy",
			G_CALLBACK(panel_destroyed), panel);
	g_signal_connect(panel->window, "button_press_event",
			G_CALLBACK(panel_button_press), panel);
	g_signal_connect(panel->window, "button_release_event",
			G_CALLBACK(panel_button_release), panel);
	g_signal_connect(panel->window, "motion-notify-event",
			G_CALLBACK(panel_motion_event), panel);
	g_signal_connect(panel->window, "leave-notify-event",
			G_CALLBACK(panel_leave_event), panel);

	if (panel->side == PANEL_RIGHT)
		align = gtk_alignment_new(1.0, 0.0, 0.0, 1.0);
	else if (panel->side == PANEL_BOTTOM)
		align = gtk_alignment_new(0.0, 1.0, 1.0, 0.0);
	else if (panel->side == PANEL_TOP)
		align = gtk_alignment_new(0.0, 0.0, 1.0, 0.0);
	else
		align = gtk_alignment_new(0.0, 0.0, 0.0, 1.0);

	gtk_container_add(GTK_CONTAINER(panel->window), align);

	vp = gtk_viewport_new(NULL, NULL);
	gtk_container_set_resize_mode(GTK_CONTAINER(vp), GTK_RESIZE_PARENT);
	gtk_viewport_set_shadow_type(GTK_VIEWPORT(vp), GTK_SHADOW_NONE);
	gtk_container_add(GTK_CONTAINER(align), vp);

	g_signal_connect(align, "expose-event",
			 G_CALLBACK(draw_panel_edge), panel);

	if (side == PANEL_TOP || side == PANEL_BOTTOM)
	{
		panel->adj = gtk_viewport_get_hadjustment(GTK_VIEWPORT(vp));
		box = gtk_hbox_new(FALSE, 0);
		panel->before = gtk_hbox_new(FALSE, 0);
		panel->after = gtk_hbox_new(FALSE, 0);
	}
	else
	{
		panel->adj = gtk_viewport_get_vadjustment(GTK_VIEWPORT(vp));
		box = gtk_vbox_new(FALSE, 0);
		panel->before = gtk_vbox_new(FALSE, 0);
		panel->after = gtk_vbox_new(FALSE, 0);
	}

	gtk_container_add(GTK_CONTAINER(vp), box);
	gtk_box_pack_start(GTK_BOX(box), panel->before, FALSE, TRUE, 0);
	gtk_box_pack_end(GTK_BOX(box), panel->after, FALSE, TRUE, 0);

	frame = make_insert_frame(panel);
	gtk_box_pack_start(GTK_BOX(box), frame, TRUE, TRUE, 4);

	/* This is used so that we can find the middle easily! */
	panel->gap = gtk_event_box_new();
	gtk_box_pack_start(GTK_BOX(box), panel->gap, FALSE, FALSE, 0);
	
	frame = make_insert_frame(panel);
	g_object_set_data(G_OBJECT(frame), "after", "yes");
	gtk_box_pack_start(GTK_BOX(box), frame, TRUE, TRUE, 4);

	if (o_panel_is_dock.int_value)
		gtk_window_set_type_hint(GTK_WINDOW(panel->window),
				GDK_WINDOW_TYPE_HINT_DOCK);	

	gtk_widget_realize(panel->window);
	make_panel_window(panel->window);
	gtk_window_stick(GTK_WINDOW(panel->window));
	
	gtk_widget_show_all(align);
	
	loading_panel = panel;
	if (panel_doc)
	{
		panel_load_from_xml(panel, panel_doc);
		xmlFreeDoc(panel_doc);

		if (need_resave)
			panel_save(panel);
	}
	else if (load_path)
	{
		parse_file(load_path, pan_from_file);
		info_message(_("Your old panel file has been "
					"converted to the new XML format."));
		panel_save(panel);
	}
	else
	{
		/* Don't scare users with an empty panel... */
		guchar *apps;
		
		panel_add_item(panel, "~", "Home", FALSE, NULL, NULL, FALSE);

		apps = pathdup(make_path(app_dir, ".."));
		if (apps)
		{
			panel_add_item(panel, apps, "Apps", FALSE, NULL, NULL, FALSE);
			g_free(apps);
		}
	}
	loading_panel = NULL;
	g_free(load_path);

	current_panel[side] = panel;

	gtk_widget_queue_resize(box);
	g_signal_connect(panel->window, "size-request",
			G_CALLBACK(panel_post_resize), panel);
	g_signal_connect(panel->window, "size-allocate",
			G_CALLBACK(reposition_panel), panel);

	number_of_windows++;
	gdk_window_lower(panel->window->window);
	gtk_widget_show(panel->window);
	/* This has no effect until after window is showing; GTK+ bug? */
	if (panel_keep_below(panel, TRUE))
	{
		GdkWindow *pinboard;

		pinboard = pinboard_get_window();
		/* (if pinboard is NULL, will go right to the back) */
		window_put_just_above(panel->window->window, pinboard);
	}

	save_panels();

	return panel;
}

/* Externally visible function to add an item to a panel */
gboolean panel_add(PanelSide side,
		   const gchar *path, const gchar *label, gboolean after, const gchar *shortcut, const gchar *args, 
		   gboolean locked)
{
	g_return_val_if_fail(side >= 0 && side < PANEL_NUMBER_OF_SIDES, FALSE);
	
	g_return_val_if_fail(current_panel[side] != NULL, FALSE);

	panel_add_item(current_panel[side], path, label, after, shortcut, args, locked);

	return TRUE;
}

/* Add the area covered by the panels to the region */
void panel_mark_used(GdkRegion *used)
{
	int i;

	for (i = 0; i < PANEL_NUMBER_OF_SIDES; i++)
	{
		Panel *panel = current_panel[i];
		GdkRectangle rect;

		if (!panel)
			continue;

		gdk_window_get_root_origin(panel->window->window,
					   &rect.x, &rect.y);
		rect.width = panel->window->allocation.width;
		rect.height = panel->window->allocation.height;

		gdk_region_union_with_rect(used, &rect);
	}
}

/* On xrandr screen size changes, update all panels */
void panel_update_size(void)
{
	int i;

	for (i = 0; i < PANEL_NUMBER_OF_SIDES; i++)
	{
		if (current_panel[i])
		{
			panel_update_geometry(current_panel[i]);
			reposition_panel(current_panel[i]->window,
					&current_panel[i]->window->allocation,
					current_panel[i]);
			gtk_widget_queue_resize(current_panel[i]->window);
		}
	}
}

/****************************************************************
 *			INTERNAL FUNCTIONS			*
 ****************************************************************/

/* User has tried to close the panel via the window manager - confirm */
static int panel_delete(GtkWidget *widget, GdkEvent *event, Panel *panel)
{
	return !confirm(_("You have tried to close a panel via the window "
			 "manager - I usually find that this is accidental... "
			 "really close?"),
			 GTK_STOCK_CLOSE, NULL);
}

static void panel_destroyed(GtkWidget *widget, Panel *panel)
{
	if (panel_options_dialog)
	{
		Panel *dlg_panel = g_object_get_data(G_OBJECT(panel_options_dialog),
					 "rox-panel");

		if (dlg_panel == panel)
			gtk_widget_destroy(panel_options_dialog);
	}

	if (current_panel[panel->side] == panel)
		current_panel[panel->side] = NULL;

	if (panel->side == PANEL_TOP || panel->side == PANEL_BOTTOM)
	{
		if (current_panel[PANEL_RIGHT])
			gtk_widget_queue_resize(
					current_panel[PANEL_RIGHT]->window);
		if (current_panel[PANEL_LEFT])
			gtk_widget_queue_resize(
					current_panel[PANEL_LEFT]->window);
	}

	if (panel->autoscroll_speed)
		g_source_remove(panel->autoscroll_to);

	g_free(panel->name);
	g_free(panel);

	one_less_window();
}

static void panel_load_side(Panel *panel, xmlNodePtr side, gboolean after)
{
	xmlNodePtr node;
	char	   *label, *path, *shortcut, *args, *tmp;
	gboolean locked;

	for (node = side->xmlChildrenNode; node; node = node->next)
	{
		if (node->type != XML_ELEMENT_NODE)
			continue;
		if (strcmp(node->name, "icon") != 0)
			continue;

		label = xmlGetProp(node, "label");
		if (!label)
			label = g_strdup("<missing label>");
		path = xmlNodeGetContent(node);
		if (!path)
			path = g_strdup("<missing path>");
		shortcut = xmlGetProp(node, "shortcut");
		args = xmlGetProp(node, "args");
		tmp = xmlGetProp(node, "locked");
		if (tmp)
		{
			locked = text_to_boolean(tmp, FALSE);
			g_free(tmp);
		}
		else
			locked = FALSE;

		panel_add_item(panel, path, label, after, shortcut, args, locked);

		g_free(path);
		g_free(label);
		g_free(shortcut);
		g_free(args);
	}
}

/* Create one panel icon for each icon in the doc */
static void panel_load_from_xml(Panel *panel, xmlDocPtr doc)
{
	xmlNodePtr root;

	root = xmlDocGetRootElement(doc);
	panel_load_side(panel, get_subnode(root, NULL, "start"), FALSE);
	panel_load_side(panel, get_subnode(root, NULL, "end"), TRUE);
}

/* Called for each line in the config file while loading a new panel */
static const char *pan_from_file(gchar *line)
{
	gchar	*sep, *leaf;
	
	g_return_val_if_fail(line != NULL, NULL);
	g_return_val_if_fail(loading_panel != NULL, NULL);

	if (*line == '\0')
		return NULL;

	sep = strpbrk(line, "<>");
	if (!sep)
		return _("Missing < or > in panel config file");

	if (sep != line)
		leaf = g_strndup(line, sep - line);
	else
		leaf = NULL;
	
	panel_add_item(loading_panel, sep + 1, leaf, sep[0] == '>',
		       NULL, NULL, FALSE);

	g_free(leaf);

	return NULL;
}

static gboolean icon_pointer_in(GtkWidget *widget,
				GdkEventCrossing *event,
				Icon *icon)
{
	gtk_widget_set_state(widget,
		icon->selected ? GTK_STATE_SELECTED : GTK_STATE_PRELIGHT);

	return 0;
}

static gboolean icon_pointer_out(GtkWidget *widget,
				 GdkEventCrossing *event,
				 Icon *icon)
{
	gtk_widget_set_state(widget,
		icon->selected ? GTK_STATE_SELECTED : GTK_STATE_NORMAL);

	return 0;
}

static void panel_icon_destroyed(PanelIcon *pi)
{
	g_return_if_fail(pi->widget != NULL);

	pi->widget = NULL;

	g_object_unref(pi);
}

/* Set the tooltip AND hide/show the label */
static void panel_icon_set_tip(PanelIcon *pi)
{
	XMLwrapper	*ai;
	xmlNode 	*node;
	Icon		*icon = (Icon *) pi;

	g_return_if_fail(pi != NULL);

	if (pi->label)
	{
		if (panel_want_show_text(pi))
			gtk_widget_show(pi->label);
		else
			gtk_widget_hide(pi->label);
	}

	if (pi->socket)
		ai = NULL;
	else
		ai = appinfo_get(icon->path, icon->item);

	if (ai && ((node = xml_get_section(ai, NULL, "Summary"))))
	{
		guchar *str;
		str = xmlNodeListGetString(node->doc,
				node->xmlChildrenNode, 1);
		if (str)
		{
			gtk_tooltips_set_tip(tooltips, pi->widget, str, NULL);
			g_free(str);
		}
	}
	else if ((!panel_want_show_text(pi)) && !pi->socket)
	{
		if (icon->item->leafname && icon->item->leafname[0])
			gtk_tooltips_set_tip(tooltips, pi->widget,
					icon->item->leafname, NULL);
	}
	else
		gtk_tooltips_set_tip(tooltips, pi->widget, NULL, NULL);

	if (ai)
		g_object_unref(ai);
}

/* Add an icon with this path to the panel. If after is TRUE then the
 * icon is added to the right/bottom end of the panel.
 *
 * If name is NULL a suitable name is taken from path.
 */
static void panel_add_item(Panel *panel,
			   const gchar *path,
			   const gchar *name,
			   gboolean after,
			   const gchar *shortcut,
			   const gchar *args,
			   gboolean locked)
{
	GtkWidget	*widget;
	PanelIcon	*pi;
	Icon		*icon;

	g_return_if_fail(panel != NULL);
	g_return_if_fail(path != NULL);

	widget = gtk_event_box_new();
	gtk_widget_set_events(widget,
			GDK_BUTTON1_MOTION_MASK | GDK_BUTTON2_MOTION_MASK |
			GDK_BUTTON3_MOTION_MASK |
			GDK_EXPOSURE_MASK | GDK_BUTTON_PRESS_MASK |
			GDK_BUTTON_RELEASE_MASK);
	
	gtk_box_pack_start(GTK_BOX(after ? panel->after : panel->before),
			widget, FALSE, TRUE, 0);
	if (after)
		gtk_box_reorder_child(GTK_BOX(panel->after), widget, 0);
	
	gtk_widget_realize(widget);

	pi = panel_icon_new(panel, path, name);
	icon = (Icon *) pi;

	/* Widget takes the initial ref of Icon */
	g_object_set_data(G_OBJECT(widget), "icon", pi);
	
	pi->widget = widget;
	g_object_ref(widget);

	gtk_widget_set_name(pi->widget, "panel-icon");

	g_signal_connect_swapped(widget, "destroy",
			  G_CALLBACK(panel_icon_destroyed), pi);

	if (icon->item->base_type == TYPE_DIRECTORY)
		run_applet(pi);

	g_signal_connect(widget, "button_release_event",
			G_CALLBACK(icon_button_release), pi);
	g_signal_connect(widget, "button_press_event",
			G_CALLBACK(icon_button_press), pi);
	g_signal_connect(widget, "motion-notify-event",
			G_CALLBACK(icon_motion_event), pi);
	g_signal_connect(widget, "enter-notify-event",
			G_CALLBACK(icon_pointer_in), pi);
	g_signal_connect(widget, "leave-notify-event",
			G_CALLBACK(icon_pointer_out), pi);

	if (!pi->socket)
	{
		g_signal_connect(widget, "enter-notify-event",
				G_CALLBACK(enter_icon), pi);
		g_signal_connect_after(widget, "expose_event",
				G_CALLBACK(expose_icon), pi);
		g_signal_connect(widget, "drag_data_get",
				G_CALLBACK(drag_data_get), NULL);

		g_signal_connect(widget, "size_request",
				G_CALLBACK(size_request), pi);

		drag_set_panel_dest(pi);

		pi->label = gtk_label_new(icon->item->leafname);
		gtk_container_add(GTK_CONTAINER(pi->widget), pi->label);
		gtk_misc_set_alignment(GTK_MISC(pi->label), 0.5, 1);
		gtk_misc_set_padding(GTK_MISC(pi->label), 1, 2);
	}

	icon_set_shortcut(icon, shortcut);
	icon_set_arguments(icon, args);
	icon->locked = locked;

	if (!loading_panel)
		panel_save(panel);

	panel_icon_set_tip(pi);
	gtk_widget_show(widget);
}

static gboolean remove_item_from_side(GtkWidget *container, const gchar *path,
				      const gchar *label)
{
	GList *kids, *next;
	gboolean found = FALSE;

	kids = gtk_container_get_children(GTK_CONTAINER(container));

	for (next = kids; next; next = next->next)
	{
		Icon    *icon;
		icon = g_object_get_data(G_OBJECT(next->data), "icon");
		if (!icon)
			continue;

		if ((!path || strcmp(path, icon->src_path) == 0) &&
		    (!label || strcmp(label, icon->item->leafname)==0))
		{
			icon->locked = FALSE;
			icon_destroy(icon);
			found = TRUE;
			break;
		}
	}
	
	g_list_free(kids);

	return found;
}

/* Remove an item with this path. If more than one item matches, only
 * one is removed. If label is not NULL then it must also match the item.
 * Returns TRUE if an item was successfully removed.
 */
gboolean panel_remove_item(PanelSide side, const gchar *path,
			   const gchar *label)
{
	Panel *panel;

	g_return_val_if_fail(side >= 0 && side < PANEL_NUMBER_OF_SIDES, FALSE);

	g_return_val_if_fail(path != NULL || label != NULL, FALSE);

	panel = current_panel[side];
	if (!panel)
	{
		g_warning("No panel on this side of the screen!");
		return FALSE;
	}

	if (remove_item_from_side(panel->before, path, label) ||
	    remove_item_from_side(panel->after, path, label))
	{
		panel_save(panel);
		panel_update(panel);
		return TRUE;
	}

	g_warning("Panel item path='%s', label='%s' not found", path, label);
	return FALSE;
}

/* Called when Gtk+ wants to know how much space an icon needs.
 * 'req' is already big enough for the label, if shown.
 */
static void size_request(GtkWidget *widget, GtkRequisition *req, PanelIcon *pi)
{
	Icon *icon = (Icon *) pi;
	gboolean horz = (pi->panel->side == PANEL_TOP ||
			 pi->panel->side == PANEL_BOTTOM);
	int max_width = 100;
	int max_height = 100;
	int image_width, image_height;
	Panel *panel = pi->panel;

	if (horz)
		max_height = panel->width - req->height;
	else
		max_width = MAX(panel->width, req->width);

	/* TODO: really need to recreate? */
	if (pi->image)
		g_object_unref(pi->image);

	pi->image = scale_pixbuf(di_image(icon->item)->src_pixbuf,
			MAX(20, max_width), MAX(20, max_height));

	image_width = gdk_pixbuf_get_width(pi->image);
	image_height = gdk_pixbuf_get_height(pi->image);
	
	if (req->height > 0 && max_height < req->height)
	{
		pi->style = TEXT_BESIDE_ICON;
		req->width += image_width;
		req->height = MAX(req->height, image_height);
		gtk_misc_set_alignment(GTK_MISC(pi->label), 1, 0.5);
	}
	else
	{
		pi->style = TEXT_UNDER_ICON;
		req->width = MAX(req->width, image_width);
		req->height += image_height;
		gtk_misc_set_alignment(GTK_MISC(pi->label), 0.5, 1);
	}
	
	if (horz)
		req->width += PANEL_ICON_SPACING;
	else
		req->height += PANEL_ICON_SPACING;
}

static gint expose_icon(GtkWidget *widget,
			GdkEventExpose *event,
			PanelIcon *pi)
{
	return draw_icon(widget, &event->area, pi);
}

static gint draw_icon(GtkWidget *widget, GdkRectangle *badarea, PanelIcon *pi)
{
	GdkRectangle	area;
	int		width, height;
	Icon		*icon = (Icon *) pi;
	int		image_x;
	int		image_y;
	GdkPixbuf	*image;
	int		text_height = 0;

	gdk_drawable_get_size(widget->window, &area.width, &area.height);

	if (panel_want_show_text(pi))
		text_height = pi->label->requisition.height;
	
	g_return_val_if_fail(pi->image != NULL, FALSE);

	image = pi->image;

	width = gdk_pixbuf_get_width(image);
	height = gdk_pixbuf_get_height(image);

	if (pi->style == TEXT_UNDER_ICON)
	{
		image_x = (area.width - width) >> 1;
		image_y = (area.height - height - text_height) >> 1;
	}
	else
	{
		image_x = PANEL_ICON_SPACING - 2;
		image_y = (area.height - height) >> 1;
	}
	
	gdk_pixbuf_render_to_drawable_alpha(
			image,
			widget->window,
			0, 0, 				/* src */
			image_x, image_y,		/* dest */
			width, height,
			GDK_PIXBUF_ALPHA_FULL, 128,	/* (unused) */
			GDK_RGB_DITHER_NORMAL, 0, 0);

	if (icon->item->flags & ITEM_FLAG_SYMLINK)
	{
		draw_emblem_on_icon(widget->window, widget->style,
				    ROX_STOCK_SYMLINK, &image_x, image_y+2);
	}
	if (icon->item->flags & ITEM_FLAG_MOUNT_POINT)
	{
		draw_emblem_on_icon(widget->window, widget->style,
				    icon->item->flags & ITEM_FLAG_MOUNTED
				    ? ROX_STOCK_MOUNTED
				    : ROX_STOCK_MOUNT,
				    &image_x, image_y+2);
	}
	return FALSE;
}

static void panel_icon_wink(Icon *icon)
{
	PanelIcon *pi = (PanelIcon *) icon;

	wink_widget(pi->widget);
}

/* icon may be NULL if the event is on the background */
static void perform_action(Panel *panel, PanelIcon *pi, GdkEventButton *event)
{
	BindAction	action;
	Icon		*icon = (Icon *) pi;
	
	action = bind_lookup_bev(icon ? BIND_PANEL_ICON : BIND_PANEL, event);

	if (pi && pi->socket)
		if (action != ACT_POPUP_MENU && action != ACT_MOVE_ICON)
			return;

	switch (action)
	{
		case ACT_OPEN_ITEM:
			dnd_motion_ungrab();
			wink_widget(pi->widget);
			icon_run(icon);
			break;
		case ACT_EDIT_ITEM:
			dnd_motion_ungrab();
			wink_widget(pi->widget);
			run_diritem(icon->path, icon->item, NULL, NULL, TRUE);
			break;
		case ACT_POPUP_MENU:
			dnd_motion_ungrab();
			panel_show_menu(event, pi, panel);
			break;
		case ACT_MOVE_ICON:
			dnd_motion_start(MOTION_REPOSITION);
			break;
		case ACT_PRIME_AND_SELECT:
			if (!icon->selected)
				icon_select_only(icon);
			dnd_motion_start(MOTION_READY_FOR_DND);
			break;
		case ACT_PRIME_AND_TOGGLE:
			icon_set_selected(icon, !icon->selected);
			dnd_motion_start(MOTION_READY_FOR_DND);
			break;
		case ACT_PRIME_FOR_DND:
			dnd_motion_start(MOTION_READY_FOR_DND);
			break;
		case ACT_TOGGLE_SELECTED:
			icon_set_selected(icon, !icon->selected);
			break;
		case ACT_SELECT_EXCL:
			icon_set_selected(icon, TRUE);
			break;
		case ACT_IGNORE:
			break;
		case ACT_CLEAR_SELECTION:
			dnd_motion_ungrab();
			icon_select_only(NULL);
			break;
		default:
			g_warning("Unsupported action : %d\n", action);
			break;
	}
}

static gint panel_button_release(GtkWidget *widget,
			      GdkEventButton *event,
			      Panel *panel)
{
	if (dnd_motion_release(event))
		return TRUE;

	perform_action(panel, NULL, event);
	
	return TRUE;
}

static gint panel_button_press(GtkWidget *widget,
			      GdkEventButton *event,
			      Panel *panel)
{
	if (dnd_motion_press(panel->window, event))
		perform_action(panel, NULL, event);

	return TRUE;
}

static gint icon_button_release(GtkWidget *widget,
				GdkEventButton *event,
				PanelIcon *pi)
{
	if (pi->socket && event->button == 1)
		return FALSE;	/* Restart button */

	if (dnd_motion_release(event))
		return TRUE;

	perform_action(pi->panel, pi, event);
	
	return TRUE;
}

static gint icon_button_press(GtkWidget *widget,
			      GdkEventButton *event,
			      PanelIcon *pi)
{
	if (pi->socket && event->button == 1)
		return FALSE;	/* Restart button */

	if (dnd_motion_press(widget, event))
		perform_action(pi->panel, pi, event);

	return TRUE;
}

/* Stop windows from maximising over all/part of us */
static void panel_setup_struts(Panel *panel, GtkAllocation *alloc)
{
	int thickness;
	struct {
		gulong left, right, top, bottom;
		gulong left_start_y, left_end_y;
		gulong right_start_y, right_end_y;
		gulong top_start_x, top_end_x;
		gulong bottom_start_x, bottom_end_x;
	} strut = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

	if (panel->avoid == FALSE)
		thickness = 2;
	else if (panel->side == PANEL_TOP ||
		 panel->side == PANEL_BOTTOM)
		thickness = alloc->height;
	else
		thickness = alloc->width;

	switch (panel->side)
	{
		case PANEL_LEFT:
			if (!panel->xinerama || !monitor_adjacent[panel->monitor].left)
			{
				strut.left = panel->geometry.x + thickness;
				strut.left_start_y = panel->geometry.y;
				strut.left_end_y = panel->geometry.y +
					panel->geometry.height - 1;
			}
			/* else there is (part of) a monitor
			 * to the left */
			else
			{
				thickness = 0;
			}
			break;
		case PANEL_RIGHT:
			if (!panel->xinerama || !monitor_adjacent[panel->monitor].right)
			{
				/* RHS of monitor might not abut edge
				 * of total virtual screen */
				strut.right = screen_width -
					panel->geometry.x -
					panel->geometry.width +
					thickness;
				strut.right_start_y = panel->geometry.y;
				strut.right_end_y = panel->geometry.y +
					panel->geometry.height - 1;
			}
			/* else there is (part of) a monitor
			 * to the right */
			else
			{
				thickness = 0;
			}
			break;
		case PANEL_TOP:
			if (!panel->xinerama || !monitor_adjacent[panel->monitor].top)
			{
				strut.top = panel->geometry.y + thickness;
				strut.top_start_x = panel->geometry.x;
				strut.top_end_x = panel->geometry.x +
					panel->geometry.width - 1;
			}
			/* else there is (part of) a monitor above */
			else
			{
				thickness = 0;
			}
			break;
		default:	/* PANEL_BOTTOM */
			if (!panel->xinerama ||
					!monitor_adjacent[panel->monitor].bottom)
			{
				/* Bottom of monitor might not abut
				 * edge of total virtual screen */
				strut.bottom = screen_height -
					panel->geometry.y -
					panel->geometry.height +
					thickness;
				strut.bottom_start_x = panel->geometry.x;
				strut.bottom_end_x = panel->geometry.x +
					panel->geometry.width - 1;
			}
			/* else there is (part of) a monitor below */
			else
			{
				thickness = 0;
			}
			break;
	}

	if (thickness)
	{
		/* Set full-width strut as well as partial in case
		 * partial isn't supported by wm */
		gdk_property_change(panel->window->window,
				gdk_atom_intern("_NET_WM_STRUT",
					FALSE),
				gdk_atom_intern("CARDINAL", FALSE),
				32, GDK_PROP_MODE_REPLACE,
				(gchar *) &strut, 4);
		gdk_property_change(panel->window->window,
				gdk_atom_intern("_NET_WM_STRUT_PARTIAL",
					FALSE),
				gdk_atom_intern("CARDINAL", FALSE),
				32, GDK_PROP_MODE_REPLACE,
				(gchar *) &strut, 12);
	}
	else
	{
		gdk_property_delete(panel->window->window,
				gdk_atom_intern("_NET_WM_STRUT_PARTIAL",
					FALSE));
		gdk_property_delete(panel->window->window,
				gdk_atom_intern("_NET_WM_STRUT",
					FALSE));
	}
}

static void reposition_panel(GtkWidget *window,
				GtkAllocation *alloc, Panel *panel)
{
	int		x = panel->geometry.x;
	int		y = panel->geometry.y;
	PanelSide	side = panel->side;

	if (side == PANEL_LEFT || side == PANEL_RIGHT)
	{
		if (side == PANEL_RIGHT)
			x += panel->geometry.width - alloc->width;

		if (current_panel[PANEL_TOP])
		{
			GtkWidget *win = current_panel[PANEL_TOP]->window;
			y += win->allocation.height;
		}
	}

	if (side == PANEL_BOTTOM)
		y += panel->geometry.height - alloc->height;
	
	gtk_window_move(GTK_WINDOW(panel->window), x, y);
	gdk_window_move(panel->window->window, x, y);

	if (side == PANEL_BOTTOM || side == PANEL_TOP)
	{
		if (current_panel[PANEL_RIGHT])
			gtk_widget_queue_resize(
					current_panel[PANEL_RIGHT]->window);
		if (current_panel[PANEL_LEFT])
			gtk_widget_queue_resize(
					current_panel[PANEL_LEFT]->window);
	}

	panel_setup_struts(panel, alloc);
}

/* Same as drag_set_dest(), but for panel icons */
static void drag_set_panel_dest(PanelIcon *pi)
{
	GtkWidget	*obj = pi->widget;

	make_drop_target(pi->widget, 0);

	g_signal_connect(obj, "drag_motion", G_CALLBACK(drag_motion), pi);
	g_signal_connect(obj, "drag_leave", G_CALLBACK(drag_leave), pi);
	g_signal_connect(obj, "drag_end", G_CALLBACK(drag_end), pi);
}

static gboolean drag_motion(GtkWidget		*widget,
			    GdkDragContext	*context,
			    gint		x,
			    gint		y,
			    guint		time,
			    PanelIcon		*pi)
{
	GdkDragAction	action = context->suggested_action;
	const char	*type = NULL;
	Icon		*icon = (Icon *) pi;
	DirItem		*item = icon->item;
	int		panel_x, panel_y;

	gdk_window_get_pointer(pi->panel->window->window,
				&panel_x, &panel_y, NULL);
	motion_may_raise(pi->panel, panel_x, panel_y);

	/* Should we scroll the panel when dragging? */
	if (motion_state != MOTION_REPOSITION)
		if (pi->panel->autoscroll_speed == 0)
			may_autoscroll(pi->panel);

	if (icon->selected)
		goto out;	/* Can't drag a selection to itself */

	type = dnd_motion_item(context, &item);

	if ((context->actions & GDK_ACTION_ASK) && o_dnd_left_menu.int_value
		&& type != drop_dest_prog)
	{
		guint state;
		gdk_window_get_pointer(NULL, NULL, NULL, &state);
		if (state & GDK_BUTTON1_MASK)
			action = GDK_ACTION_ASK;
	}

	if (!item)
		type = NULL;
out:
	/* We actually must pretend to accept the drop, even if the
	 * directory isn't writeable, so that the spring-opening
	 * thing works.
	 */

	/* Don't allow drops to non-writeable directories */
	if (o_dnd_spring_open.int_value == FALSE &&
			type == drop_dest_dir &&
			access(icon->path, W_OK) != 0)
	{
		type = NULL;
	}

	g_dataset_set_data(context, "drop_dest_type", (gpointer) type);
	if (type)
	{
		gdk_drag_status(context, action, time);
		g_dataset_set_data_full(context, "drop_dest_path",
				g_strdup(icon->path), g_free);
		if (type == drop_dest_dir)
			dnd_spring_load(context, NULL);

		if (dnd_highlight && dnd_highlight != pi->widget)
		{
			gtk_drag_unhighlight(dnd_highlight);
			dnd_highlight = NULL;
		}

		if (dnd_highlight == NULL)
		{
			gtk_drag_highlight(pi->widget);
			dnd_highlight = pi->widget;
		}
	}

	return type != NULL;
}


static void add_uri_list(GtkWidget          *widget,
			 GdkDragContext     *context,
			 gint               x,
			 gint               y,
			 GtkSelectionData   *selection_data,
			 guint              info,
			 guint32            time,
			 Panel		    *panel)
{
	gboolean after = FALSE;
	GList *uris, *next;
	
	if (!selection_data->data)
		return;

	g_return_if_fail(selection_data->data[selection_data->length] == '\0');

	if (g_object_get_data(G_OBJECT(widget), "after"))
		after = TRUE;

	uris = uri_list_to_glist(selection_data->data);

	for (next = uris; next; next = next->next)
	{
		guchar	*path;

		path = get_local_path((EscapedPath *) next->data);

		if (path) {
			panel_add_item(panel, path, NULL, after, NULL, NULL, FALSE);
			g_free(path);
		}
	}

	g_list_free(uris);
}

static void drag_end(GtkWidget *widget,
		     GdkDragContext *context,
		     Icon *icon)
{
	if (tmp_icon_selected)
	{
		icon_select_only(NULL);
		tmp_icon_selected = FALSE;
	}
}

static void drag_leave(GtkWidget	*widget,
		       GdkDragContext	*context,
		       guint32		time,
		       Icon	*icon)
{
	panel_drag_leave(widget, context, time, ((PanelIcon *) icon)->panel);

	if (dnd_highlight && dnd_highlight == widget)
	{
		gtk_drag_unhighlight(dnd_highlight);
		dnd_highlight = NULL;
	}

	dnd_spring_abort();
}

/* Create XML icon nodes for these widgets.
 * Always frees the widgets list.
 */
static void make_widgets(xmlNodePtr side, GList *widgets)
{
	GList	*next;

	for (next = widgets; next; next = next->next)
	{
		Icon	*icon;
		xmlNodePtr tree;

		icon = g_object_get_data(G_OBJECT(next->data), "icon");

		if (!icon)
		{
			g_warning("Can't find Icon from widget\n");
			continue;
		}

		tree = xmlNewTextChild(side, NULL, "icon", icon->src_path);

		xmlSetProp(tree, "label", icon->item->leafname);
		if (icon->shortcut)
			xmlSetProp(tree, "shortcut", icon->shortcut);
		if (icon->args)
			xmlSetProp(tree, "args", icon->args);
		if (icon->locked)
			xmlSetProp(tree, "locked", "true");
	}
	
	if (widgets)
		g_list_free(widgets);
}

void panel_save(Panel *panel)
{
	xmlDocPtr doc;
	xmlNodePtr root;
	xmlNodePtr options;
	guchar	*save = NULL;
	guchar	*save_new = NULL;

	g_return_if_fail(panel != NULL);

	if (strchr(panel->name, '/'))
		save = g_strdup(panel->name);
	else
	{
		guchar	*leaf;

		leaf = g_strconcat("pan_", panel->name, NULL);
		save = choices_find_xdg_path_save(leaf, PROJECT, SITE, TRUE);
		g_free(leaf);
	}

	if (!save)
		return;

	doc = xmlNewDoc("1.0");
	xmlDocSetRootElement(doc, xmlNewDocNode(doc, NULL, "panel", NULL));

	root = xmlDocGetRootElement(doc);

	xmlSetProp(root, "side", panel_side_to_name(panel->side));

	options = xmlNewChild(root, NULL, "options", NULL);
	set_int_prop(options, "style", panel->style);
	set_int_prop(options, "width", panel->width);
	set_int_prop(options, "avoid", panel->avoid);
	set_int_prop(options, "xinerama", panel->xinerama);
	set_int_prop(options, "monitor", panel->monitor);
	
	make_widgets(xmlNewChild(root, NULL, "start", NULL),
		gtk_container_get_children(GTK_CONTAINER(panel->before)));

	make_widgets(xmlNewChild(root, NULL, "end", NULL),
			g_list_reverse(gtk_container_get_children(
					GTK_CONTAINER(panel->after))));

	save_new = g_strconcat(save, ".new", NULL);
	if (save_xml_file(doc, save_new) || rename(save_new, save))
		delayed_error(_("Error saving panel %s: %s"),
				save, g_strerror(errno));
	g_free(save_new);

	g_free(save);
	if (doc)
		xmlFreeDoc(doc);
}

/* Create a frame widget which can be used to add icons to the panel */
static GtkWidget *make_insert_frame(Panel *panel)
{
	GtkWidget *frame;
	GtkTargetEntry 	target_table[] = {
		{"text/uri-list", 0, TARGET_URI_LIST},
	};

	frame = gtk_frame_new(NULL);
	gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_NONE);
	gtk_widget_set_size_request(frame, 16, 16);

	g_signal_connect(frame, "drag-motion",
			G_CALLBACK(insert_drag_motion), panel);
	g_signal_connect(frame, "drag-leave",
			G_CALLBACK(panel_drag_leave), panel);

	g_signal_connect(frame, "drag-data-received",
			G_CALLBACK(add_uri_list), panel);
	gtk_drag_dest_set(frame,
			GTK_DEST_DEFAULT_ALL,
			target_table,
			sizeof(target_table) / sizeof(*target_table),
			GDK_ACTION_COPY);

	return frame;
}

static gboolean enter_icon(GtkWidget *widget,
			   GdkEventCrossing *event,
			   Icon *icon)
{
	icon_may_update(icon);
	panel_icon_set_tip((PanelIcon *) icon);

	return FALSE;
}

static gint panel_leave_event(GtkWidget *widget,
			      GdkEventCrossing *event,
			      Panel *panel)
{
	GdkWindow *pinboard;

	if (event->mode != GDK_CROSSING_NORMAL)
		return FALSE;	/* Grab for menu, DnD, etc */

	if (event->x >= 0 &&
	    event->y >= 0 &&
	    event->x < widget->allocation.width &&
	    event->y < widget->allocation.height)
	{
		/* Still inside the panel's area.
		 * Probably we've moved over a panel applet. */
		return FALSE;
	}

	if (panel_keep_below(panel, TRUE))
	{
		/* Shouldn't need this as well as keep_below but some WMs don't
		 * automatically lower as soon as the hint is set */ 
		pinboard = pinboard_get_window();
		window_put_just_above(panel->window->window, pinboard);
	}

	return FALSE;
}

/* If (x, y) is at the edge of the panel then raise */
static void motion_may_raise(Panel *panel, int x, int y)
{
	gboolean raise;

	if (panel->side == PANEL_TOP)
		raise = y == 0;
	else if (panel->side == PANEL_BOTTOM)
		raise = y == panel->window->allocation.height - 1;
	else if (panel->side == PANEL_LEFT)
		raise = x == 0;
	else
		raise = x == panel->window->allocation.width - 1;

	if (raise && panel_keep_below(panel, FALSE))
	{
		/* Shouldn't need this as well as keep_below but some WMs don't
		 * automatically raise as soon as the hint is set */ 
		gdk_window_raise(panel->window->window);
	}
}

static gboolean may_autoscroll(Panel *panel)
{
	gboolean horz = panel->side == PANEL_TOP || panel->side == PANEL_BOTTOM;
	gint max, panel_x, panel_y, delta, new;

	if (panel->adj->upper <= panel->adj->page_size)
		goto stop_scrolling;	/* Can see everything already */

	gdk_window_get_pointer(panel->window->window, &panel_x, &panel_y, NULL);

	if (horz)
	{
		delta = panel_x;
		max   = panel->window->allocation.width;
		if (panel_y < 0 || panel_y > panel->window->allocation.height)
			goto stop_scrolling;	/* Not over the panel */
	}
	else
	{
		delta = panel_y;
		max   = panel->window->allocation.height;
		if (panel_x < 0 || panel_x > panel->window->allocation.width)
			goto stop_scrolling;	/* Not over the panel */
	}

	if (delta >= 20 && delta <= max - 20)
		goto stop_scrolling;	/* Not at either end */

	panel->autoscroll_speed = MIN(panel->autoscroll_speed + 2, 200);

	new = panel->adj->value - ((delta < 20) ? panel->autoscroll_speed
						: -panel->autoscroll_speed);
	new = CLAMP(new, 0, panel->adj->upper - panel->adj->page_size);
	gtk_adjustment_set_value(panel->adj, new);

	panel->autoscroll_to = g_timeout_add(40,
			(GSourceFunc) may_autoscroll, panel);

	return FALSE;

stop_scrolling:
	panel->autoscroll_speed = 0;
	return FALSE;
}

static gint panel_motion_event(GtkWidget *widget,
			      GdkEventMotion *event,
			      Panel *panel)
{
	motion_may_raise(panel, event->x, event->y);

	if (motion_state != MOTION_REPOSITION)
		if (panel->autoscroll_speed == 0)
			may_autoscroll(panel);

	return FALSE;
}

static gint icon_motion_event(GtkWidget *widget,
			      GdkEventMotion *event,
			      PanelIcon *pi)
{
	Panel	*panel = pi->panel;
	GList	*list, *me;
	gboolean horz = panel->side == PANEL_TOP || panel->side == PANEL_BOTTOM;
	int	val;
	int	dir = 0;

	if (motion_state == MOTION_READY_FOR_DND)
	{
		if (dnd_motion_moved(event))
			start_drag(pi, event);
		return TRUE;
	}
	else if (motion_state != MOTION_REPOSITION)
		return FALSE;

	list = gtk_container_get_children(GTK_CONTAINER(panel->before));
	list = g_list_append(list, NULL);	/* The gap in the middle */
	list = g_list_concat(list,
		gtk_container_get_children(GTK_CONTAINER(panel->after)));
	me = g_list_find(list, widget);

	g_return_val_if_fail(me != NULL, TRUE);

	val = horz ? event->x_root : event->y_root;

	if (me->prev)
	{
		GtkWidget *prev;
		int	  x, y;

		if (me->prev->data)
			prev = GTK_WIDGET(me->prev->data);
		else
			prev = panel->gap;

		gdk_window_get_origin(prev->window, &x, &y);

		if (val <= (horz ? x : y))
			dir = -1;
	}

	if (dir == 0 && me->next)
	{
		GtkWidget *next;
		int	  x, y, w, h;

		if (me->next->data)
			next = GTK_WIDGET(me->next->data);
		else
			next = panel->gap;

		gdk_window_get_origin(next->window, &x, &y);

		gdk_drawable_get_size(next->window, &w, &h);

		x += w;
		y += h;

		if (val >= (horz ? x : y)-1)
		{
			if (next == panel->gap)
				dir = +2;
			else
				dir = +1;
		}
	}

	if (dir)
		reposition_icon(pi, g_list_index(list, widget) + dir);

	return TRUE;
}

static void reposition_icon_on_side(GtkWidget *side, GtkWidget *widget,
				    int index)
{
	GList *list;

	list = gtk_container_get_children(GTK_CONTAINER(side));

	/* Want to move icon to the list in the given 'side'. Is it there
	 * already?
	 */

	if (!g_list_find(list, widget))
	{
		/* No, reparent */
		gtk_grab_remove(widget);
		gtk_widget_reparent(widget, side);
		dnd_motion_grab_pointer();
		gtk_grab_add(widget);
	}

	gtk_box_reorder_child(GTK_BOX(side), widget, index);

	g_list_free(list);
}

/* Move icon to this index in the complete widget list.
 * 0 makes the icon the left-most icon. The gap in the middle has
 * an index number, which allows you to specify that the icon should
 * go on the left or right side.
 */
static void reposition_icon(PanelIcon *pi, int index)
{
	Panel	  *panel = pi->panel;
	GtkWidget *widget = pi->widget;
	GList	  *list;
	int	  before_len;

	list = gtk_container_get_children(GTK_CONTAINER(panel->before));
	before_len = g_list_length(list);
	g_list_free(list);

	if (index <= before_len)
		reposition_icon_on_side(panel->before, widget, index);
	else
		reposition_icon_on_side(panel->after, widget,
					index - (before_len + 1));

	panel_save(panel);
}

static void start_drag(PanelIcon *pi, GdkEventMotion *event)
{
	GtkWidget *widget = pi->widget;
	Icon	  *icon = (Icon *) pi;

	if (!icon->selected)
	{
		if (event->state & GDK_BUTTON1_MASK)
		{
			/* Select just this one */
			icon_select_only(icon);
			tmp_icon_selected = TRUE;
		}
		else
			icon_set_selected(icon, TRUE);
	}
	
	g_return_if_fail(icon_selection != NULL);

	if (icon_selection->next == NULL)
		drag_one_item(widget, event, icon->path, icon->item, NULL);
	else
	{
		guchar	*uri_list;

		uri_list = icon_create_uri_list();
		drag_selection(widget, event, uri_list);
		g_free(uri_list);
	}
}

static void applet_died(GtkWidget *socket)
{
	gboolean never_plugged;

	never_plugged = (!g_object_get_data(G_OBJECT(socket), "lost_plug"))
		      && !GTK_SOCKET(socket)->plug_window;

	if (never_plugged)
	{
		report_error(
			_("Applet quit without ever creating a widget!"));
		gtk_widget_destroy(socket);
	}

	gtk_widget_unref(socket);
}

static void socket_destroyed(GtkWidget *socket, GtkWidget *widget)
{
	g_object_set_data(G_OBJECT(socket), "lost_plug", "yes");

	gtk_widget_unref(socket);

	gtk_widget_destroy(widget);	/* Remove from panel */

	if (!closing_panel)
		panel_save(g_object_get_data(G_OBJECT(socket), "panel"));
}

/* Try to run this applet.
 * Cases:
 * 
 * - No executable AppletRun:
 * 	icon->socket == NULL (unchanged) on return.
 *
 * Otherwise, create socket (setting icon->socket) and ref it twice.
 * 
 * - AppletRun quits without connecting a plug:
 * 	On child death lost_plug is unset and socket is empty.
 * 	Unref socket.
 * 	Report error and destroy widget (to 'socket destroyed').
 *
 * - AppletRun quits while plug is in socket:
 * 	Unref socket once. Socket will be destroyed later.
 *
 * - Socket is destroyed.
 * 	Set lost_plug = "yes" and remove widget from panel.
 * 	Unref socket.
 */
static void run_applet(PanelIcon *pi)
{
	GError	*error = NULL;
	char	*argv[3];
	gint	pid;
	Icon	*icon = (Icon *) pi;

	argv[0] = (char *) make_path(icon->path, "AppletRun");
	
	if (access(argv[0], X_OK) != 0)
		return;

	pi->socket = gtk_socket_new();
	
	gtk_container_add(GTK_CONTAINER(pi->widget), pi->socket);
	gtk_widget_show_all(pi->socket);
	gtk_widget_realize(pi->socket);

	/* Always get button-2 events so we can drag */
	XGrabButton(gdk_display, Button2, AnyModifier,
			GDK_WINDOW_XWINDOW(pi->socket->window),
			False,
			ButtonPressMask | ButtonReleaseMask | Button2MotionMask,
			GrabModeAsync,	/* Pointer */
			GrabModeAsync,	/* Keyboard */
			None, None);

	{
		gchar		*pos;
		PanelSide	side = pi->panel->side;

		/* Set a hint to let applets position their menus correctly */
		pos = g_strdup_printf("%s,%d",
				panel_side_to_name(side), MENU_MARGIN(side));
		gdk_property_change(pi->socket->window,
				gdk_atom_intern("_ROX_PANEL_MENU_POS", FALSE),
				gdk_atom_intern("STRING", FALSE),
				8, GDK_PROP_MODE_REPLACE,
				pos, strlen(pos));
		g_free(pos);

		/* Ensure that the properties are set before starting the
		 * applet.
		 */
		gdk_flush();
	}

	g_object_set_data(G_OBJECT(pi->widget), "icon", pi);
	g_object_set_data(G_OBJECT(pi->socket), "panel", pi->panel);

	argv[1] = g_strdup_printf("%ld",
			GDK_WINDOW_XWINDOW(pi->socket->window));
	argv[2] = NULL;

	if (!g_spawn_async(NULL, argv, NULL, G_SPAWN_DO_NOT_REAP_CHILD,
			NULL, NULL, &pid, &error))
	{
		delayed_error(_("Error running applet:\n%s"), error->message);
		g_error_free(error);
		gtk_widget_destroy(pi->socket);
		pi->socket = NULL;
	}
	else
	{
		gtk_widget_ref(pi->socket);
		on_child_death(pid, (CallbackFn) applet_died, pi->socket);

		gtk_widget_ref(pi->socket);
		g_signal_connect(pi->socket, "destroy",
				G_CALLBACK(socket_destroyed), pi->widget);
	}	
	
	g_free(argv[1]);
}

static void panel_post_resize(GtkWidget *win, GtkRequisition *req, Panel *panel)
{
	if (panel->side == PANEL_TOP || panel->side == PANEL_BOTTOM)
	{
		req->width = panel->geometry.width;
		req->height += EDGE_WIDTH;
	}
	else
	{
		int h = panel->geometry.height;

		if (current_panel[PANEL_TOP])
		{
			GtkWidget *win = current_panel[PANEL_TOP]->window;
			h -= win->allocation.height;
		}

		if (current_panel[PANEL_BOTTOM])
		{
			GtkWidget *win = current_panel[PANEL_BOTTOM]->window;
			h -= win->allocation.height;
		}

		req->height = h;
		req->width += EDGE_WIDTH;
	}
}

static void update_side(GtkWidget *side)
{
	GList	*kids, *next;

	kids = gtk_container_get_children(GTK_CONTAINER(side));
	for (next = kids; next; next = next->next)
	{
		PanelIcon *pi;
		pi = g_object_get_data(next->data, "icon");
		panel_icon_set_tip(pi);
	}
	g_list_free(kids);
}

/* Tips or style has changed -- update everything on this panel */
static void panel_set_style(Panel *panel)
{
	update_side(panel->before);
	update_side(panel->after);
	gtk_widget_queue_resize(panel->window);
}

static gboolean recreate_panels(char **names)
{
	int i;

	for (i = 0; i < PANEL_NUMBER_OF_SIDES; i++)
	{
		if (names[i])
		{
			panel_new(names[i], i);
			g_free(names[i]);
		}
	}

	g_free(names);
	
	return FALSE;
}

static void update_side_size(GtkWidget *side)
{
	GList	*kids, *next;

	kids = gtk_container_get_children(GTK_CONTAINER(side));
	for (next = kids; next; next = next->next)
	{
		PanelIcon *pi;
		pi = g_object_get_data(next->data, "icon");
		gtk_widget_queue_resize(pi->widget);
	}
	g_list_free(kids);
}

/* Update panel size and redraw */
static void panel_update(Panel *panel)
{
	update_side_size(panel->before);
	update_side_size(panel->after);
	gtk_widget_queue_resize(panel->window);
	gtk_widget_queue_draw(panel->window);
}

static void panel_style_changed(void)
{
	int i;

	if (o_override_redirect.has_changed)
	{
		gchar **names;
		
		names = g_new(char *, PANEL_NUMBER_OF_SIDES);

		for (i = 0; i < PANEL_NUMBER_OF_SIDES; i++)
		{
			Panel *panel = current_panel[i];
			names[i] = panel ? g_strdup(panel->name) : NULL;
			panel_new(NULL, i);
		}

		g_idle_add((GtkFunction) recreate_panels, names);
	}
}

static gboolean draw_panel_edge(GtkWidget *widget, GdkEventExpose *event,
				Panel *panel)
{
	int	x, y, width, height;

	if (panel->side == PANEL_TOP || panel->side == PANEL_BOTTOM)
	{
		width = panel->geometry.width;
		height = EDGE_WIDTH;

		x = 0;
		if (panel->side == PANEL_BOTTOM)
			y = 0;
		else
			y = widget->allocation.height - EDGE_WIDTH;
	}
	else
	{
		width = EDGE_WIDTH;
		height = panel->geometry.height;

		y = 0;
		if (panel->side == PANEL_RIGHT)
			x = 0;
		else
			x = widget->allocation.width - EDGE_WIDTH;
	}
	
	gdk_draw_rectangle(widget->window,
			widget->style->fg_gc[GTK_STATE_NORMAL], TRUE,
			x, y, width, height);

	return FALSE;
}

static gpointer parent_class;

static void panel_icon_destroy(Icon *icon)
{
	PanelIcon *pi = (PanelIcon *) icon;
	
	g_return_if_fail(pi != NULL);
	
	if (pi->image)
		g_object_unref(pi->image);

	g_return_if_fail(pi->widget != NULL);

	gtk_widget_destroy(pi->widget);
}

static void panel_remove_items(void)
{
	Panel *panel;

	g_return_if_fail(icon_selection != NULL);

	panel = ((PanelIcon *) icon_selection->data)->panel;

	while (icon_selection)
		icon_destroy((Icon *) icon_selection->data);

	panel_save(panel);
}

/* Icon's size, shape or appearance has changed - update the display */
static void panel_icon_redraw(Icon *icon)
{
	PanelIcon *pi = (PanelIcon *) icon;

	gtk_widget_set_state(pi->widget,
			icon->selected ? GTK_STATE_SELECTED
				       : GTK_STATE_NORMAL);

	/* Will regenerate the scaled icon from the new image */
	gtk_widget_queue_resize(pi->widget);

	panel_icon_set_tip((PanelIcon *) icon);
}

static void panel_icon_update(Icon *icon)
{
	PanelIcon	*pi = (PanelIcon *) icon;

	gtk_widget_queue_draw(pi->widget);
	gtk_label_set_text(GTK_LABEL(pi->label), icon->item->leafname);
	panel_save(pi->panel);
}

/* The point of this is to clear the selection if the existing icons
 * aren't from the same panel...
 */
static gboolean panel_icon_same_group(Icon *icon, Icon *other)
{
	if (IS_PANEL_ICON(other))
	{
		PanelIcon *a = (PanelIcon *) icon;
		PanelIcon *b = (PanelIcon *) other;

		return a->panel == b->panel;
	}
	else
		return FALSE;
}

static void panel_icon_class_init(gpointer gclass, gpointer data)
{
	IconClass *icon = (IconClass *) gclass;

	parent_class = g_type_class_peek_parent(gclass);

	icon->destroy = panel_icon_destroy;
	icon->redraw = panel_icon_redraw;
	icon->update = panel_icon_update;
	icon->remove_items = panel_remove_items;
	icon->same_group = panel_icon_same_group;
	icon->wink = panel_icon_wink;
}

static void panel_icon_init(GTypeInstance *object, gpointer gclass)
{
	PanelIcon *pi = (PanelIcon *) object;

	pi->widget = NULL;
	pi->image = NULL;
	pi->label = NULL;
	pi->socket = NULL;
	pi->style = TEXT_UNDER_ICON;
}

static GType panel_icon_get_type(void)
{
	static GType type = 0;

	if (!type)
	{
		static const GTypeInfo info =
		{
			sizeof (PanelIconClass),
			NULL,			/* base_init */
			NULL,			/* base_finalise */
			panel_icon_class_init,
			NULL,			/* class_finalise */
			NULL,			/* class_data */
			sizeof(PanelIcon),
			0,			/* n_preallocs */
			panel_icon_init
		};

		type = g_type_register_static(icon_get_type(),
						"PanelIcon", &info, 0);
	}

	return type;
}

static PanelIcon *panel_icon_new(Panel *panel,
				 const char *pathname,
				 const char *name)
{
	PanelIcon *pi;
	Icon	  *icon;

	pi = g_object_new(panel_icon_get_type(), NULL);
	icon = (Icon *) pi;

	icon_set_path(icon, pathname, name);
	pi->panel = panel;

	return pi;
}

static gboolean panel_want_show_text(PanelIcon *pi)
{
	Icon *icon = (Icon *) pi;
	Panel *panel = pi->panel;

	if (!icon->item->leafname[0])
		return FALSE;

	if (panel->style == SHOW_BOTH)
		return TRUE;
	if (panel->style == SHOW_ICON)
		return FALSE;

	if (icon->item->flags & ITEM_FLAG_APPDIR)
		return FALSE;

	if (EXECUTABLE_FILE(icon->item))
		return FALSE;

	return TRUE;
}

static void xinerama_sensitive(GtkBuilder *builder, gboolean sensitive)
{
	gtk_widget_set_sensitive(
			GTK_WIDGET(gtk_builder_get_object(builder, "panel_xinerama_monitor")),
			sensitive);
}

inline static Panel *panel_from_opts_widget(GtkWidget *widget)
{
	return g_object_get_data(G_OBJECT(gtk_widget_get_toplevel(widget)),
			"rox-panel");
}

static void panel_style_radio_toggled(GtkToggleButton *widget, int style)
{
	Panel *panel;

	if (!gtk_toggle_button_get_active(widget))
		return;
	panel = panel_from_opts_widget(GTK_WIDGET(widget));
	if (style != panel->style)
	{
		panel->style = style;
		panel_set_style(panel);
		panel_save(panel);
	}
}

static void panel_xinerama_changed(Panel *panel)
{
	panel_update_geometry(panel);
	reposition_panel(panel->window, &panel->window->allocation, panel);
	gtk_widget_queue_resize(panel->window);
	panel_save(panel);
}

static void panel_side_radio_toggled(GtkWidget *widget, PanelSide new_side)
{
	Panel *panel;
	PanelSide old_side;
	char *name, *other_side_name;
	
	if (!gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)))
		return;

	panel = panel_from_opts_widget(widget);
	old_side = panel->side;
	if (new_side == old_side)
		return;

	name = g_strdup(panel->name);
	other_side_name = current_panel[new_side]
			? g_strdup(current_panel[new_side]->name)
			: NULL;

	panel_new(name, new_side);
	g_object_set_data(G_OBJECT(gtk_widget_get_toplevel(widget)),
			"rox-panel", current_panel[new_side]);
	panel_new(other_side_name, old_side);

	g_free(name);
	g_free(other_side_name);
}

static void panel_style_radio_0_toggled_cb(GtkToggleButton *widget)
{
	panel_style_radio_toggled(widget, 0);
}

static void panel_style_radio_1_toggled_cb(GtkToggleButton *widget)
{
	panel_style_radio_toggled(widget, 1);
}

static void panel_style_radio_2_toggled_cb(GtkToggleButton *widget)
{
	panel_style_radio_toggled(widget, 2);
}

static void panel_width_changed_cb(GtkSpinButton *widget)
{
	Panel *panel = panel_from_opts_widget(GTK_WIDGET(widget));
	int width = gtk_spin_button_get_value_as_int(widget);

	if (width != panel->width)
	{
		panel->width = width;
		panel_update(panel);
		panel_save(panel);
	}
}

static void panel_avoid_toggled_cb(GtkToggleButton *widget)
{
	Panel *panel = panel_from_opts_widget(GTK_WIDGET(widget));
	gboolean avoid = gtk_toggle_button_get_active(widget);

	if (avoid != panel->avoid)
	{
		panel->avoid = avoid;
		panel_setup_struts(panel, &panel->window->allocation);
		panel_save(panel);
	}
}

static void panel_xinerama_confine_toggled_cb(GtkWidget *widget, GtkWidget *spinner)
{
	Panel *panel = panel_from_opts_widget(widget);
	gboolean xinerama = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget));

	gtk_widget_set_sensitive(spinner, xinerama);

	if (xinerama != panel->xinerama)
	{
		panel->xinerama = xinerama;
		panel_xinerama_changed(panel);
	}
}

static void panel_xinerama_monitor_changed_cb(GtkSpinButton *widget)
{
	Panel *panel = panel_from_opts_widget(GTK_WIDGET(widget));
	int monitor = gtk_spin_button_get_value_as_int(widget);

	if (monitor != panel->monitor)
	{
		panel->monitor = monitor;
		panel_xinerama_changed(panel);
	}
}

static void panel_pos_top_toggled_cb(GtkWidget *widget)
{
	panel_side_radio_toggled(widget, PANEL_TOP);
}

static void panel_pos_bottom_toggled_cb(GtkWidget *widget)
{
	panel_side_radio_toggled(widget, PANEL_BOTTOM);
}

static void panel_pos_left_toggled_cb(GtkWidget *widget)
{
	panel_side_radio_toggled(widget, PANEL_LEFT);
}

static void panel_pos_right_toggled_cb(GtkWidget *widget)
{
	panel_side_radio_toggled(widget, PANEL_RIGHT);
}

static void panel_connect_dialog_signal_handlers(GtkBuilder *builder,
						 GObject *object,
						 const gchar *signal_name,
						 const gchar *handler_name,
						 GObject *connect_object,
						 GConnectFlags flags,
						 gpointer user_data)
{
	void *fn = NULL;

	if (strcmp(handler_name, "gtk_widget_destroy") == 0)
		fn = gtk_widget_destroy;
	else if (strcmp(handler_name, "panel_style_radio_0_toggled_cb") == 0)
		fn = panel_style_radio_0_toggled_cb;
	else if (strcmp(handler_name, "panel_style_radio_0_toggled_cb") == 0)
		fn = panel_style_radio_0_toggled_cb;
	else if (strcmp(handler_name, "panel_style_radio_1_toggled_cb") == 0)
		fn = panel_style_radio_1_toggled_cb;
	else if (strcmp(handler_name, "panel_style_radio_2_toggled_cb") == 0)
		fn = panel_style_radio_2_toggled_cb;
	else if (strcmp(handler_name, "panel_width_changed_cb") == 0)
		fn = panel_width_changed_cb;
	else if (strcmp(handler_name, "panel_avoid_toggled_cb") == 0)
		fn = panel_avoid_toggled_cb;
	else if (strcmp(handler_name, "panel_xinerama_confine_toggled_cb") == 0)
		fn = panel_xinerama_confine_toggled_cb;
	else if (strcmp(handler_name, "panel_xinerama_monitor_changed_cb") == 0)
		fn = panel_xinerama_monitor_changed_cb;
	else if (strcmp(handler_name, "panel_pos_top_toggled_cb") == 0)
		fn = panel_pos_top_toggled_cb;
	else if (strcmp(handler_name, "panel_pos_bottom_toggled_cb") == 0)
		fn = panel_pos_bottom_toggled_cb;
	else if (strcmp(handler_name, "panel_pos_left_toggled_cb") == 0)
		fn = panel_pos_left_toggled_cb;
	else if (strcmp(handler_name, "panel_pos_right_toggled_cb") == 0)
		fn = panel_pos_right_toggled_cb;

	if (fn != NULL)
	{
		g_signal_connect(object, signal_name, fn, connect_object);
	}
	else
	{
		g_warning("Unknown handler '%s'", handler_name);
	}
}

static void panel_setup_options_dialog(GtkBuilder *builder, Panel *panel)
{
	char *wnm;
	const char *pos_radio;

	wnm = g_strdup_printf("panel_style_radio_%d", panel->style);
	gtk_toggle_button_set_active(
			GTK_TOGGLE_BUTTON(gtk_builder_get_object(builder, wnm)),
			TRUE);
	g_free(wnm);
	gtk_spin_button_set_value(
		GTK_SPIN_BUTTON(gtk_builder_get_object(builder, "panel_width")),
		panel->width);
	gtk_toggle_button_set_active(
		GTK_TOGGLE_BUTTON(gtk_builder_get_object(builder, "panel_avoid")),
		panel->avoid);
	gtk_toggle_button_set_active(
		GTK_TOGGLE_BUTTON(gtk_builder_get_object(builder,
				"panel_xinerama_confine")),
		panel->xinerama);
	gtk_spin_button_set_adjustment(
		GTK_SPIN_BUTTON(gtk_builder_get_object(builder, "panel_xinerama_monitor")),
		GTK_ADJUSTMENT(gtk_adjustment_new(MAX(0, panel->monitor),
				0, n_monitors - 1, 1, 10, 0)));
	xinerama_sensitive(builder, panel->xinerama);
	switch (panel->side)
	{
		case PANEL_TOP:
			pos_radio = "panel_pos_top";
			break;
		case PANEL_BOTTOM:
			pos_radio = "panel_pos_bottom";
			break;
		case PANEL_LEFT:
			pos_radio = "panel_pos_left";
			break;
		case PANEL_RIGHT:
			pos_radio = "panel_pos_right";
			break;
		default:
			pos_radio = NULL;
			break;
	}
	g_return_if_fail(pos_radio != NULL);
	gtk_toggle_button_set_active(
		GTK_TOGGLE_BUTTON(gtk_builder_get_object(builder, pos_radio)),
		TRUE);
}

static void panel_show_options(Panel *panel)
{
	GtkWidget *dialog;
	gboolean already_showing = FALSE;
	GtkBuilder *builder;
	gchar *ids[] = {"adjustment1", "adjustment2", "Panel Options", NULL};
	
	builder = get_gtk_builder(ids);

	if (panel_options_dialog)
	{
		dialog = panel_options_dialog;
		already_showing = TRUE;
	}
	else
	{
		dialog = GTK_WIDGET(gtk_builder_get_object(builder, "Panel Options"));
		panel_options_dialog = dialog;
		g_signal_connect(dialog, "destroy",
				G_CALLBACK(gtk_widget_destroyed),
				&panel_options_dialog);
		gtk_builder_connect_signals_full(builder, &panel_connect_dialog_signal_handlers, NULL);
	}
	g_object_set_data(G_OBJECT(panel_options_dialog), "rox-panel", panel);

	panel_setup_options_dialog(builder, panel);

	if (already_showing)
	{
		GtkWindow *win = GTK_WINDOW(dialog);

		gtk_widget_hide(dialog);
		/* This extra set_position() should ensure it moves to new position
		 * under pointer */
		gtk_window_set_position(win, GTK_WIN_POS_CENTER_ALWAYS);
		gtk_window_set_position(win, GTK_WIN_POS_MOUSE);
		gtk_window_present(win);
	}
	else
	{
		gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_MOUSE);
		gtk_widget_show_all(dialog);
	}

	g_object_unref(builder);
}

static void panel_position_menu(GtkMenu *menu, gint *x, gint *y,
				gboolean  *push_in, gpointer data)
{
	int		*pos = (int *) data;
	GtkRequisition 	requisition;
	int		margin = pos[2];
	int		mon = pos[3];
	int		mon_right = monitor_geom[mon].x +
				monitor_geom[mon].width;
	int		mon_bottom = monitor_geom[mon].y +
				monitor_geom[mon].height;

	gtk_widget_size_request(GTK_WIDGET(menu), &requisition);

	if (pos[0] == -1)
		*x = mon_right - margin - requisition.width;
	else if (pos[0] == -2)
		*x = monitor_geom[mon].x + margin;
	else
		*x = pos[0] - (requisition.width >> 2);
		
	if (pos[1] == -1)
		*y = mon_bottom - margin - requisition.height;
	else if (pos[1] == -2)
		*y = monitor_geom[mon].y + margin;
	else
		*y = pos[1] - (requisition.height >> 2);

	*x = CLAMP(*x, 0, mon_right - requisition.width);
	*y = CLAMP(*y, 0, mon_bottom - requisition.height);

	*push_in = FALSE;
}

static void panel_remove_callback(PanelSide side)
{
	GtkWidget *dialog = gtk_message_dialog_new(NULL, GTK_DIALOG_MODAL,
			GTK_MESSAGE_QUESTION, GTK_BUTTONS_OK_CANCEL,
			_("Are you sure you want to remove this panel from the desktop?"));

	gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_OK);
	gtk_window_set_title(GTK_WINDOW(dialog), _("Remove Panel"));
	gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_MOUSE);
	if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_OK)
		panel_new(NULL, side);
	gtk_widget_destroy(dialog);
}

static void panel_show_menu(GdkEventButton *event, PanelIcon *pi, Panel *panel)
{
	GtkWidget	*option_item;
	GtkWidget	*del_item;
	PanelSide	side = panel->side;
	int		pos[4];

	pos[0] = event->x_root;
	pos[1] = event->y_root;
	pos[2] = MENU_MARGIN(side);
	/* FIXME: Should we read screen from event's window rather than
	 * using default? */
	pos[3] = gdk_screen_get_monitor_at_point(
				gdk_screen_get_default(),
				event->x_root, event->y_root);

	option_item = gtk_image_menu_item_new_with_label(_("Panel Options..."));
	g_signal_connect_swapped(option_item, "activate",
			 G_CALLBACK(panel_show_options), panel);

	del_item = gtk_image_menu_item_new_with_label(_("Remove Panel"));
	add_stock_to_menu_item(del_item, GTK_STOCK_REMOVE);
	g_signal_connect_swapped(del_item, "activate",
			 G_CALLBACK(panel_remove_callback), GINT_TO_POINTER(side));

	icon_prepare_menu((Icon *) pi, option_item, del_item, NULL);

	if (side == PANEL_LEFT)
		pos[0] = -2;
	else if (side == PANEL_RIGHT)
		pos[0] = -1;

	if (side == PANEL_TOP)
		pos[1] = -2;
	else if (side == PANEL_BOTTOM)
		pos[1] = -1;

	gtk_menu_popup(GTK_MENU(icon_menu), NULL, NULL,
			panel_position_menu,
			(gpointer) pos, event->button, event->time);
}

/* Note: also called from icon handler */
static gboolean panel_drag_motion(GtkWidget	*widget,
			    GdkDragContext	*context,
			    gint		x,
			    gint		y,
			    guint		time,
			    Panel		*panel)
{
	int panel_x, panel_y;

	gdk_window_get_pointer(panel->window->window, &panel_x, &panel_y, NULL);

	motion_may_raise(panel, panel_x, panel_y);
	gdk_drag_status(context, 0, time);

	return TRUE;
}

static gboolean insert_drag_motion(GtkWidget	*widget,
			    GdkDragContext	*context,
			    gint		x,
			    gint		y,
			    guint		time,
			    Panel		*panel)
{
	int panel_x, panel_y;

	gdk_window_get_pointer(panel->window->window, &panel_x, &panel_y, NULL);
	motion_may_raise(panel, panel_x, panel_y);

	return FALSE;
}

/* Note: also called from icon handler */
static void panel_drag_leave(GtkWidget	*widget,
		       GdkDragContext	*context,
		       guint32		time,
		       Panel		*panel)
{
	GdkWindow *pinboard, *window;
	GtkAllocation *alloc = &panel->window->allocation;
	int x, y;
	
	window = panel->window->window;
	gdk_window_get_pointer(window, &x, &y, NULL);
	if ((x < 0 || y < 0 || x > alloc->width || y > alloc->height) &&
		panel_keep_below(panel, TRUE))
	{
		/* Shouldn't need this as well as keep_below but some WMs don't
		 * automatically lower as soon as the hint is set */ 
		pinboard = pinboard_get_window();
		window_put_just_above(panel->window->window, pinboard);
	}
}

static void panel_update_geometry(Panel *panel)
{
	if (panel->xinerama && panel->monitor >= n_monitors)
	{
		g_warning(_("Xinerama monitor %d unavailable"), panel->monitor); 
		panel->xinerama = FALSE;
	}

	if (panel->xinerama)
	{
		panel->geometry = monitor_geom[panel->monitor];
	}
	else if (o_panel_obey_workarea.int_value)
	{
                get_work_area(&panel->geometry.x, &panel->geometry.y,
                              &panel->geometry.width, &panel->geometry.height);
	}
        else
	{
		panel->geometry.x = panel->geometry.y = 0;
                panel->geometry.width = screen_width;
                panel->geometry.height = screen_height;
	}
}

static GList *build_monitor_number(Option *option, xmlNode *node, guchar *label)
{
	GtkObject *adj;
	
	adj = gtk_adjustment_new(MAX(0, panel_monitor),
				0, n_monitors - 1, 1, 10, 1);
	return build_numentry_base(option, node, label, GTK_ADJUSTMENT(adj));
}

static const char *panel_side_to_translated_name(PanelSide side)
{
	switch (side)
	{
		case PANEL_TOP:
			return _("Top");
		case PANEL_BOTTOM:
			return _("Bottom");
		case PANEL_LEFT:
			return _("Left");
		case PANEL_RIGHT:
			return _("Right");
		case PANEL_DEFAULT_SIDE:
			return _("Default");
		default:
			break;
	}
	return _("Unknown side");
}

const char *panel_side_to_name(PanelSide side)
{
	switch (side)
	{
		case PANEL_TOP:
			return "Top";
		case PANEL_BOTTOM:
			return "Bottom";
		case PANEL_LEFT:
			return "Left";
		case PANEL_RIGHT:
			return "Right";
		case PANEL_DEFAULT_SIDE:
			return "Default";
		default:
			break;
	}
	return "UnknownSide";
}

/* Returns PANEL_NUMBER_OF_SIDES if name is invalid */
PanelSide panel_name_to_side(gchar *side)
{
	if (strcmp(side, "Top") == 0)
		return PANEL_TOP;
	else if (strcmp(side, "Bottom") == 0)
		return PANEL_BOTTOM;
	else if (strcmp(side, "Left") == 0)
		return PANEL_LEFT;
	else if (strcmp(side, "Right") == 0)
		return PANEL_RIGHT;
	else
		g_warning("Unknown panel side '%s'", side);
	return PANEL_NUMBER_OF_SIDES;
}

static void panel_add_callback(PanelSide side)
{
	g_return_if_fail(current_panel[side] == NULL);
	panel_new(panel_side_to_name(side), side);
}

GtkWidget *panel_new_panel_submenu(void)
{
	GtkWidget *menu = gtk_menu_new();
	PanelSide side;

	for (side = 0; side < PANEL_NUMBER_OF_SIDES; ++side)
	{
		GtkWidget *item = gtk_menu_item_new_with_label(
				panel_side_to_translated_name(side));

		g_signal_connect_swapped(item, "activate",
				G_CALLBACK(panel_add_callback), GINT_TO_POINTER(side));
		gtk_widget_set_sensitive(item, current_panel[side] == NULL);
		gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
		gtk_widget_show(item);
	}
	return menu;
}

/* Set the panel to be above/below other windows (setting = TRUE for below),
 * and return TRUE.
 *
 * If o_panel_on_top is set then do nothing and return FALSE.
 */
static gboolean panel_keep_below(Panel *panel, gboolean setting)
{
	if (!o_panel_on_top.int_value)
	{
		keep_below(panel->window->window, setting);
		return TRUE;
	}
	return FALSE;
}

