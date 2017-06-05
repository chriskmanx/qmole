/*
 * Geeqie
 * (C) 2004 John Ellis
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * Author: Colin Clark
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */

#include "main.h"
#ifdef HAVE_LIBCHAMPLAIN
#ifdef HAVE_LIBCHAMPLAIN_GTK

#include "bar_gps.h"

#include "bar.h"
#include "filedata.h"
#include "layout.h"
#include "metadata.h"
#include "menu.h"
#include "rcfile.h"
#include "thumb.h"
#include "ui_menu.h"

#include <clutter-gtk/clutter-gtk.h>
#include <champlain/champlain.h>
#include <champlain-gtk/champlain-gtk.h>

#define MARKER_COLOUR 0x00, 0x00, 0xff, 0xff
#define TEXT_COLOUR 0x00, 0x00, 0x00, 0xff
#define THUMB_COLOUR 0xff, 0xff, 0xff, 0xff
#define THUMB_SIZE 100

/*
 *-------------------------------------------------------------------
 * GPS Map utils
 *-------------------------------------------------------------------
 */

typedef struct _PaneGPSData PaneGPSData;
struct _PaneGPSData
{
	PaneData pane;
	GtkWidget *widget;
	FileData *fd;
	gchar *map_source;
	gint height;
	ClutterActor *gps_view;
	ChamplainLayer *icon_layer;
	GList *selection_list;
	GPtrArray *marker_list;
	guint create_markers_id;
	GtkWidget *progress;
	GtkWidget *slider;
	GtkWidget *state;
	gint selection_count;
	gboolean centre_map_checked;
	gboolean enable_markers_checked;
};

static void bar_pane_gps_thumb_done_cb(ThumbLoader *tl, gpointer data)
{
	FileData *fd;
	ClutterActor *marker;
	ClutterActor *actor;

	marker = CLUTTER_ACTOR(data);
	fd = g_object_get_data(G_OBJECT(marker), "file_fd");
	if (fd->thumb_pixbuf != NULL)
		{
		actor = clutter_texture_new();
		gtk_clutter_texture_set_from_pixbuf(CLUTTER_TEXTURE(actor), fd->thumb_pixbuf, NULL);
		champlain_marker_set_image(CHAMPLAIN_MARKER(marker), actor);
		}
	thumb_loader_free(tl);
}

static void bar_pane_gps_thumb_error_cb(ThumbLoader *tl, gpointer data)
{
	thumb_loader_free(tl);
}

static gboolean bar_pane_gps_marker_keypress_cb(GtkWidget *widget, ClutterButtonEvent *bevent, gpointer data)
{
	//PaneGPSData *pgd = data;
	FileData *fd;
	ClutterActor *marker;
	ClutterColor marker_colour = { MARKER_COLOUR };
	ClutterColor text_colour = { TEXT_COLOUR };
	ClutterColor thumb_colour = { THUMB_COLOUR };
	gchar *current_text;
	ClutterActor *actor;
	ClutterActor *current_image;
	GString *text;
	gint height, width, rotate;
	gchar *altitude = NULL;
	ThumbLoader *tl;

	if (bevent->button == MOUSE_BUTTON_LEFT)
		{
		marker = CLUTTER_ACTOR(widget);
		fd = g_object_get_data(G_OBJECT(marker), "file_fd");

		/* If the marker is showing a thumbnail, delete it
		 */
		current_image = champlain_marker_get_image(CHAMPLAIN_MARKER(marker));
		if (current_image != NULL)
			{
			clutter_actor_destroy(CLUTTER_ACTOR(current_image));
		 	champlain_marker_set_image(CHAMPLAIN_MARKER(marker), NULL);
			}
			
		current_text = g_strdup(champlain_marker_get_text(CHAMPLAIN_MARKER(marker)));

		/* If the marker is showing only the text character, replace it with a
		 * thumbnail and date and altitude
		 */
		if (g_strcmp0(current_text, "i") == 0)
			{
			/* If a thumbail has already been generated, use that. If not try the pixbuf of the full image.
			 * If not, call the thumb_loader to generate a thumbnail and update the marker later in the
			 * thumb_loader callback
			 */
			 if (fd->thumb_pixbuf != NULL)
				{
				actor = clutter_texture_new();
				gtk_clutter_texture_set_from_pixbuf(CLUTTER_TEXTURE(actor), fd->thumb_pixbuf, NULL);
				champlain_marker_set_image(CHAMPLAIN_MARKER(marker), actor);
				}
			else if (fd->pixbuf != NULL)
				{
				actor = clutter_texture_new();
				width = gdk_pixbuf_get_width (fd->pixbuf);
				height = gdk_pixbuf_get_height (fd->pixbuf);
				switch (fd->exif_orientation)
					{
					case 8:
						rotate = GDK_PIXBUF_ROTATE_COUNTERCLOCKWISE;
						break;
					case 3:
						rotate = GDK_PIXBUF_ROTATE_UPSIDEDOWN;
						break;
					case 6:
						rotate = GDK_PIXBUF_ROTATE_CLOCKWISE;
						break;
					default:
						rotate = GDK_PIXBUF_ROTATE_NONE;
					}
										
					gtk_clutter_texture_set_from_pixbuf(CLUTTER_TEXTURE(actor),
										gdk_pixbuf_rotate_simple(gdk_pixbuf_scale_simple(fd->pixbuf, THUMB_SIZE, height * THUMB_SIZE / width,
										GDK_INTERP_NEAREST), rotate), NULL);
					champlain_marker_set_image(CHAMPLAIN_MARKER(marker), actor);
				}
			else
				{
				tl = thumb_loader_new(THUMB_SIZE, THUMB_SIZE);
				thumb_loader_set_callbacks(tl,
											bar_pane_gps_thumb_done_cb,
											bar_pane_gps_thumb_error_cb,
											NULL,
											marker);
				thumb_loader_start(tl, fd);
				}
				
			text = g_string_new(fd->name);
			g_string_append(text, "\n");
			g_string_append(text, text_from_time(fd->date));
			g_string_append(text, "\n");
			altitude = metadata_read_string(fd, "formatted.GPSAltitude", METADATA_FORMATTED);
			if (altitude != NULL)
				{
				g_string_append(text, altitude);
				}

			champlain_marker_set_text(CHAMPLAIN_MARKER(marker), text->str);
			champlain_marker_set_color(CHAMPLAIN_MARKER(marker), &thumb_colour);
			champlain_marker_set_text_color(CHAMPLAIN_MARKER(marker), &text_colour);
			champlain_marker_set_font_name(CHAMPLAIN_MARKER(marker), "sans 8");

			g_free(altitude);
			g_string_free(text, TRUE);
			}
		/* otherwise, revert to the hidden text marker
		 */
		else
			{
			champlain_marker_set_text(CHAMPLAIN_MARKER(marker), "i");
			champlain_marker_set_color(CHAMPLAIN_MARKER(marker), &marker_colour);
			champlain_marker_set_text_color(CHAMPLAIN_MARKER(marker), &marker_colour);
			champlain_marker_set_font_name(CHAMPLAIN_MARKER(marker), "courier 5");
			}

		g_free(current_text);
		
		return TRUE;
		}
	return TRUE;
}

static gboolean bar_pane_gps_create_markers_cb(gpointer data)
{
	PaneGPSData *pgd = data;
	gdouble latitude;
	gdouble longitude;
	GList *work;
	ClutterActor *marker;
	FileData *fd;
	ClutterColor marker_colour = { MARKER_COLOUR };
	GString *message;

	gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(pgd->progress),
							(gdouble)(pgd->selection_count - g_list_length(pgd->selection_list)) /
							(gdouble)pgd->selection_count);
							
	message = g_string_new("");
	g_string_printf(message, "%i/%i", (pgd->selection_count - g_list_length(pgd->selection_list)),
																			pgd->selection_count);
	gtk_progress_bar_set_text(GTK_PROGRESS_BAR(pgd->progress), message->str);
	g_string_free(message, TRUE);
	
	work = pgd->selection_list;
	while (work)
		{
		fd = work->data;
		pgd->selection_list = g_list_remove(pgd->selection_list, work->data);
		/* If the file has a parent, it must be a sidecar file. Do not process sidecar files
		*/
		if (fd != NULL && fd->parent == NULL)
			{
			latitude = metadata_read_GPS_coord(fd, "Xmp.exif.GPSLatitude", 1000);
			longitude = metadata_read_GPS_coord(fd, "Xmp.exif.GPSLongitude", 1000);

			if ((latitude != 1000) && (longitude != 1000))
				{
				marker = champlain_marker_new_with_text("i","courier 5", &marker_colour, &marker_colour);

				champlain_base_marker_set_position(CHAMPLAIN_BASE_MARKER(marker), latitude, longitude);
				clutter_container_add(CLUTTER_CONTAINER(pgd->icon_layer), marker, NULL);
				clutter_actor_set_reactive(marker, TRUE);

				g_signal_connect(G_OBJECT(marker), "button_release_event",
						 				G_CALLBACK(bar_pane_gps_marker_keypress_cb), pgd);

				g_object_set_data(G_OBJECT(marker), "file_fd", fd);

				g_ptr_array_add(pgd->marker_list, marker);
				if (pgd->centre_map_checked)
					{
					g_ptr_array_add(pgd->marker_list, NULL);
					champlain_view_ensure_markers_visible(CHAMPLAIN_VIEW(pgd->gps_view),
													(void *)pgd->marker_list->pdata, FALSE);
					g_ptr_array_remove(pgd->marker_list, NULL);
					}
				}
			}
		return TRUE;
		}
		
	if (pgd->marker_list->len >= 1)
		{
		g_ptr_array_add(pgd->marker_list, NULL);

		if (pgd->centre_map_checked)
			{
			champlain_view_ensure_markers_visible(CHAMPLAIN_VIEW(pgd->gps_view), (void *)pgd->marker_list->pdata, FALSE);
			}
		}

	gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(pgd->progress), 0);
	gtk_progress_bar_set_text(GTK_PROGRESS_BAR(pgd->progress), NULL);
	g_list_free(pgd->selection_list);
	g_ptr_array_free(pgd->marker_list, TRUE);
	pgd->create_markers_id = 0;

	return FALSE;
}

static void bar_pane_gps_update(PaneGPSData *pgd)
{
	GList *list;
	GList *work;

	/* The widget does not have a parent during bar_pane_gps_new, so calling gtk_widget_show_all there gives a
	 * "Gtk-CRITICAL **: gtk_widget_realize: assertion `GTK_WIDGET_ANCHORED (widget) || GTK_IS_INVISIBLE (widget)' failed"
	 * error. gtk_widget_show_all can be given after it has been added to the bar.
	 */
	if (gtk_widget_get_parent(pgd->widget) != NULL)
		gtk_widget_show_all(pgd->widget);

	/* If a create-marker background process is running, kill it
	 * and start again
	 */
	if (pgd->create_markers_id != 0)
		{
		if (g_idle_remove_by_data(pgd))
			{
			pgd->create_markers_id = 0;
			}
		else
			{
			return;
			}		
		}

	/* Delete any markers currently displayed
	 */
	work = clutter_container_get_children(CLUTTER_CONTAINER(pgd->icon_layer));
	while (work)
		{
		clutter_container_remove(CLUTTER_CONTAINER(pgd->icon_layer), work->data, NULL);
		work = work->next;
		}
	g_list_free(work);

	if (!pgd->enable_markers_checked)
		{
		return;
		}

	/* For each selected photo that has GPS data, create a marker containing
	 * a single, small text character the same colour as the marker background.
	 * Use a background process in case the user selects a large number of files.
	 */
	list = layout_selection_list(pgd->pane.lw);
	list = file_data_process_groups_in_selection(list, FALSE, NULL);

	if (list != NULL)
		{
		pgd->selection_list = g_list_copy(list);
		pgd->marker_list = g_ptr_array_new();
		pgd->selection_count = g_list_length(pgd->selection_list);
		pgd->create_markers_id = g_idle_add(bar_pane_gps_create_markers_cb, pgd);
		}

	g_list_free(list);
	g_list_free(work);
}

void bar_pane_gps_set_map_source(PaneGPSData *pgd, const gchar *map_id)
{
	ChamplainMapSource *map_source;
	ChamplainMapSourceFactory *map_factory;

	map_factory = champlain_map_source_factory_dup_default();
	map_source = champlain_map_source_factory_create(map_factory, map_id);

	if (map_source != NULL)
		{
		g_object_set(G_OBJECT(pgd->gps_view), "map-source", map_source, NULL);
		g_object_unref(map_factory);
		}

	g_object_unref(map_source);
}

void bar_pane_gps_enable_markers_checked_toggle_cb(GtkWidget *menu_widget, gpointer data)
{
	PaneGPSData *pgd = data;

	if (pgd->enable_markers_checked)
		{
		pgd->enable_markers_checked = FALSE;
		}
	else
		{
		pgd->enable_markers_checked = TRUE;
		}
}

static void bar_pane_gps_centre_map_checked_toggle_cb(GtkWidget *menu_widget, gpointer data)
{
	PaneGPSData *pgd = data;

	if (pgd->centre_map_checked)
		{
		pgd->centre_map_checked = FALSE;
		}
	else
		{
		pgd->centre_map_checked = TRUE;
		}
}

static void bar_pane_gps_change_map_cb(GtkWidget *widget, gpointer data)
{
	PaneGPSData *pgd = data;
	gchar *mapsource;

	if (!gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(widget)))
		return;

	if (!pgd) return;

	mapsource = g_object_get_data(G_OBJECT(widget), "menu_item_radio_data");
	bar_pane_gps_set_map_source(pgd, mapsource);
}

static void bar_pane_gps_notify_selection(GtkWidget *bar, gint count)
{
	PaneGPSData *pgd;

	if (count == 0) return;

	pgd = g_object_get_data(G_OBJECT(bar), "pane_data");
	if (!pgd) return;

	bar_pane_gps_update(pgd);
}

static void bar_pane_gps_set_fd(GtkWidget *bar, FileData *fd)
{
	PaneGPSData *pgd;

	pgd = g_object_get_data(G_OBJECT(bar), "pane_data");
	if (!pgd) return;

	file_data_unref(pgd->fd);
	pgd->fd = file_data_ref(fd);

	bar_pane_gps_update(pgd);
}

static gint bar_pane_gps_event(GtkWidget *bar, GdkEvent *event)
{
	PaneGPSData *pgd;

	pgd = g_object_get_data(G_OBJECT(bar), "pane_data");
	if (!pgd) return FALSE;

	if (GTK_WIDGET_HAS_FOCUS(pgd->widget)) return gtk_widget_event(GTK_WIDGET(pgd->widget), event);

	return FALSE;
}

static void bar_pane_gps_write_config(GtkWidget *pane, GString *outstr, gint indent)
{
	PaneGPSData *pgd;
	gint zoom;
	ChamplainMapSource *mapsource;
	const gchar *map_id;
	gchar *str = NULL;
	GString *buffer = g_string_new(str);
	gdouble position;
	gint int_position;

	pgd = g_object_get_data(G_OBJECT(pane), "pane_data");
	if (!pgd) return;

	WRITE_NL();
	WRITE_STRING("<pane_gps ");
	write_char_option(outstr, indent, "id", pgd->pane.id);
	write_char_option(outstr, indent, "title", gtk_label_get_text(GTK_LABEL(pgd->pane.title)));
	WRITE_BOOL(pgd->pane, expanded);
	WRITE_INT(*pgd, height);
	indent++;

	g_object_get(G_OBJECT(pgd->gps_view), "map-source", &mapsource, NULL);
	map_id = champlain_map_source_get_id(mapsource);
	WRITE_NL();
	write_char_option(outstr, indent, "map-id", map_id);

	g_object_get(G_OBJECT(pgd->gps_view), "zoom-level", &zoom, NULL);
	g_string_printf(buffer, "%d", zoom);
	WRITE_NL();
	write_char_option(outstr, indent, "zoom-level", buffer->str);

	g_object_get(G_OBJECT(pgd->gps_view), "latitude", &position, NULL);
	int_position = position * 1000000;
	g_string_printf(buffer, "%i", int_position);
	WRITE_NL();
	write_char_option(outstr, indent, "latitude", buffer->str);

	g_object_get(G_OBJECT(pgd->gps_view), "longitude", &position, NULL);
	int_position = position * 1000000;
	g_string_printf(buffer, "%i", int_position);
	WRITE_NL();
	write_char_option(outstr, indent, "longitude", buffer->str);

	indent--;
	WRITE_NL();
	WRITE_STRING("/>");

  g_object_unref(mapsource);

}

static void bar_pane_gps_slider_changed_cb(GtkScaleButton *slider,
					   gdouble zoom,
					   gpointer data)
{
	PaneGPSData *pgd = data;
	GString *message;

	message = g_string_new("");
	g_string_printf(message, _("Zoom %i"), (gint)zoom);

	g_object_set(G_OBJECT(CHAMPLAIN_VIEW(pgd->gps_view)), "zoom-level", (gint)zoom, NULL);
	gtk_widget_set_tooltip_text(GTK_WIDGET(slider), message->str);
	g_string_free(message, TRUE);

}
static void bar_pane_gps_view_state_changed_cb(ChamplainView *view,
           				       GParamSpec *gobject,
           				       gpointer data)
{
	PaneGPSData *pgd = data;
 	ChamplainState status;
 	gint zoom;
	GString *message;

	g_object_get(G_OBJECT(view), "zoom-level", &zoom, NULL);
	message = g_string_new("");
	g_string_printf(message, _("Zoom level %i"), zoom);

	g_object_get(G_OBJECT(view), "state", &status, NULL);
	if (status == CHAMPLAIN_STATE_LOADING)
		{
		gtk_label_set_text(GTK_LABEL(pgd->state), _("Loading map"));
		}
	else
		{
		gtk_label_set_text(GTK_LABEL(pgd->state), message->str);
		}
		
	gtk_widget_set_tooltip_text(GTK_WIDGET(pgd->slider), message->str);
	gtk_scale_button_set_value(GTK_SCALE_BUTTON(pgd->slider), (gdouble)zoom);

	g_string_free(message, TRUE);
}

static void bar_pane_gps_notify_cb(FileData *fd, NotifyType type, gpointer data)
{
	PaneGPSData *pgd = data;
	
	if ((type & (NOTIFY_REREAD | NOTIFY_CHANGE | NOTIFY_METADATA)) && fd == pgd->fd) 
		{
		bar_pane_gps_update(pgd);
		}
}

const gchar *bar_pane_gps_get_map_id(PaneGPSData *pgd)
{
	const gchar *map_id;
	ChamplainMapSource *mapsource;

	g_object_get(G_OBJECT(pgd->gps_view), "map-source", &mapsource, NULL);
	map_id = champlain_map_source_get_id(mapsource);

	g_object_unref(mapsource);

	return map_id;
}

static GtkWidget *bar_pane_gps_menu(PaneGPSData *pgd)
{
	GtkWidget *menu;
	GtkWidget *map_centre;
	GtkWidget *parent;
	ChamplainMapSourceFactory *map_factory;
	GSList *map_list;
	ChamplainMapSourceDesc *map_desc;
	const gchar *current;

	menu = popup_menu_short_lived();

	map_factory = champlain_map_source_factory_dup_default();
	map_list = champlain_map_source_factory_dup_list(map_factory);
	current = bar_pane_gps_get_map_id(pgd);

	while (map_list)
		{
		map_desc = (ChamplainMapSourceDesc *)(map_list->data);
		
		menu_item_add_radio(menu, map_desc->name, map_desc->id, strcmp(map_desc->id, current) == 0, G_CALLBACK(bar_pane_gps_change_map_cb), pgd); 
		
		map_list = g_slist_next(map_list);
		}
		
	menu_item_add_divider(menu);
	menu_item_add_check(menu, _("Enable markers"), pgd->enable_markers_checked,
											G_CALLBACK(bar_pane_gps_enable_markers_checked_toggle_cb), pgd);
	map_centre = menu_item_add_check(menu, _("Centre map on marker"), pgd->centre_map_checked,
											G_CALLBACK(bar_pane_gps_centre_map_checked_toggle_cb), pgd);
	if (!pgd->enable_markers_checked)
		{
		gtk_widget_set_sensitive(map_centre, FALSE);
		}

	g_slist_free(map_list);
	g_object_unref(map_factory);
	//g_object_unref(map_centre);

	return menu;
}

/* Determine if the map is to be re-centred on the marker when another photo is selected
 */
void bar_pane_gps_map_centreing(PaneGPSData *pgd)
{
	GtkWidget *dialog;
	GString *message = g_string_new("");

	if (pgd->centre_map_checked)
		{
		message = g_string_append(message, _("Move map centre to marker\n is disabled"));
		pgd->centre_map_checked = FALSE;
		}
	else
		{
		message = g_string_append(message, _("Move map centre to marker\n is enabled"));
		pgd->centre_map_checked = TRUE;
		}
		
	dialog = gtk_message_dialog_new(NULL,
							  GTK_DIALOG_DESTROY_WITH_PARENT,
							  GTK_MESSAGE_INFO,
							  GTK_BUTTONS_CLOSE,
							  "%s", message->str);
	gtk_window_set_title(GTK_WINDOW(dialog), _("Map Centreing"));
	gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_MOUSE);
	gtk_dialog_run(GTK_DIALOG(dialog));
	
	gtk_widget_destroy(dialog);
	g_string_free(message, TRUE);
}

static gboolean bar_pane_gps_map_keypress_cb(GtkWidget *widget, GdkEventButton *bevent, gpointer data)
{
	PaneGPSData *pgd = data;
	GtkWidget *menu;

	if (bevent->button == MOUSE_BUTTON_RIGHT)
		{
		menu = bar_pane_gps_menu(pgd);
		gtk_menu_popup(GTK_MENU(menu), NULL, NULL, NULL, NULL, bevent->button, bevent->time);
		return TRUE;
		}
	else if (bevent->button == MOUSE_BUTTON_MIDDLE)
		{
		bar_pane_gps_map_centreing(pgd);
		return TRUE;
		}
	else if (bevent->button == MOUSE_BUTTON_LEFT)
		{
		return FALSE;
		}
	else
		{
		return FALSE;
		}
}

static void bar_pane_gps_destroy(GtkWidget *widget, gpointer data)
{
	PaneGPSData *pgd = data;

	file_data_unregister_notify_func(bar_pane_gps_notify_cb, pgd);

	file_data_unref(pgd->fd);
	g_free(pgd->map_source);
	g_free(pgd->pane.id);
	clutter_actor_destroy(pgd->gps_view);
	g_free(pgd);
}


GtkWidget *bar_pane_gps_new(const gchar *id, const gchar *title, const gchar *map_id,
         					const gint zoom, const gdouble latitude, const gdouble longitude,
            				gboolean expanded, gint height)
{
	PaneGPSData *pgd;
	GtkWidget *vbox, *frame;
	GtkWidget *gpswidget, *viewport;
	GtkWidget *status, *state, *progress, *slider;
	ChamplainLayer *layer;
	ChamplainView *view;
	const gchar *slider_list[] = {GTK_STOCK_ZOOM_IN, GTK_STOCK_ZOOM_OUT, NULL};
	const gchar **slider_icons = slider_list;

	pgd = g_new0(PaneGPSData, 1);

	pgd->pane.pane_set_fd = bar_pane_gps_set_fd;
	pgd->pane.pane_notify_selection = bar_pane_gps_notify_selection;
	pgd->pane.pane_event = bar_pane_gps_event;
	pgd->pane.pane_write_config = bar_pane_gps_write_config;
	pgd->pane.title = bar_pane_expander_title(title);
	pgd->pane.id = g_strdup(id);
	pgd->pane.type = PANE_GPS;
	pgd->pane.expanded = expanded;
	pgd->height = height;

	frame = gtk_frame_new(NULL);
	vbox = gtk_vbox_new(FALSE, 0);

	gpswidget = gtk_champlain_embed_new();
	view = gtk_champlain_embed_get_view(GTK_CHAMPLAIN_EMBED(gpswidget));

	gtk_box_pack_start(GTK_BOX(vbox), gpswidget, TRUE, TRUE, 0);
	gtk_container_add(GTK_CONTAINER(frame), vbox);

	status = gtk_hbox_new(FALSE,0);
	slider = gtk_scale_button_new(GTK_ICON_SIZE_SMALL_TOOLBAR, 1, 17, 1, slider_icons);
	gtk_widget_set_tooltip_text(slider, "Zoom");
	gtk_scale_button_set_value(GTK_SCALE_BUTTON(slider), (gdouble)zoom);

	progress = gtk_progress_bar_new();
	state = gtk_label_new("");
	gtk_label_set_justify(GTK_LABEL(state), GTK_JUSTIFY_CENTER);
	
	gtk_box_pack_start(GTK_BOX(status), GTK_WIDGET(slider), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(status), GTK_WIDGET(state), FALSE, FALSE, 5);
	gtk_box_pack_end(GTK_BOX(status), GTK_WIDGET(progress), FALSE, FALSE, 0);
	gtk_box_pack_end(GTK_BOX(vbox),GTK_WIDGET(status), FALSE, FALSE, 0);
	
	layer = champlain_layer_new();
	champlain_view_add_layer(view, layer);

	pgd->icon_layer = layer;
	pgd->gps_view = CLUTTER_ACTOR(view);
	pgd->widget = frame;
	pgd->progress = progress;
	pgd->slider = slider;
	pgd->state = state;

	bar_pane_gps_set_map_source(pgd, map_id);
	
	g_object_set(G_OBJECT(view), "scroll-mode", CHAMPLAIN_SCROLL_MODE_KINETIC,
				     "zoom-level", zoom,
				     "keep-center-on-resize", TRUE,
/* This seems to be broken, https://bugzilla.gnome.org/show_bug.cgi?id=596419
				     "decel-rate", 1.0,
*/
				     "show-license", TRUE,
				     "zoom-on-double-click", FALSE,
				     "max-zoom-level", 17,
				     "min-zoom-level", 1,
				     NULL);
	champlain_view_center_on(view, latitude, longitude);
	pgd->centre_map_checked = TRUE;
	g_object_set_data(G_OBJECT(pgd->widget), "pane_data", pgd);
	g_signal_connect(G_OBJECT(pgd->widget), "destroy", G_CALLBACK(bar_pane_gps_destroy), pgd);

	gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_IN);

	gtk_widget_set_size_request(pgd->widget, -1, height);

	clutter_set_motion_events_enabled(TRUE);
	g_signal_connect(G_OBJECT(gpswidget), "button_press_event", G_CALLBACK(bar_pane_gps_map_keypress_cb), pgd);
	g_signal_connect(pgd->gps_view, "notify::state", G_CALLBACK(bar_pane_gps_view_state_changed_cb), pgd);
	g_signal_connect(pgd->gps_view, "notify::zoom-level", G_CALLBACK(bar_pane_gps_view_state_changed_cb), pgd);
	g_signal_connect(G_OBJECT(slider), "value-changed", G_CALLBACK(bar_pane_gps_slider_changed_cb), pgd);

	file_data_register_notify_func(bar_pane_gps_notify_cb, pgd, NOTIFY_PRIORITY_LOW);

	pgd->create_markers_id = 0;
	pgd->enable_markers_checked = TRUE;
	pgd->centre_map_checked = TRUE;
	
	return pgd->widget;
}

GtkWidget *bar_pane_gps_new_from_config(const gchar **attribute_names, const gchar **attribute_values)
{
	gchar *title = g_strdup(_("GPS Map"));
	gchar *map_id = NULL;
	gboolean expanded = TRUE;
	gint height = 350;
	gint zoom = 7;
	gdouble latitude;
	gdouble longitude;
	/* Latitude and longitude are stored in the config file as an integer of
	 * (actual value * 1,000,000). There is no READ_DOUBLE utilty function.
	 */
	gint int_latitude = 54000000;
	gint int_longitude = -4000000;
	gchar *id = g_strdup("gps");
	GtkWidget *ret;

	while (*attribute_names)
		{
		const gchar *option = *attribute_names++;
		const gchar *value = *attribute_values++;

		if (READ_CHAR_FULL("title", title))
			continue;
		if (READ_CHAR_FULL("map-id", map_id))
			continue;
		/* There is a bug in the libchamplain libraries which prevents correct
		 * initialisation if the zoom level starts higher than 8
		 */
		if (READ_INT_CLAMP_FULL("zoom-level", zoom, 1, 8))
			continue;
		if (READ_INT_CLAMP_FULL("latitude", int_latitude, -90000000, +90000000))
			continue;
		if (READ_INT_CLAMP_FULL("longitude", int_longitude, -90000000, +90000000))
			continue;
		if (READ_BOOL_FULL("expanded", expanded))
			continue;
		if (READ_INT_FULL("height", height))
			continue;
		if (READ_CHAR_FULL("id", id))
			continue;

		log_printf("unknown attribute %s = %s\n", option, value);
		}

	bar_pane_translate_title(PANE_COMMENT, id, &title);
	latitude = int_latitude / 1000000;
	longitude = int_longitude / 1000000;
	ret = bar_pane_gps_new(id, title, map_id, zoom, latitude, longitude, expanded, height);
	g_free(title);
	g_free(map_id);
	g_free(id);
	return ret;
}

void bar_pane_gps_update_from_config(GtkWidget *pane, const gchar **attribute_names,
                                						const gchar **attribute_values)
{
	PaneGPSData *pgd;
	gint zoom;
	gint int_longitude, int_latitude;
	gdouble longitude, latitude;

	pgd = g_object_get_data(G_OBJECT(pane), "pane_data");
	if (!pgd)
		return;

	gchar *title = NULL;

	while (*attribute_names)
	{
		const gchar *option = *attribute_names++;
		const gchar *value = *attribute_values++;

		if (READ_CHAR_FULL("title", title))
			continue;
		if (READ_CHAR_FULL("map-id", pgd->map_source))
			continue;
		if (READ_BOOL_FULL("expanded", pgd->pane.expanded))
			continue;
		if (READ_INT_FULL("height", pgd->height))
			continue;
		if (READ_CHAR_FULL("id", pgd->pane.id))
			continue;
		if (READ_INT_CLAMP_FULL("zoom-level", zoom, 1, 8))
			{
			g_object_set(G_OBJECT(CHAMPLAIN_VIEW(pgd->gps_view)), "zoom-level", zoom, NULL);
			continue;
			}
		if (READ_INT_CLAMP_FULL("longitude", int_longitude, -90000000, +90000000))
			{
			longitude = int_longitude / 1000000;
			g_object_set(G_OBJECT(CHAMPLAIN_VIEW(pgd->gps_view)), "longitude", longitude, NULL);
			continue;
			}
		if (READ_INT_CLAMP_FULL("latitude", int_latitude, -90000000, +90000000))
			{
			latitude = int_latitude / 1000000;
			g_object_set(G_OBJECT(CHAMPLAIN_VIEW(pgd->gps_view)), "latitude", latitude, NULL);
			continue;
			}
		log_printf("unknown attribute %s = %s\n", option, value);
	}

	if (title)
		{
		bar_pane_translate_title(PANE_COMMENT, pgd->pane.id, &title);
		gtk_label_set_text(GTK_LABEL(pgd->pane.title), title);
		g_free(title);
		}

	gtk_widget_set_size_request(pgd->widget, -1, pgd->height);
	bar_update_expander(pane);
}

#endif
#endif

/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
