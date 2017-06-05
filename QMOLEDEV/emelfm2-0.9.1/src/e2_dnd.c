/* $Id: e2_dnd.c 3001 2014-01-17 23:05:07Z tpgww $

Copyright (C) 2006-2014 tooar <tooar@emelfm2.net>.

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
@file src/e2_dnd.c
@brief drag and drop functions

This file contains functions related to selection and drag'ndrop.
Relevant button-events are handled in e2_fileview.c
*/

#include "e2_dnd.h"
#include <string.h>
#include "e2_fileview.h"
#include "e2_filelist.h"
#include "e2_task.h"
#include "e2_icons.h"

//order of table entries matters !
GtkTargetEntry target_table[] = {
	{ "text/uri-list", 0, TARGET_URI },
	{ "text/plain",    0, TARGET_STRING },
//	{ "XdndDirectSave0", 0, TARGET_XDS } //this last target is supported for drops only
};
guint n_targets = sizeof(target_table) / sizeof(GtkTargetEntry);

//mouse pointer height, used for getting the path at the _top_ of the pointer
static gint pointer_height;
//default pointer height, used when we can't find the runtime value
//NOTE: setting the height too small eg 16 causes problems for
//un-lighting a row when the mouse moves slowly
#define POINTER_HEIGHT 26

extern gboolean btn2_released;

#ifdef E2_ALTLEFTMOUSE
/*this provides a quick check whether the left btn has been pressed
set and cleared in button-press and -release callbacks, respectively*/
extern gboolean left_pressed;
/*flag for whether selection-by-dragging is underway
set TRUE in drag-begin cb, cleared in drag-data-get cb*/
extern gboolean drag_sel_now;
#endif

#ifdef E2_ALTLEFTMOUSE
static GdkModifierType mask;
#endif
//for DnD autoscrolling
#include <math.h>
//c.f POINTER_HEIGHT
#define AUTO_SCROLL_MARGIN 20
//delay before autoscrolling starts is approximately the sum of
//AUTO_SCROLL_INITIAL_DELAY plus AUTO_SCROLL_STEP_INTERVAL
//c.f the default menu-popdown-delay (400 msec)
#define AUTO_SCROLL_INITIAL_DELAY 360
//msec between autoscroll steps 25 Hz seems ok to give reasonably smooth effect
#define AUTO_SCROLL_STEP_INTERVAL 40
//only ever 1 scroll at a time, so this can be static (for dynamic, add to ViewInfo)
//guint auto_scroll_timer_id = 0;

/**
@brief callback for "drag-begin" signal emitted on drag source when drag is started

For normal drag, sets dnd pointer height & multiple-item drag icon
For drag-selection, just sets item-counter=1

@param treeview widget where drag started unused
@param context dragcontext
@param view rt data for view where drag started unused

@return
*/
void e2_dnd_drag_begin_cb (GtkWidget *treeview, GdkDragContext *context,
	ViewInfo *view)
{
	printd (DEBUG, "drag begin");
#ifndef USE_GTK3_10
	GtkSettings *s = gtk_settings_get_default ();
#endif
	gint pointer_width;
#ifdef USE_GTK3_10
	if (!gtk_icon_size_lookup (GTK_ICON_SIZE_DND, &pointer_width, &pointer_height))
#else
	if (!gtk_icon_size_lookup_for_settings (s, GTK_ICON_SIZE_DND, //? is too big !
		&pointer_width, &pointer_height))
#endif
			pointer_height = POINTER_HEIGHT;	//can't find useful size, use the default
	pointer_height--;	//effectively make the 'y hotspot' 1-down from cursor top

	//remember where we began, for later drop-check
//	dragged_sel = view->selection;
#ifdef E2_ALTLEFTMOUSE
	mask =
# ifdef USE_GTK3_0
		e2_utils_get_savedstate (app.main_window)
# else
		e2_utils_get_modifiers ()
# endif
		& gtk_accelerator_get_default_mod_mask ();

	NEEDCLOSEBGL
	if (left_pressed && (mask & GDK_CONTROL_MASK) && !(mask & GDK_SHIFT_MASK))
	{
		drag_sel_now = TRUE;	//signal that a drag-sel has started
		//custom (blank) icon for drag-sel, if the relevant mod keys are ok
		gchar *iconsdir = e2_icons_get_custom_path (TRUE);
		gchar *localpath = e2_utils_strcat (iconsdir, "dragsel.png");

		GdkPixbuf *pxb = e2_icons_get_puxbuf (localpath, GTK_ICON_SIZE_LARGE_TOOLBAR, FALSE);
		if (pxb != NULL)
			gtk_drag_set_icon_pixbuf (context, pxb, 10, 0);	//x offset is ineffective ...

		g_free (iconsdir);
		g_free (localpath);
	}
	else
	{
#endif
		//setup dnd cursor icon
		guint count = gtk_tree_selection_count_selected_rows (view->selection);
		if (count > 1)
#ifdef USE_GTK3_10
			gtk_drag_set_icon_name (context, STOCK_NAME_DND_MULTIPLE, 10, 0);
#else
			gtk_drag_set_icon_stock (context, STOCK_NAME_DND_MULTIPLE, 10, 0);	//y was 1, x makes no diff.
#endif
//		else //no point in checking for > 0  as default will be used, anyway
//			gtk_drag_set_icon_stock (dc, STOCK_NAME_DND, 10, 0);	//y was 1
#ifdef E2_ALTLEFTMOUSE
	}
#endif
	NEEDOPENBGL
}
/**
@brief 'chained' timer callback function which scrolls the filelist treeview for @a view

The initial timer was initiated in the drag-motion callback.

@param view rt data for the view where the drag-motion occurred

@return TRUE to continue timer, FALSE to cancel it
*/
static gboolean _e2_dnd_auto_scroll2 (ViewInfo *view)
{
	static gdouble prior_scroll = 0.0;	//for matching and timer cancellation
	//autoscroll "drivers" -AUTO_SCROLL_MARGIN .. -1 for backward scroll, 1 .. AUTO_SCROLL_MARGIN for forward
	gint y_scr; //,x_scr;
	//must check mouse position here, as e2_dnd_drag_motion_cb() may not be called
	//often enough when auto-scrolling a large treeview
	//is for bottom-left of pointer FIXME make it work for top-left
	gint my;
#ifdef USE_GTK3_0 //CHECKME pointer position
	GdkDevice *device = (GdkDevice *)g_object_get_data (G_OBJECT (view->treeview),
		"e2-drag-device");
	if (device != NULL)
	{
		GdkWindow *win = gtk_widget_get_window (view->treeview);
		//this func can return NULL, but that's irrelevant, contrary to implication of API doc
		gdk_window_get_device_position (win, device, &my, NULL, NULL);
	}
	else
		return FALSE;
#else //gtk 2
	if (gdk_window_get_pointer (
# ifdef USE_GTK2_14
			gtk_widget_get_window (view->treeview),
# else
			view->treeview->window,
# endif
			NULL, &my, NULL) == NULL)
		return FALSE;
#endif

#ifdef USE_GTK2_18
	GtkAllocation alloc;
	gtk_widget_get_allocation (view->treeview, &alloc);
#endif
	if (my >= AUTO_SCROLL_MARGIN && my <= (
#ifdef USE_GTK2_18
		alloc.height
#else
		view->treeview->allocation.height
#endif
		- AUTO_SCROLL_MARGIN))
	{
		//mouse out of scroll zones now
		app.timers[ASCROLL_T] = 0;
		return FALSE;
	}
	else if (my < AUTO_SCROLL_MARGIN)
		y_scr = my - AUTO_SCROLL_MARGIN;	//-AUTO_SCROLL_MARGIN to -1
	else
		y_scr = my + AUTO_SCROLL_MARGIN -
#ifdef USE_GTK2_18
		alloc.height;
#else
		view->treeview->allocation.height; //1 to AUTO_SCROLL_MARGIN
#endif
	GtkAdjustment *vadj;
#ifdef USE_GTK3_0
	vadj = gtk_scrollable_get_vadjustment (GTK_SCROLLABLE (view->treeview));
#else
	vadj = gtk_tree_view_get_vadjustment (GTK_TREE_VIEW (view->treeview));
#endif
	gdouble scrollnow = gtk_adjustment_get_value (vadj);
	gdouble uppernow =
#ifdef USE_GTK2_14
		gtk_adjustment_get_upper (vadj);
#else
		vadj->upper;
#endif
	gdouble sizenow =
#ifdef USE_GTK2_14
		gtk_adjustment_get_page_size (vadj);
#else
		vadj->page_size;
#endif
	gdouble scroll_factor;
	//accelerate the scroll by increasing the stepsize when we are further into
	//the scroll margin. TrialnError fudge factor
	scroll_factor = uppernow / 500.0
		* (exp (ABS (y_scr) * 2.0 / (gdouble) AUTO_SCROLL_MARGIN) - 0.8);
	if (y_scr < 0)
		scrollnow -= scroll_factor;
	else
		scrollnow += scroll_factor;
	//clamp the new setting within relevant range
	if (scrollnow < 0)
		scrollnow = 0;
	else if (scrollnow > uppernow - sizenow)
		scrollnow = uppernow - sizenow - 0.1;
	//no movement, turn off the scrolling CHECKME more iterations before turnoff ?
	if (ABS (scrollnow - prior_scroll) < 0.1)	//0.001*vadj->upper)
	{
		app.timers[ASCROLL_T] = 0;
		return FALSE;
	}

	if (scrollnow >= 0)
	{
		CLOSEBGL
		gtk_adjustment_set_value (vadj, scrollnow);
		OPENBGL
		prior_scroll = scrollnow;
	}
	return TRUE;
}
/**
@brief first (longer-interval) timer callback for scrolling a filelist treeview

Substitutes a shorter interval for the initial one which got to here, initiated
in the drag-motion callback

@param view rt data for the view where the drag-motion occurred

@return FALSE always, so timer is cancelled
*/
static gboolean _e2_dnd_auto_scroll (ViewInfo *view)
{
	app.timers[ASCROLL_T] = g_timeout_add (AUTO_SCROLL_STEP_INTERVAL,
		(GSourceFunc) _e2_dnd_auto_scroll2, view);
	return FALSE;
}
/**
@brief "drag-motion" signal callback

This is called when the user moves the cursor during a drag.
For a normal drag it handles the highlighting of drop-target rows, and manages
treeview scrolling when cursor is near top or bottom of treeview.
For a drag-selection it selects item if needed and bumps counter.

@param treeview widget ptr
@param context drag context
@param x the left coordinate of the current cursor position
@param y the BOTTOM coordinate of the current cursor position
@param time the timestamp of the motion event
@param view rt data for the view where the drag [started?]

@return  TRUE unless drag-sel is happening (i.e. everywhere is considered a drop-zone)
*/
gboolean e2_dnd_drag_motion_cb (GtkWidget *treeview, GdkDragContext *context,
	gint x, gint y, guint time, ViewInfo *view)
{
//	printd (DEBUG, "callback: drag motion");
	static ViewInfo *last_view;
	GtkTreeIter iter;
	GtkTreePath *path;
	GtkTreeModelFilter *fmodel = GTK_TREE_MODEL_FILTER (view->model);
//same as fmodel - gtk_tree_view_get_model (GTK_TREE_VIEW (treeview));
//	GtkTreeModel *model = gtk_tree_model_filter_get_model (fmodel);
//same as model - GTK_TREE_MODEL (view->store);
	GtkTreeModel *model = GTK_TREE_MODEL (view->store);
	
	NEEDCLOSEBGL

	//get the path of the item under the cursor TOP ie y-pointer_height
	if (gtk_tree_view_get_path_at_pos (GTK_TREE_VIEW (treeview),
		x, y-pointer_height, &path, NULL, NULL, NULL))
	{
#ifdef E2_ALTLEFTMOUSE
		if (drag_sel_now	//a shift-left drag was started
//			&& (mask & GDK_CONTROL_MASK) && !(mask & GDK_SHIFT_MASK))//mod keys still correct
		)
		{
			GtkTreeSelection *sel = view->selection;
			if (!gtk_tree_selection_path_is_selected (sel, path)	//over an un-selected item
				&& (treeview == gtk_drag_get_source_widget (context))) //in the original treeview
			{
				//select it
				gtk_tree_selection_select_path (sel, path);
				gtk_tree_path_free (path);
				NEEDOPENBGL
				return FALSE;
			}
		}
#endif

		GtkTreePath *childpath = gtk_tree_model_filter_convert_path_to_child_path
			(fmodel, path);
		//list store path depth is 1, so row no. is 1st index
		gint row = *gtk_tree_path_get_indices (childpath);
		if (view == last_view)
		{
			if (row != view->drop_row //mouse has moved to another row
				&& view->lit
//				&& view->drop_row != -1
				&& gtk_tree_model_iter_nth_child (model, &iter, NULL, view->drop_row))
			{
//				printd (DEBUG, "restored color of row %d in same pane", view->drop_row);
				e2_fileview_clear_row_background (view, &iter);
			}
		}
		else
		{	//1st use, or changed pane
			// the drag-leave callback handles color-restoration  & drop_row = -1
			last_view = view;
		}

		if (row != view->drop_row
			&& gtk_tree_model_get_iter (model, &iter, childpath))
		{
			view->drop_row = row;

			FileInfo *info;
			gtk_tree_model_get (model, &iter, FINFO, &info, -1);
			//only highlight dirs (either pane)
			//FIXME also highlight executable items
			gchar *infopath = F_FILENAME_TO_LOCALE (view->dir);
#ifdef E2_VFS
			VPATH sdata = { infopath, view->spacedata };
			gboolean dir = e2_fs_is_dir (&sdata, info);
#else
			gboolean dir = e2_fs_is_dir (infopath, info);
#endif
			F_FREE (infopath, view->dir);
			if (dir)
			{
//				printd (DEBUG, "CHANGED color of row %d in same pane", row);
				e2_fileview_set_row_background (view, &iter,
					e2_option_color_get ("color-highlight"));
			}
		}

		gtk_tree_path_free (path);
		gtk_tree_path_free (childpath);
	}
	else //there is no row under the mouse-top
	{
//		printd (DEBUG, "No row under mouse now");
		if (view == last_view)	//still in same pane
		{
			if (view->lit
//				&& view->drop_row != -1
				&& gtk_tree_model_iter_nth_child (model, &iter, NULL, view->drop_row))
			{
//				printd (DEBUG, "restored color of row %d in same pane", view->drop_row);
				//restore the color of the last row that was lit
				e2_fileview_clear_row_background (view, &iter);
				view->drop_row = -1;
			}
		}
		else	//now in the other file-pane
		{
			// the drag-leave callback andles color restoration
			last_view = view;
		}
	}
	//arrange auto-scrolling when mouse pointer is near top or bottom of treeview
#ifdef USE_GTK2_18
	GtkAllocation alloc;
	gtk_widget_get_allocation (view->treeview, &alloc);
#endif

	NEEDOPENBGL

	if (y < (pointer_height + AUTO_SCROLL_MARGIN)
	 || y > (
#ifdef USE_GTK2_18
			 alloc.height
#else
	         view->treeview->allocation.height
#endif
			 - AUTO_SCROLL_MARGIN))
	{
#ifdef USE_GTK3_0 //CHECKME pointer position
		//hacky way to send extra data to timer callback
		g_object_set_data (G_OBJECT (view->treeview), "e2-drag-device",
			gdk_drag_context_get_device (context));
#endif
		if (app.timers[ASCROLL_T] == 0)
		{
			//setup initial delay
			app.timers[ASCROLL_T] = g_timeout_add (AUTO_SCROLL_INITIAL_DELAY,
				(GSourceFunc) _e2_dnd_auto_scroll, view);
		}
	}
	else if (app.timers[ASCROLL_T] > 0)
	{
		g_source_remove (app.timers[ASCROLL_T]);
		app.timers[ASCROLL_T] = 0;
	}

	return TRUE;
}
/**
@brief revert a drop target directory to default background

This is the callback for "drag-leave" signals, and is called when changing
file panes, and for exits from the app window

@param treeview widget which was departed
@param context  dragcontext unused
@param time unused
@param view rt data for the view where the drop ocurred

@return  TRUE if the conversion completed successfully, else FALSE
*/
gboolean e2_dnd_drag_leave_cb (GtkWidget *treeview, GdkDragContext *context,
	guint time, ViewInfo *view)
{
//	printd (DEBUG, "callback: drag leave");
	//turn off any autoscrolling due to departure
	if (app.timers[ASCROLL_T] > 0)
	{
		g_source_remove (app.timers[ASCROLL_T]);
		app.timers[ASCROLL_T] = 0;
	}
	gboolean retval;
	GtkTreeIter iter;
	//always use the underlying model, here
	if (!view->lit
		|| view->drop_row == -1
		|| ! gtk_tree_model_iter_nth_child (GTK_TREE_MODEL (view->store), &iter,
				NULL, view->drop_row))
	{
		retval = FALSE;
	}
	else
	{
		NEEDCLOSEBGL
		e2_fileview_clear_row_background (view, &iter);
		NEEDOPENBGL
		retval = TRUE;
	}
	view->drop_row = -1;	//make sure any re-entry will trigger a highlight
	return retval;
}
/* *
@brief UNUSED "drag-data-delete" signal callback

This is called when a drag of type GDK_ACTION_MOVE
was successfully completed.

@param treeview widget where drag started unused
@param dc dragcontext
@param view rt data for view where drag started

@return
*/
/*void e2_dnd_drag_delete_cb (GtkWidget *treeview, GdkDragContext *context,
	ViewInfo *view)
{
	printd (DEBUG, "drag data delete");
	NEEDCLOSEBGL
	e2_task_delete (NULL, NULL, NULL);	//delete active-pane selected items
	NEEDOPENBGL
	return;
} */
/**
@brief callback for "drag-data-get" signal

This is the emitted on the drag source when the drop site requests the data
which has been dragged.
For a drag-selection it just cleans up some related flags.
For a normal drag it builds a URI list of the selected filenames to set as
the drag data.
Any directory item will not have a trailing separator, as the names are
freshly converted from the relevant FileInfo field

@param treeview UNUSED the treeview which received the signal
@param context UNUSED the drag context
@param data the GtkSelectionData to be filled with the dragged data
@param info_arg the info that has been registered with the target in the GtkTargetList.
@param time the timestamp at which the data was requested
@param view data structure for the view

@return
*/
void e2_dnd_drag_data_get_cb (GtkWidget *treeview, GdkDragContext *context,
		GtkSelectionData *data, guint info_arg, guint time, ViewInfo *view)
{
	printd (DEBUG, "callback: drag data get");
#ifdef E2_ALTLEFTMOUSE
	if (drag_sel_now)
	{
		drag_sel_now = FALSE;
		return;
	}
#endif

#ifdef E2_REFRESH_DEBUG
	printd (DEBUG, "disable refresh, drag data get");
#endif
	e2_filelist_disable_refresh ();

	/* this is the emerging standard for uri list entries ?
	<?xml version="1.0" encoding="UTF-8"?>
   <resource-lists xmlns="urn:ietf:params:xml:ns:resource-lists" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">\r\n
	 <list>\r\n
	   <entry uri="sip:bill@example.com" />\r\n
	   <entry uri="sip:joe@example.org" />\r\n
	   <entry uri="sip:ted@example.net" />\r\n
	 </list>\r\n
   </resource-lists>\r\n
for now, at least, in accord with old w3c standard (which is currently under review)
/foo/bar => file:///foo/bar
OR
/foo/bar => file://host/foo/bar (this is the xdg preferred style)
NOTE some apps omit the leading "//" from the first example, resulting in file:/foo/bar
*/
	GList *base, *tmp;
//	NEEDCLOSEBGL
	base = e2_fileview_get_selected_local (view);
//	NEEDOPENBGL
	if (base != NULL)
	{
		//there is actually a selection to process ...
		gchar *uri, *local_path, *local_dir;
#ifdef E2_VFSTMP
	//FIXME dir when not mounted local
#else
		local_dir = D_FILENAME_TO_LOCALE (view->dir); //always dup, to avoid dirchange race
#endif
		GPtrArray *uri_array = g_ptr_array_sized_new (g_list_length (base) + 1);
		//FIXME vfs
/* some apps don't yet support URI's with a host, we'll use worst-case
#ifdef USE_GLIB2_8
		const gchar *host = g_get_host_name ();
#else
		const gchar *host = "localhost";
#endif
*/
//		GError *error = NULL;
		for (tmp = base; tmp != NULL; tmp = tmp->next)
		{
			local_path = e2_utils_strcat (local_dir,
				((FileInfo *)tmp->data)->filename);
//see above uri = g_filename_to_uri (local_path, host, NULL);	//&error);
			uri = g_filename_to_uri (local_path, NULL, NULL);	//&error);
			g_free (local_path);
			if (uri != NULL)
				g_ptr_array_add (uri_array, uri);
/*			else
			{
				//FIXME warn the user
				g_error_free (error);
			}
*/
		}
		g_ptr_array_add (uri_array, NULL);	//NULL-terminate the array
		gtk_selection_data_set_uris (data, (gchar **)uri_array->pdata);
//#ifndef USE_GLIB2_22
		g_ptr_array_foreach (uri_array, (GFunc)g_free, NULL);
//#endif
		g_ptr_array_free (uri_array, TRUE);
		g_list_free (base);
		g_free (local_dir);
	}
#ifdef E2_REFRESH_DEBUG
	printd (DEBUG, "enable refresh, drag data get");
#endif
	e2_filelist_enable_refresh ();
}
/* *
@brief UNUSED callback for "drag-drop" signal emitted on the drop site when the user drops data there
This is called for only XDS protocol ??
@param treeview widget where drag occured
@param context source? drag context
@param x the x coordinate of the current cursor position
@param y the y coordinate of the current cursor position
@param time the timestamp of the motion event
@param view rt data for view where drag started ?

@return TRUE always (the cursor is always in a drop zone)
*/
/*THIS DOES NOT WORK
gboolean e2_dnd_drag_drop_cb (GtkWidget *treeview, GdkDragContext *context,
	gint x, gint y, guint time, ViewInfo *view)
{
	printd (DEBUG, "callback: drag drop");
	if (context->protocol == GDK_DRAG_PROTO_XDND)
	{
		gint srcformat, srclen;
		guchar *srcitem;
		gchar *dest_dir, *destpath = NULL;

		NEEDCLOSEBGL
		if (gdk_property_get (context->source_window,
			atom_XdndDirectSave0, atom_text_plain,
			0, PATH_MAX, FALSE, NULL, &srcformat, &srclen, &srcitem))
		{
			if (srcitem != NULL)
			{
				//terminate the path|name string
				srcitem [srclen] = '\0';
				//CHECKME convert to utf8 ?
				//FIXME get host, destpath)

#ifdef E2_VFSTMP
				//FIXME dir when not mounted local
#else
				dest_dir = F_FILENAME_TO_LOCALE (view->dir); //localised string with trailing /
#endif
				GtkTreePath *path;
				if (gtk_tree_view_get_path_at_pos (GTK_TREE_VIEW (treeview),
					x, y-pointer_height, &path, NULL, NULL, NULL))
				{
					GtkTreeIter iter;
					GtkTreeModelFilter *fmodel = GTK_TREE_MODEL_FILTER (view->model);
					//same as fmodel - gtk_tree_view_get_model (GTK_TREE_VIEW (treeview));
					GtkTreeModel *model = gtk_tree_model_filter_get_model (fmodel);
					//same as model - GTK_TREE_MODEL (view->store);
					GtkTreePath *childpath = gtk_tree_model_filter_convert_path_to_child_path
						(fmodel, path);
					if (gtk_tree_model_get_iter (model, &iter, childpath))
					{
						FileInfo *info;
						gtk_tree_model_get (model, &iter, FINFO, &info, -1);
						//NICE to add check for executable, here, and do a different drop menu
						//check if dropping onto a dir
						gchar *infopath = F_FILENAME_TO_LOCALE (view->dir);
#ifdef E2_VFS
						VPATH sdata = { infopath, view->spacedata };
						gboolean dir = e2_fs_is_dir (&sdata, info);
#else
						gboolean dir = e2_fs_is_dir (infopath, info);
#endif
						F_FREE (infopath, view->dir);
						if (dir)
						{
							//prevent dropping into a dir that's part of the selection
							if ((treeview == gtk_drag_get_source_widget (context))	//we're in the same selection as we started with
								&& gtk_tree_selection_path_is_selected (view->selection, path))
							{
								NEEDOPENBGL
								gtk_tree_path_free (path);
								gtk_tree_path_free (childpath);
								gtk_drag_finish (context, FALSE, FALSE, time);
								return FALSE;
							}
							//append target dir's name to the pane dir path
							//view->dir, filename both utf, both have trailing /
#ifdef E2_VFSTMP
				//FIXME dir when not mounted local
#else
							destpath = g_strconcat (dest_dir, info->filename, G_DIR_SEPARATOR_S, srcitem, NULL);
#endif
						}
					}
					gtk_tree_path_free (path);
					gtk_tree_path_free (childpath);
				}
				if (destpath == NULL)
					destpath = g_strconcat (dest_dir, srcitem, NULL);

				//FIXME get correct hostname
				gchar *desturi = g_filename_to_uri (destpath, NULL, NULL);

				gdk_property_change (context->source_window,
					atom_XdndDirectSave0, atom_text_plain, srcformat,
					GDK_PROP_MODE_REPLACE, (guchar *)desturi, strlen (desturi));

				NEEDOPENBGL

				F_FREE (dest_dir);
				g_free (destpath);
				g_free (desturi);

				gtk_drag_get_data (treeview, context, atom_XdndDirectSave0, time);
				return TRUE;
			}
		}
		NEEDOPENBGL
	}
/ *	else
	{
		GList *member;
		for (member = context->targets; member != NULL; member = member->next)
		{
			if ((GdkAtom) member->data == atom_text_uri_list)
				break;
		}
		if (member == NULL)
			goto badexit;
	}
* /
	//failure
	gtk_drag_finish (context, FALSE, FALSE, time);
	return FALSE;
}
*/

typedef struct _E2_MenuWait
{
	GtkWidget *chosen;	//pointer to chosen menu item
	E2_MainLoop *loopdata;
} E2_MenuWait;

/**
@brief get drag op menu item

@param menuitem the activated menu item widget
@param w wait-data for this callback

@return
*/
static void _e2_dnd_drag_op_selected_cb (GtkMenuItem *menuitem, E2_MenuWait *w)
{
	w->chosen = GTK_WIDGET(menuitem);
	NEEDCLOSEBGL
	e2_main_loop_quit (w->loopdata);
	NEEDOPENBGL
}
/**
@brief execute drop

This is the callback for "drag-data-received" signals, emitted on @a treeview
when dragged data has been received. It initiates action for the dropped items.

@param treeview widget for pane where the drop is to occur
@param context the dragcontext
@param x where the drop happened, mouse-cursor-left
@param y where the drop happened, mouse-cursor-BOTTOM
@param data pointer to selected items data struct
@param drag_info the info that has been registered with the target in the GtkTargetList
@param time the timestamp at which the data was received
@param view pointer to ViewInfo data struct for the pane shown in @a treeview

@return
*/
void e2_dnd_drag_data_received_cb (GtkWidget *treeview, GdkDragContext *context,
	gint x, gint y, GtkSelectionData *data, guint drag_info, guint time,
	ViewInfo *view)
{
	printd (DEBUG, "callback: drag data received");
#ifdef E2_ALTLEFTMOUSE
	left_pressed = FALSE;	//flag must be cleared at end of drag
#endif
	gchar *dest_dir;
	//default is to just drop to the pane dir
#ifdef E2_VFSTMP
	//FIXME dir when not mounted local
	dest_dir = view->dir; //utf-8 string with trailing /
#else
	dest_dir = view->dir; //utf-8 string with trailing /
#endif
	gboolean ask;
#ifdef USE_GTK2_22
	GdkDragAction action = gdk_drag_context_get_actions (context);
#else
	GdkDragAction action = context->actions;
#endif
	//get the user's choice on what to do with the dragged items
	ask = (action & GDK_ACTION_ASK);
	if (ask)
	{	//a choice is offered
		if (!btn2_released)
		{
			//need to get fresh mask here, the one set in drag-begin doesn't work
#ifdef USE_GTK3_0
			ask = (e2_utils_get_savedstate (app.main_window) & GDK_MOD1_MASK); //alt key pressed in main window
#else
			NEEDCLOSEBGL
			ask = (e2_utils_get_modifiers () & GDK_MOD1_MASK); //alt key pressed
			NEEDOPENBGL
#endif
		}
	}
	if (ask)
	{
		E2_MenuWait wdata;
		//this loop for popup menu must be in default context and the menu will
		//not survive any UI event
		wdata.loopdata = e2_main_loop_new (TRUE);
		if (wdata.loopdata != NULL)
		{
			GtkWidget *drag_op_menu = e2_menu_get();
			GtkWidget *item1, *item2, *item3, *item4;

			if (action & GDK_ACTION_COPY)
				item1 = e2_menu_add (drag_op_menu, _("_Copy"), NULL, NULL,
					_e2_dnd_drag_op_selected_cb, &wdata);
			else
				item1 = NULL;

			if (action & GDK_ACTION_MOVE)
				item2 = e2_menu_add (drag_op_menu, _("_Move"), NULL, NULL,
					_e2_dnd_drag_op_selected_cb, &wdata);
			else
				item2 = NULL;

			if (action & GDK_ACTION_LINK)
				item3 = e2_menu_add (drag_op_menu, _("_Link"), NULL, NULL,
					_e2_dnd_drag_op_selected_cb, &wdata);
			else
				item3 = NULL;

/*			if (?)	//drop target is an executable item
				item4 = e2_menu_add (drag_op_menu, _("_SendTo"), NULL, NULL,
					_e2_dnd_drag_op_selected_cb, &wdata);
			else
				item4 = NULL;
*/
			item4 = e2_menu_add (drag_op_menu, _("C_ancel"), NULL, NULL,
				_e2_dnd_drag_op_selected_cb, &wdata);
			wdata.chosen = item4; //default in case of abort == cancel

			NEEDCLOSEBGL

			gtk_menu_popup (GTK_MENU (drag_op_menu), NULL, NULL, NULL, NULL, 0,
				(guint32) time);

			e2_main_loop_run (wdata.loopdata);

			NEEDOPENBGL

			e2_menu_selection_done_cb (drag_op_menu, NULL);

			if (wdata.chosen == item1)
				action = GDK_ACTION_COPY;
			else if (wdata.chosen == item2)
				action = GDK_ACTION_MOVE;
			else if (wdata.chosen == item3)
				action = GDK_ACTION_LINK;
/*			else if (wdata.chosen == item4)
				setup to handle the sendto request
*/
			else //the user cancelled
				action = 0;
		}
		else
		{ //can't do anything
			//FIXME warn user
			action = 0;
		}
	}
	else	//don't ask
#ifdef USE_GTK2_22
		action = gdk_drag_context_get_suggested_action (context);
#else
		action = context->suggested_action;
#endif
	//FIXME handle sendto
	E2_TaskType droptask;
	if (action & GDK_ACTION_COPY)
		droptask = E2_TASK_COPY;
	else if (action & GDK_ACTION_MOVE)
		droptask = E2_TASK_MOVE;
	else if (action & GDK_ACTION_LINK)
		droptask = E2_TASK_LINK;
	else
		goto badexit;

	NEEDCLOSEBGL
	//determine whether to drop the selection other than in view->dir
	GtkTreePath *path;
	if (gtk_tree_view_get_path_at_pos (GTK_TREE_VIEW (treeview),
		x, y-pointer_height, &path, NULL, NULL, NULL))
	{
		GtkTreeIter iter;
		GtkTreeModelFilter *fmodel = GTK_TREE_MODEL_FILTER (view->model);
//same as fmodel - gtk_tree_view_get_model (GTK_TREE_VIEW (treeview));
		GtkTreeModel *model = gtk_tree_model_filter_get_model (fmodel);
//same as model - GTK_TREE_MODEL (view->store);
		GtkTreePath *childpath = gtk_tree_model_filter_convert_path_to_child_path
			(fmodel, path);
		if (gtk_tree_model_get_iter (model, &iter, childpath))
		{
			gchar *infopath, *filename;
			FileInfo *info;
			gtk_tree_model_get (model, &iter, FILENAME, &filename, FINFO, &info, -1);
			//NICE to add check for executable, here, and do a different drop menu
			//check if dropping onto a dir
			infopath = F_FILENAME_TO_LOCALE (view->dir);
#ifdef E2_VFS
			VPATH sdata = { infopath, view->spacedata };
			gboolean dir = e2_fs_is_dir (&sdata, info);
#else
			gboolean dir = e2_fs_is_dir (infopath, info);
#endif
			F_FREE (infopath, view->dir);
			if (dir)
			{
				//now that we're finishing, un-light the row
				e2_fileview_clear_row_background (view, &iter);
				//prevent dropping into a dir that's part of the selection
				if ((treeview == gtk_drag_get_source_widget (context))	//we're in the same selection as we started with
					&& gtk_tree_selection_path_is_selected (view->selection, path))
				{
					g_free (filename);
					gtk_tree_path_free (path);
					gtk_tree_path_free (childpath);
					goto badexit;
				}
				//append target dir's name to the pane dir path
				//both view->dir and filename are UTF-8, with trailing /
#ifdef E2_VFSTMP
	//FIXME dir when not mounted local
#else
				dest_dir = e2_utils_dircat (view, filename, FALSE);
#endif
			}
			g_free (filename);
		}
		gtk_tree_path_free (path);
		gtk_tree_path_free (childpath);
	}

	gboolean result, move;
	gchar **uris = gtk_selection_data_get_uris (data);
	if (uris != NULL)
	{
		result = e2_task_drop (droptask, dest_dir, uris); //do it
		move = (result && (droptask == E2_TASK_MOVE));
		g_strfreev (uris);
	}
	else
	{
badexit:
		result = FALSE;
		move = FALSE;
		action = 0;
	}

	NEEDOPENBGL
#ifdef USE_GTK2_22
	gdk_drag_status (context, action, (guint32) time);
#else
	context->action = action;
#endif
	gtk_drag_finish (context, result, move, time);
//CHECKME filtermodel ...
/* Must override the default 'drag-data-received' handler on GtkTreeView when
using models that don't support the GtkTreeDragDest interface and enable
drag-and-drop. The simplest way to do this is to connect to ::drag-data-received
and call g_signal_stop_emission_by_name() in your signal handler to prevent the
default handler from running. Look at the source code for the default handler in
gtktreeview.c to get an idea what your handler should do.
*/
	//CHECKME nothing else needs doing here ?
	g_signal_stop_emission_by_name ((gpointer)view->treeview,
		"drag-data-received");

#ifdef E2_VFSTMP
	//FIXME dir when not mounted local
#else
	if (dest_dir != view->dir)
#endif
		g_free (dest_dir);
}
